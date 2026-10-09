# =============================================================================
# 02 — Modélisation
# Six modèles, réglés puis évalués par validation croisée à 5 plis sur
# l'échantillon d'apprentissage (mêmes plis pour tous). Le jeu de test n'est
# utilisé qu'une fois, à la fin, par les modèles définitifs.
# Sorties : donnees/derivees/{predictions,modeles_finaux,reglages}.rds
# =============================================================================
suppressPackageStartupMessages({ library(naivebayes); library(glmnet); library(xgboost); library(scorecard) })
source("app/preparation.R")
source("R/fonctions.R")
source("app/modeles_application.R")   # probabilite_depuis_points(), partagée avec l'application

apprentissage <- readRDS("donnees/derivees/apprentissage.rds")
test <- readRDS("donnees/derivees/test.rds")
plis <- plis_stratifies(apprentissage$defaut)
FILS <- max(1, parallel::detectCores() - 1)

# Chaque modèle se décrit par deux fonctions : ajuster(données brutes) et predire(objet, données brutes).
# La préparation (imputation par la médiane) est réapprise à chaque ajustement, donc à l'intérieur de chaque pli.

# --- 1. Version soutenance : reproduction de la régression logistique de la v1 -
v1 <- list(
  ajuster = function(d) {
    params <- ajuster_preparation(d)
    x <- appliquer_preparation(d, params)
    x$score_risque <- x$taux_effort * x$taux_interet   # variable de la v1
    list(params = params, glm = glm(defaut ~ age + revenu_annuel + statut_logement + anciennete_emploi + motif_pret +
                                      note_risque + montant_pret + taux_interet + taux_effort + defaut_anterieur +
                                      anciennete_credit + score_risque, data = x, family = binomial))
  },
  predire = function(m, d) {
    x <- appliquer_preparation(d, m$params)
    x$score_risque <- x$taux_effort * x$taux_interet
    unname(predict(m$glm, x, type = "response"))
  }
)

# --- 2. Naïve Bayes (densités par noyau pour les variables asymétriques, réglé plus bas) --
VARIABLES_NB <- c(VARIABLES_NUMERIQUES, VARIABLES_QUALITATIVES)
fabriquer_nb <- function(noyau) list(
  ajuster = function(d) {
    params <- ajuster_preparation(d)
    x <- appliquer_preparation(d, params)
    list(params = params, nb = naive_bayes(x[, VARIABLES_NB], factor(x$defaut), usekernel = noyau, laplace = 1))
  },
  predire = function(m, d) {
    x <- appliquer_preparation(d, m$params)
    unname(predict(m$nb, x[, VARIABLES_NB], type = "prob")[, "1"])
  }
)

# --- 3. Régression logistique (log des montants, indicateurs de valeurs manquantes) ----
logistique <- list(
  ajuster = function(d) {
    params <- ajuster_preparation(d)
    x <- matrice_modele(appliquer_preparation(d, params), "lineaire")
    ajust <- glm.fit(cbind(constante = 1, x), d$defaut, family = binomial())
    list(params = params, coefficients = ajust$coefficients)
  },
  predire = function(m, d) {
    x <- cbind(constante = 1, matrice_modele(appliquer_preparation(d, m$params), "lineaire"))
    as.vector(plogis(x %*% m$coefficients))
  }
)

# --- 4. Logistique pénalisée (elastic net) : alpha et lambda réglés plus bas -----
fabriquer_elastic_net <- function(alpha, lambda) list(
  ajuster = function(d) {
    params <- ajuster_preparation(d)
    x <- matrice_modele(appliquer_preparation(d, params), "lineaire")
    ajust <- glmnet(x, d$defaut, family = "binomial", alpha = alpha)
    coefs <- as.matrix(coef(ajust, s = lambda))[, 1]
    list(params = params, coefficients = coefs)
  },
  predire = function(m, d) {
    x <- cbind(1, matrice_modele(appliquer_preparation(d, m$params), "lineaire"))
    as.vector(plogis(x %*% m$coefficients))
  }
)

# --- 5. Grille de score (méthode bancaire : WoE + logistique, puis points) ----
VARIABLES_GRILLE <- c(VARIABLES_NUMERIQUES, VARIABLES_QUALITATIVES)
grille <- list(
  ajuster = function(d) {
    params <- ajuster_preparation(d)
    x <- appliquer_preparation(d, params)[, c(VARIABLES_GRILLE, "defaut")]
    for (v in VARIABLES_QUALITATIVES) x[[v]] <- as.character(x[[v]])
    classes <- woebin(x, y = "defaut", x = VARIABLES_GRILLE, method = "tree", bin_num_limit = 6, print_info = FALSE)
    x_woe <- woebin_ply(x, classes, print_info = FALSE)
    # On ne garde que les variables suffisamment discriminantes (valeur d'information > 0,02).
    iv <- vapply(classes, function(b) b$total_iv[1], numeric(1))
    retenues <- paste0(names(iv)[iv > 0.02], "_woe")
    # Règle des grilles bancaires : chaque coefficient doit être positif (un WoE plus risqué => plus de risque).
    # Un signe négatif trahit une colinéarité (ex. défaut antérieur, déjà contenu dans la note du prêteur) et
    # donnerait des points absurdes : la variable est retirée et le modèle réestimé.
    repeat {
      modele <- glm(reformulate(retenues, "defaut"), data = x_woe, family = binomial)
      negatifs <- names(which(coef(modele)[-1] < 0))
      if (length(negatifs) == 0) break
      retenues <- setdiff(retenues, negatifs)
    }
    list(params = params, classes = classes, carte = scorecard(classes, modele, points0 = 600, odds0 = 1 / 19, pdo = 50))
  },
  predire = function(m, d) {
    x <- appliquer_preparation(d, m$params)[, VARIABLES_GRILLE]
    for (v in VARIABLES_QUALITATIVES) x[[v]] <- as.character(x[[v]])
    points <- scorecard_ply(x, m$carte, print_step = 0)$score
    probabilite_depuis_points(points)
  }
)

# --- 6. Gradient boosting (XGBoost), hyperparamètres réglés plus bas -----------
# Contraintes de monotonie : toutes choses égales par ailleurs, le risque ne peut que
# croître avec le montant, le taux d'effort, le taux d'intérêt, la note de risque et
# un défaut antérieur, et décroître avec le revenu. Sans elles, l'audit de cohérence
# (03_evaluation.R) montre qu'augmenter le revenu fait MONTER le risque pour 23 % des
# dossiers de test : inacceptable pour un modèle de décision de crédit.
contraintes_monotonie <- function(colonnes) {
  sens <- c(taux_effort = 1, montant_pret = 1, taux_interet = 1, note_risque_rang = 1, defaut_anterieur = 1, revenu_annuel = -1)
  unname(ifelse(colonnes %in% names(sens), sens[colonnes], 0))
}
fabriquer_xgboost <- function(reglage, sans = character()) list(
  ajuster = function(d) {
    params <- ajuster_preparation(d)
    x <- matrice_modele(appliquer_preparation(d, params), "arbres")
    x <- x[, setdiff(colnames(x), sans), drop = FALSE]
    p <- reglage$params
    if (isTRUE(reglage$monotone)) p$monotone_constraints <- contraintes_monotonie(colnames(x))
    set.seed(GRAINE)
    list(params = params, colonnes = colnames(x),
         booster = xgb.train(p, xgb.DMatrix(x, label = d$defaut), nrounds = reglage$nrounds, verbose = 0))
  },
  predire = function(m, d) {
    x <- matrice_modele(appliquer_preparation(d, m$params), "arbres")[, m$colonnes, drop = FALSE]
    predict(m$booster, xgb.DMatrix(x))
  }
)

# Prédictions « hors pli » : chaque dossier d'apprentissage est prédit par un modèle qui ne l'a pas vu.
hors_pli <- function(modele) {
  p <- numeric(nrow(apprentissage))
  for (k in sort(unique(plis))) {
    m <- modele$ajuster(apprentissage[plis != k, ])
    p[plis == k] <- modele$predire(m, apprentissage[plis == k, ])
  }
  p
}

# =============================================================================
# Réglage des hyperparamètres (validation croisée sur l'apprentissage uniquement)
# =============================================================================
cat("Réglage du Naïve Bayes...\n")
auc_nb <- c(gaussien = auc(apprentissage$defaut, hors_pli(fabriquer_nb(FALSE))),
            noyau = auc(apprentissage$defaut, hors_pli(fabriquer_nb(TRUE))))
print(round(auc_nb, 4))

cat("Réglage de l'elastic net...\n")
params_lin <- ajuster_preparation(apprentissage)
x_lin <- matrice_modele(appliquer_preparation(apprentissage, params_lin), "lineaire")
essais_en <- lapply(c(0, 0.25, 0.5, 0.75, 1), function(a) {
  cv <- cv.glmnet(x_lin, apprentissage$defaut, family = "binomial", alpha = a, foldid = plis, type.measure = "auc")
  data.frame(alpha = a, lambda = cv$lambda.min, auc_cv = max(cv$cvm))
})
essais_en <- do.call(rbind, essais_en)
print(essais_en, row.names = FALSE)
meilleur_en <- essais_en[which.max(essais_en$auc_cv), ]

cat("Réglage de XGBoost (grille + arrêt précoce)...\n")
params_arb <- ajuster_preparation(apprentissage)
x_arb <- matrice_modele(appliquer_preparation(apprentissage, params_arb), "arbres")
dm <- xgb.DMatrix(x_arb, label = apprentissage$defaut)
liste_plis <- lapply(sort(unique(plis)), function(k) which(plis == k))
grille_xgb <- expand.grid(max_depth = c(3, 5, 7), min_child_weight = c(1, 5), monotone = c(FALSE, TRUE))
essais_xgb <- lapply(seq_len(nrow(grille_xgb)), function(i) {
  g <- grille_xgb[i, ]
  p <- xgb.params(objective = "binary:logistic", eval_metric = "auc", learning_rate = 0.05,
                  max_depth = g$max_depth, min_child_weight = g$min_child_weight,
                  subsample = 0.8, colsample_bytree = 0.8, nthread = FILS, seed = GRAINE)
  if (g$monotone) p$monotone_constraints <- contraintes_monotonie(colnames(x_arb))
  set.seed(GRAINE)
  cv <- xgb.cv(p, dm, nrounds = 3000, folds = liste_plis, early_stopping_rounds = 100, verbose = FALSE)
  meilleur <- cv$early_stop$best_iteration
  data.frame(g, nrounds = meilleur, auc_cv = cv$evaluation_log$test_auc_mean[meilleur])
})
essais_xgb <- do.call(rbind, essais_xgb)
print(essais_xgb, row.names = FALSE)
# Deux modèles sont retenus : le meilleur AVEC contraintes (modèle de décision) et le
# meilleur SANS contrainte (référence, pour mesurer le prix de la cohérence).
fabriquer_reglage <- function(ligne) list(
  params = xgb.params(objective = "binary:logistic", learning_rate = 0.05, max_depth = ligne$max_depth,
                      min_child_weight = ligne$min_child_weight, subsample = 0.8, colsample_bytree = 0.8,
                      nthread = FILS, seed = GRAINE),
  monotone = ligne$monotone, nrounds = ligne$nrounds)
contraints <- essais_xgb[essais_xgb$monotone, ]
libres <- essais_xgb[!essais_xgb$monotone, ]
reglage_xgb <- fabriquer_reglage(contraints[which.max(contraints$auc_cv), ])
reglage_libre <- fabriquer_reglage(libres[which.max(libres$auc_cv), ])

# =============================================================================
# Modèles retenus : prédictions hors pli (choix du seuil) puis ajustement final
# =============================================================================
MODELES <- list(
  "Logistique v1 (soutenance)" = v1,
  "Naïve Bayes"                = fabriquer_nb(names(which.max(auc_nb)) == "noyau"),
  "Régression logistique"      = logistique,
  "Elastic net"                = fabriquer_elastic_net(meilleur_en$alpha, meilleur_en$lambda),
  "Grille de score"            = grille,
  "XGBoost"                    = fabriquer_xgboost(reglage_xgb),
  "XGBoost non contraint"      = fabriquer_xgboost(reglage_libre)
)

predictions <- list(hors_pli = list(), test = list())
modeles_finaux <- list()
for (nom in names(MODELES)) {
  cat("Validation croisée et ajustement final :", nom, "\n")
  predictions$hors_pli[[nom]] <- hors_pli(MODELES[[nom]])
  modeles_finaux[[nom]] <- MODELES[[nom]]$ajuster(apprentissage)
  predictions$test[[nom]] <- MODELES[[nom]]$predire(modeles_finaux[[nom]], test)
}

# Analyse de sensibilité : que vaut le meilleur modèle SANS la note de risque attribuée par le prêteur ?
xgb_sans_note <- fabriquer_xgboost(reglage_xgb, sans = "note_risque_rang")
m_sans_note <- xgb_sans_note$ajuster(apprentissage)
predictions$sans_note <- list(hors_pli = hors_pli(xgb_sans_note), test = xgb_sans_note$predire(m_sans_note, test))

saveRDS(predictions, "donnees/derivees/predictions.rds")
saveRDS(modeles_finaux, "donnees/derivees/modeles_finaux.rds")
saveRDS(list(naive_bayes = auc_nb, elastic_net = essais_en, xgboost = essais_xgb, xgboost_retenu = reglage_xgb, xgboost_libre = reglage_libre),
        "donnees/derivees/reglages.rds")
write.csv(essais_xgb, "resultats/reglage_xgboost.csv", row.names = FALSE)
write.csv(essais_en, "resultats/reglage_elastic_net.csv", row.names = FALSE)
cat("Modélisation terminée.\n")
