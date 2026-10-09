# =============================================================================
# Tests automatiques — à lancer après R/executer_tout.R
#   Rscript tests/tests.R
# Ils échouent (code de sortie 1) au moindre écart : à relancer avant chaque mise en ligne.
# =============================================================================
suppressPackageStartupMessages({ library(xgboost); library(naivebayes) })
source("app/preparation.R")
source("app/modeles_application.R")
source("R/fonctions.R")

reussis <- 0; echecs <- 0
verifier <- function(description, condition) {
  if (isTRUE(condition)) { reussis <<- reussis + 1; cat("  ✓", description, "\n") }
  else { echecs <<- echecs + 1; cat("  ✗", description, "\n") }
}

M <- readRDS("app/modeles/modeles.rds")
M$xgboost <- xgb.load("app/modeles/xgboost.ubj")
test <- readRDS("donnees/derivees/test.rds")
base <- list(age = 30, revenu_annuel = 50000, statut_logement = "locataire", anciennete_emploi = 5, motif_pret = "personnel",
             note_risque = "B", montant_pret = 10000, taux_interet = 11, defaut_anterieur = "non", anciennete_credit = 4)
dossier <- function(...) modifyList(base, list(...))

cat("\n1. Contrôle des saisies (y compris venant d'un client modifié)\n")
verifier("un dossier normal est accepté", length(verifier_dossier(base)) == 0)
verifier("une modalité inventée est refusée", length(verifier_dossier(dossier(statut_logement = "<script>alert(1)</script>"))) > 0)
verifier("une note de risque inexistante est refusée", length(verifier_dossier(dossier(note_risque = "Z"))) > 0)
verifier("une valeur vide est refusée", length(verifier_dossier(dossier(motif_pret = character(0)))) > 0)
verifier("plusieurs valeurs au lieu d'une sont refusées", length(verifier_dossier(dossier(note_risque = c("A", "B")))) > 0)
verifier("du texte à la place d'un nombre est refusé", length(verifier_dossier(dossier(revenu_annuel = "system('ls')"))) > 0)
verifier("un nombre infini est refusé", length(verifier_dossier(dossier(montant_pret = Inf))) > 0)
verifier("un âge de 15 ans est refusé", length(verifier_dossier(dossier(age = 15))) > 0)
verifier("une ancienneté incompatible avec l'âge est refusée", length(verifier_dossier(dossier(age = 22, anciennete_emploi = 15))) > 0)
verifier("un prêt de plus de 5 ans de revenu est refusé", length(verifier_dossier(dossier(montant_pret = 300000))) > 0)

cat("\n2. Préparation des données\n")
x <- appliquer_preparation(test, M$params)
verifier("aucune valeur manquante après préparation", !anyNA(x[, c(VARIABLES_NUMERIQUES, VARIABLES_QUALITATIVES)]))
verifier("matrice « arbres » : colonnes identiques à l'entraînement", identical(colnames(matrice_modele(x, "arbres"))[seq_along(M$colonnes_xgboost)], M$colonnes_xgboost))
verifier("matrice « linéaire » : une colonne par coefficient", ncol(matrice_modele(x, "lineaire")) + 1 == length(M$logistique))
un <- appliquer_preparation(as.data.frame(base), M$params)
verifier("un dossier isolé garde toutes les modalités possibles", all(vapply(VARIABLES_QUALITATIVES, function(v) nlevels(un[[v]]) == length(MODALITES[[v]]), logical(1))))

cat("\n3. Prédictions\n")
p <- predire_tous(test, M)
verifier("cinq probabilités par dossier, toutes entre 0 et 1", length(p) == 5 && all(vapply(p, function(v) all(v >= 0 & v <= 1) && length(v) == nrow(test), logical(1))))
verifier("AUC du modèle de décision supérieure à 0,9 sur le test", auc(test$defaut, p$XGBoost) > 0.9)
predictions <- readRDS("donnees/derivees/predictions.rds")
verifier("l'application reproduit exactement l'entraînement", max(abs(p$XGBoost - predictions$test$XGBoost)) < 1e-6)
# Propriété d'efficacité des valeurs de Shapley : biais + somme des contributions = prédiction (en log-odds).
xm <- matrice_modele(appliquer_preparation(as.data.frame(base), M$params), "arbres")[, M$colonnes_xgboost, drop = FALSE]
contrib <- predict(M$xgboost, xgb.DMatrix(xm), predcontrib = TRUE)
logit <- qlogis(predict(M$xgboost, xgb.DMatrix(xm)))
verifier("les contributions de Shapley se somment à la prédiction", abs(sum(contrib) - logit) < 1e-4)
e <- expliquer(as.data.frame(base), M)
verifier("l'explication affichée regroupe toutes les contributions", abs(sum(e$contribution) - sum(contrib[colnames(contrib) != "BIAS" & colnames(contrib) != "(Intercept)"])) < 1e-6)

cat("\n4. Cohérence du modèle de décision (contraintes de monotonie)\n")
p0 <- p$XGBoost
modifie <- function(f) { d <- test; d <- f(d); predire_tous(d, M)$XGBoost }
verifier("montant plus élevé : le risque ne baisse jamais", all(modifie(function(d) { d$montant_pret <- d$montant_pret * 1.2; d }) >= p0 - 1e-6))
verifier("revenu plus élevé : le risque ne monte jamais", all(modifie(function(d) { d$revenu_annuel <- d$revenu_annuel * 1.2; d }) <= p0 + 1e-6))
verifier("taux plus élevé : le risque ne baisse jamais", all(modifie(function(d) { d$taux_interet <- d$taux_interet + 2; d }) >= p0 - 1e-6))
verifier("note dégradée : le risque ne baisse jamais", all(modifie(function(d) { d$note_risque <- LETTERS[pmin(match(d$note_risque, LETTERS) + 1, 7)]; d }) >= p0 - 1e-6))
verifier("défaut antérieur : le risque ne baisse jamais", all(modifie(function(d) { d$defaut_anterieur <- "oui"; d }) >= p0 - 1e-6))

cat("\n5. Grille de score\n")
g <- M$grille_score$table
verifier("chaque variable de la grille couvre tout l'axe des valeurs", all(vapply(split(g, g$variable), function(t) !t$numerique[1] || (min(t$borne_inf) == -Inf && max(t$borne_sup) == Inf), logical(1))))
verifier("plus de taux d'effort ne rapporte jamais de points", { t <- g[g$variable == "taux_effort", ]; all(diff(t$points[order(t$borne_inf)]) <= 0) })

cat(sprintf("\n%d tests réussis, %d échec(s)\n", reussis, echecs))
if (echecs > 0) quit(status = 1)
