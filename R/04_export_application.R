# =============================================================================
# 04 — Export pour l'application
# Seul le strict nécessaire est exporté : coefficients, tables, booster XGBoost
# et grille de points. L'application démarre ainsi vite, sans données
# d'entraînement embarquées (le modèle de la v1 pesait 40 Mo).
# =============================================================================
suppressPackageStartupMessages({ library(xgboost); library(scorecard); library(naivebayes) })
source("app/preparation.R")
source("R/fonctions.R")

modeles_finaux <- readRDS("donnees/derivees/modeles_finaux.rds")
evaluation <- readRDS("donnees/derivees/evaluation.rds")
test <- readRDS("donnees/derivees/test.rds")
apprentissage <- readRDS("donnees/derivees/apprentissage.rds")

# --- Grille de score : transformée en table de points lisible sans package -----
carte <- modeles_finaux[["Grille de score"]]$carte
table_points <- do.call(rbind, lapply(setdiff(names(carte), "basepoints"), function(v) {
  b <- as.data.frame(carte[[v]])
  b <- b[b$bin != "missing", ]
  numerique <- v %in% VARIABLES_NUMERIQUES
  bornes <- if (numerique) do.call(rbind, regmatches(b$bin, regexec("^\\[(.+),(.+)\\)$", b$bin)))[, 2:3, drop = FALSE] else NULL
  data.frame(
    variable = v,
    numerique = numerique,
    borne_inf = if (numerique) as.numeric(bornes[, 1]) else NA,
    borne_sup = if (numerique) as.numeric(bornes[, 2]) else NA,
    modalites = if (numerique) NA else b$bin,   # modalités regroupées, séparées par « %,% »
    points = b$points
  )
}))
grille_score <- list(points_base = carte$basepoints$points, table = table_points)

# Vérification : la table reproduit exactement les points du package sur tout le jeu de test.
source("app/modeles_application.R")
params <- modeles_finaux[["XGBoost"]]$params
x_test <- appliquer_preparation(test, params)
points_app <- points_grille(x_test, grille_score)
x_pkg <- x_test[, c(VARIABLES_NUMERIQUES, VARIABLES_QUALITATIVES)]
for (v in VARIABLES_QUALITATIVES) x_pkg[[v]] <- as.character(x_pkg[[v]])
points_pkg <- scorecard_ply(x_pkg, carte, print_step = 0)$score
stopifnot("La table de points ne reproduit pas la grille" = isTRUE(all.equal(points_app, points_pkg)))

# --- Modèles allégés -----------------------------------------------------------
dir.create("app/modeles", showWarnings = FALSE)
invisible(xgb.save(modeles_finaux[["XGBoost"]]$booster, "app/modeles/xgboost.ubj"))
modeles_app <- list(
  params = params,
  colonnes_xgboost = modeles_finaux[["XGBoost"]]$colonnes,
  logistique = modeles_finaux[["Régression logistique"]]$coefficients,
  elastic_net = modeles_finaux[["Elastic net"]]$coefficients,
  naive_bayes = modeles_finaux[["Naïve Bayes"]]$nb,
  grille_score = grille_score,
  seuils = evaluation$seuils,
  # Intervalles observés à l'entraînement, pour signaler les extrapolations dans l'application.
  plages = lapply(setNames(nm = VARIABLES_NUMERIQUES[VARIABLES_NUMERIQUES != "taux_effort"]), function(v) range(apprentissage[[v]], na.rm = TRUE))
)
saveRDS(modeles_app, "app/modeles/modeles.rds", compress = "xz")

# Vérification : l'application retrouve les mêmes probabilités que l'entraînement.
modeles_app$xgboost <- xgb.load("app/modeles/xgboost.ubj")
predit <- predire_tous(test, modeles_app)
predictions <- readRDS("donnees/derivees/predictions.rds")
for (n in c("XGBoost", "Régression logistique", "Elastic net", "Naïve Bayes", "Grille de score")) {
  ecart <- max(abs(predit[[n]] - predictions$test[[n]]))
  if (ecart > 1e-6) stop(sprintf("Écart de %.2e pour %s entre l'application et l'entraînement", ecart, n))
}
cat("Contrôle : l'application reproduit les prédictions des 5 modèles sur les", nrow(test), "dossiers de test.\n")

# --- Résultats affichés dans l'application (courbes allégées) ----------------
alleger <- function(d) do.call(rbind, lapply(split(d, d$cle), function(x) x[unique(round(seq(1, nrow(x), length.out = 150))), ]))
saveRDS(list(
  test = evaluation$test, validation = evaluation$validation, comparaisons = evaluation$comparaisons, coherence = evaluation$coherence,
  sans_note = evaluation$sans_note, meilleur = evaluation$meilleur, courbes = alleger(evaluation$courbes),
  calibration = evaluation$calibration, importance = evaluation$importance,
  journal = read.csv("resultats/journal_nettoyage.csv"),
  ratio_cout = RATIO_COUT, n_apprentissage = nrow(apprentissage), n_test = nrow(test),
  reglage_xgboost = readRDS("donnees/derivees/reglages.rds")$xgboost,
  n_dossiers = nrow(apprentissage) + nrow(test), taux_defaut = mean(c(apprentissage$defaut, test$defaut))
), "app/modeles/evaluation.rds", compress = "xz")


tailles <- file.info(list.files("app/modeles", full.names = TRUE))$size
cat(sprintf("Fichiers de l'application : %s (total %.2f Mo)\n",
            paste(basename(list.files("app/modeles")), collapse = ", "), sum(tailles) / 1e6))

# --- Graphiques de l'application, dessinés ici une fois pour toutes ----------------
# Le serveur gratuit de Render dispose d'environ 0,1 processeur : y dessiner un graphique
# ggplot coûte près d'une seconde. Ces images ne changent pas d'un visiteur à l'autre ;
# elles sont donc produites à l'export, et ggplot2 n'est plus chargé par l'application.
suppressPackageStartupMessages(library(ggplot2))
dir.create("app/www/figures", recursive = TRUE, showWarnings = FALSE)
BLEU <- "#2563eb"; VIOLET <- "#7c3aed"; ROUGE <- "#dc2626"
COULEURS <- c("Logistique v1 (soutenance)" = "#94a3b8", "Naïve Bayes" = "#f59e0b", "Régression logistique" = "#16a34a",
              "Elastic net" = "#0891b2", "Grille de score" = "#7c3aed", "XGBoost" = BLEU, "XGBoost non contraint" = "#93c5fd")
virgule <- scales::label_number(decimal.mark = ",", big.mark = " ", drop0trailing = TRUE)
pct_axe <- scales::label_percent(decimal.mark = ",", suffix = " %")
theme_set(theme_minimal(base_size = 13) + theme(legend.position = "bottom", panel.grid.minor = element_blank()))
image_app <- function(g, nom, l = 7.2, h = 4.6) ggsave(file.path("app/www/figures", nom), g, width = l, height = h, dpi = 144, bg = "white")

courbes <- alleger(evaluation$courbes)
etiquettes <- setNames(unique(courbes$Modele), unique(courbes$cle))
image_app(ggplot(courbes, aes(fpr, tpr, colour = cle)) + geom_abline(linetype = 2, colour = "grey70") +
  geom_path(linewidth = 1) + scale_colour_manual(values = COULEURS, labels = etiquettes, name = NULL) +
  scale_x_continuous(labels = pct_axe) + scale_y_continuous(labels = pct_axe) + guides(colour = guide_legend(ncol = 2)) +
  labs(x = "Bons clients refusés", y = "Défauts détectés"), "roc.png", 7.2, 5.6)
image_app(ggplot(evaluation$calibration, aes(predit, observe, colour = Modele)) + geom_abline(linetype = 2, colour = "grey60") +
  geom_line(linewidth = 0.8) + geom_point(size = 2) + scale_colour_manual(values = COULEURS, name = NULL) +
  scale_x_continuous(labels = pct_axe, limits = c(0, 1)) + scale_y_continuous(labels = pct_axe, limits = c(0, 1)) +
  labs(x = "Probabilité prédite", y = "Défauts observés"), "calibration.png", 7.2, 5.6)
image_app(ggplot(evaluation$importance, aes(valeur, reorder(Variable, valeur))) + geom_col(fill = BLEU) +
  scale_x_continuous(labels = virgule) + labs(x = "Contribution moyenne (|Shapley|)", y = NULL), "importance.png", 7.6, 4.8)

exploration <- rbind(apprentissage, test)
exploration$statut <- factor(ifelse(exploration$defaut == 1, "Défaut", "Remboursé"), levels = c("Remboursé", "Défaut"))
for (v in VARIABLES_NUMERIQUES) {
  d <- exploration
  if (v == "taux_effort") d$taux_effort <- d$montant_pret / d$revenu_annuel
  d <- d[!is.na(d[[v]]), ]
  image_app(ggplot(d, aes(.data[[v]], fill = statut)) + geom_density(alpha = 0.55, colour = NA) +
    coord_cartesian(xlim = quantile(d[[v]], c(0.005, 0.995))) +
    scale_fill_manual(values = c("Remboursé" = BLEU, "Défaut" = ROUGE), name = NULL) +
    scale_x_continuous(labels = virgule) + scale_y_continuous(labels = virgule) +
    labs(x = LIBELLES_VARIABLES[v], y = "Densité", caption = "Valeurs extrêmes (0,5 % de chaque côté) hors du cadre"),
    paste0("exploration_", v, ".png"))
}
for (v in VARIABLES_QUALITATIVES) {
  t <- aggregate(defaut ~ modalite, data = data.frame(modalite = exploration[[v]], defaut = exploration$defaut), FUN = mean)
  t$libelle <- LIBELLES_MODALITES[[v]][as.character(t$modalite)]
  image_app(ggplot(t, aes(defaut, reorder(libelle, defaut))) + geom_col(fill = VIOLET) +
    geom_text(aes(label = paste0(formatC(100 * defaut, format = "f", digits = 1, decimal.mark = ","), " %")), hjust = -0.1, size = 4.2) +
    scale_x_continuous(labels = pct_axe, expand = expansion(mult = c(0, 0.18))) + labs(x = "Taux de défaut", y = NULL),
    paste0("exploration_", v, ".png"))
}
cat(sprintf("Graphiques de l'application : %d images dans app/www/figures (%.2f Mo)\n",
            length(list.files("app/www/figures")), sum(file.info(list.files("app/www/figures", full.names = TRUE))$size) / 1e6))
