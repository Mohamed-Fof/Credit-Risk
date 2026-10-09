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
  reglage_xgboost = readRDS("donnees/derivees/reglages.rds")$xgboost
), "app/modeles/evaluation.rds", compress = "xz")

# --- Données d'exploration : jeu nettoyé, codes français ---------------------
saveRDS(rbind(apprentissage, test), "app/modeles/exploration.rds", compress = "xz")

tailles <- file.info(list.files("app/modeles", full.names = TRUE))$size
cat(sprintf("Fichiers de l'application : %s (total %.2f Mo)\n",
            paste(basename(list.files("app/modeles")), collapse = ", "), sum(tailles) / 1e6))
