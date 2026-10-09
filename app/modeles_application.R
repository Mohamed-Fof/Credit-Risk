# =============================================================================
# Prédiction avec les modèles exportés — partagé par l'application et par le
# contrôle de l'export (R/04_export_application.R), qui vérifie que ce code
# reproduit exactement les prédictions obtenues à l'entraînement.
# =============================================================================

# Conversion points -> probabilité de défaut (600 points pour une cote de 1 contre 19 ; 50 points doublent la cote).
probabilite_depuis_points <- function(points, points0 = 600, odds0 = 1 / 19, pdo = 50) {
  facteur <- pdo / log(2)
  plogis((points0 + facteur * log(odds0) - points) / facteur)
}

# Points de la grille de score pour des dossiers déjà préparés (appliquer_preparation).
points_grille <- function(x, grille) {
  total <- rep(grille$points_base, nrow(x))
  for (v in unique(grille$table$variable)) {
    lignes <- grille$table[grille$table$variable == v, ]
    valeurs <- x[[v]]
    if (lignes$numerique[1]) {
      classe <- findInterval(valeurs, c(lignes$borne_inf, Inf), left.open = FALSE)
      total <- total + lignes$points[classe]
    } else {
      groupes <- strsplit(lignes$modalites, "%,%", fixed = TRUE)
      total <- total + vapply(as.character(valeurs), function(val) {
        lignes$points[which(vapply(groupes, function(g) val %in% g, logical(1)))[1]]
      }, numeric(1), USE.NAMES = FALSE)
    }
  }
  total
}

# Détail des points par variable pour un seul dossier (affichage de la grille).
detail_points <- function(x1, grille) {
  do.call(rbind, lapply(unique(grille$table$variable), function(v) {
    lignes <- grille$table[grille$table$variable == v, ]
    val <- x1[[v]]
    i <- if (lignes$numerique[1]) findInterval(val, c(lignes$borne_inf, Inf)) else
      which(vapply(strsplit(lignes$modalites, "%,%", fixed = TRUE), function(g) as.character(val) %in% g, logical(1)))[1]
    data.frame(variable = v, points = lignes$points[i])
  }))
}

lineaire <- function(x, coefficients) as.vector(plogis(cbind(1, x) %*% coefficients))

# Probabilités de défaut des cinq modèles pour des dossiers bruts (noms français).
predire_tous <- function(d, m) {
  x <- appliquer_preparation(d, m$params)
  x_lin <- matrice_modele(x, "lineaire")
  x_arb <- matrice_modele(x, "arbres")[, m$colonnes_xgboost, drop = FALSE]
  list(
    "XGBoost"               = predict(m$xgboost, xgboost::xgb.DMatrix(x_arb)),
    "Grille de score"       = probabilite_depuis_points(points_grille(x, m$grille_score)),
    "Régression logistique" = lineaire(x_lin, m$logistique),
    "Elastic net"           = lineaire(x_lin, m$elastic_net),
    "Naïve Bayes"           = unname(predict(m$naive_bayes, x[, c(VARIABLES_NUMERIQUES, VARIABLES_QUALITATIVES)], type = "prob")[, "1"])
  )
}

# Contributions de chaque variable à la décision de XGBoost pour un dossier (valeurs de Shapley).
expliquer <- function(d1, m) {
  x <- matrice_modele(appliquer_preparation(d1, m$params), "arbres")[, m$colonnes_xgboost, drop = FALSE]
  contrib <- predict(m$xgboost, xgboost::xgb.DMatrix(x), predcontrib = TRUE)[1, ]
  contrib <- contrib[!names(contrib) %in% c("(Intercept)", "BIAS")]
  # Les indicatrices d'une même variable qualitative sont regroupées sous un seul libellé.
  groupe <- sub("^(statut_logement|motif_pret)_.*$", "\\1", names(contrib))
  somme <- tapply(contrib, groupe, sum)
  data.frame(variable = names(somme), libelle = libelle_colonne(names(somme)), contribution = as.numeric(somme))
}
