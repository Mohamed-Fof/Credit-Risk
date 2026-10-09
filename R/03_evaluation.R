# =============================================================================
# 03 — Évaluation
# Seuils choisis sur les prédictions hors pli (jamais sur le test), puis mesure
# unique de chaque modèle sur le jeu de test. Figures et tableaux dans resultats/.
# =============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(pROC); library(xgboost) })
source("app/preparation.R")
source("R/fonctions.R")

apprentissage <- readRDS("donnees/derivees/apprentissage.rds")
test <- readRDS("donnees/derivees/test.rds")
predictions <- readRDS("donnees/derivees/predictions.rds")
modeles_finaux <- readRDS("donnees/derivees/modeles_finaux.rds")
noms <- names(predictions$test)

# --- Seuils : la v1 garde son seuil d'origine (0,5) ; les autres minimisent le coût hors pli
seuils <- vapply(noms, function(n) {
  if (n == "Logistique v1 (soutenance)") 0.5 else seuil_optimal(apprentissage$defaut, predictions$hors_pli[[n]])
}, numeric(1))

tableau <- function(y, liste) {
  t <- do.call(rbind, lapply(noms, function(n) cbind(Modele = n, mesures(y, liste[[n]], seuils[[n]]))))
  rownames(t) <- NULL
  t
}
validation <- tableau(apprentissage$defaut, predictions$hors_pli)
resultats_test <- tableau(test$defaut, predictions$test)

# Intervalles de confiance à 95 % de l'AUC (méthode de DeLong)
rocs <- lapply(predictions$test, function(p) roc(test$defaut, p, levels = c(0, 1), direction = "<", quiet = TRUE))
ic <- t(vapply(rocs, function(r) as.numeric(ci.auc(r))[c(1, 3)], numeric(2)))
resultats_test$AUC_IC95 <- sprintf("[%.3f ; %.3f]", ic[, 1], ic[, 2])

# Modèle de décision : XGBoost AVEC contraintes de cohérence. Le non contraint a une AUC un peu
# plus haute, mais réagit à contresens (voir l'audit de cohérence ci-dessous) : il sert de référence.
meilleur <- "XGBoost"
comparaisons <- data.frame(
  Comparaison = paste(meilleur, "vs", setdiff(noms, meilleur)),
  p_valeur_DeLong = vapply(setdiff(noms, meilleur), function(n) roc.test(rocs[[meilleur]], rocs[[n]])$p.value, numeric(1)),
  row.names = NULL
)

arrondir <- function(d) { d[] <- lapply(d, function(x) if (is.numeric(x)) round(x, 4) else x); d }

# Audit de cohérence : part des dossiers de test dont le risque bouge dans le MAUVAIS sens
# (de plus d'un point) quand on modifie une seule saisie, toutes choses égales par ailleurs.
predire_modele <- function(m, d) {
  x <- appliquer_preparation(d, m$params)
  if (!is.null(m$booster)) return(predict(m$booster, xgb.DMatrix(matrice_modele(x, "arbres")[, m$colonnes, drop = FALSE])))
  as.vector(plogis(cbind(1, matrice_modele(x, "lineaire")) %*% m$coefficients))
}
cran_suivant <- function(n) LETTERS[pmin(match(n, LETTERS) + 1, 7)]
SCENARIOS <- list(
  "Montant +20 %" = list(modif = function(d) { d$montant_pret <- d$montant_pret * 1.2; d }, sens = 1),
  "Revenu +20 %"  = list(modif = function(d) { d$revenu_annuel <- d$revenu_annuel * 1.2; d }, sens = -1),
  "Taux +2 points" = list(modif = function(d) { d$taux_interet <- d$taux_interet + 2; d }, sens = 1),
  "Note dégradée d'un cran" = list(modif = function(d) { d$note_risque <- cran_suivant(d$note_risque); d }, sens = 1)
)
coherence <- do.call(rbind, lapply(c("XGBoost", "XGBoost non contraint", "Régression logistique"), function(n) {
  m <- modeles_finaux[[n]]
  p0 <- predire_modele(m, test)
  cbind(Modele = n, as.data.frame(lapply(SCENARIOS, function(sc) {
    p1 <- predire_modele(m, sc$modif(test))
    mean(if (sc$sens > 0) p1 < p0 - 0.01 else p1 > p0 + 0.01)
  }), check.names = FALSE))
}))
write.csv(arrondir(coherence), "resultats/coherence.csv", row.names = FALSE)
cat("\nAudit de cohérence (part des dossiers à contresens, écart > 1 point) :\n"); print(arrondir(coherence), row.names = FALSE)

# Analyse de sensibilité : sans la note de risque du prêteur
sans_note <- data.frame(
  Modele = c("XGBoost (contraint) avec la note du prêteur", "XGBoost (contraint) sans la note du prêteur"),
  AUC_test = c(auc(test$defaut, predictions$test[["XGBoost"]]), auc(test$defaut, predictions$sans_note$test))
)

write.csv(arrondir(validation), "resultats/metriques_validation_croisee.csv", row.names = FALSE)
write.csv(arrondir(resultats_test), "resultats/metriques_test.csv", row.names = FALSE)
comparaisons$p_valeur_DeLong <- format.pval(comparaisons$p_valeur_DeLong, digits = 2, eps = 1e-4)
write.csv(comparaisons, "resultats/tests_delong.csv", row.names = FALSE)
write.csv(arrondir(sans_note), "resultats/sensibilite_note_preteur.csv", row.names = FALSE)

cat("\n=== Jeu de test (", nrow(test), " dossiers jamais vus) ===\n", sep = "")
print(arrondir(resultats_test[, c("Modele", "AUC", "AUC_IC95", "Gini", "KS", "Brier", "Seuil", "Rappel", "Precision", "Specificite", "Cout")]), row.names = FALSE)
cat("\nTests de DeLong :\n"); print(comparaisons, row.names = FALSE)
cat("\nSensibilité à la note du prêteur :\n"); print(arrondir(sans_note), row.names = FALSE)

# =============================================================================
# Figures
# =============================================================================
theme_set(theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom"))
COULEURS <- c("Logistique v1 (soutenance)" = "#94a3b8", "Naïve Bayes" = "#f59e0b", "Régression logistique" = "#16a34a",
              "Elastic net" = "#0891b2", "Grille de score" = "#7c3aed", "XGBoost" = "#2563eb", "XGBoost non contraint" = "#93c5fd")
enregistrer <- function(graphique, nom, l = 8, h = 5.5) ggsave(file.path("resultats/figures", nom), graphique, width = l, height = h, dpi = 150, bg = "white")

# 1. Courbes ROC sur le test
courbes <- do.call(rbind, lapply(noms, function(n) {
  r <- rocs[[n]]
  data.frame(Modele = sprintf("%s (AUC %s)", n, sub(".", ",", sprintf("%.3f", as.numeric(pROC::auc(r))), fixed = TRUE)), cle = n,
             fpr = 1 - r$specificities, tpr = r$sensitivities)
}))
etiquettes <- setNames(unique(courbes$Modele), unique(courbes$cle))
enregistrer(ggplot(courbes, aes(fpr, tpr, colour = cle)) +
  geom_abline(linetype = 2, colour = "grey70") + geom_path(linewidth = 0.9) +
  scale_colour_manual(values = COULEURS, labels = etiquettes, name = NULL) +
  guides(colour = guide_legend(ncol = 2)) + coord_equal() +
  labs(title = "Courbes ROC sur le jeu de test", x = "Taux de faux positifs (bons clients refusés)",
       y = "Taux de vrais positifs (défauts détectés)"), "roc_test.png", 8, 8)

# 2. Calibration : probabilité prédite vs taux de défaut observé, par décile
calib <- do.call(rbind, lapply(c("Régression logistique", "Grille de score", "XGBoost", "Naïve Bayes"), function(n) {
  p <- predictions$test[[n]]
  decile <- cut(p, unique(quantile(p, seq(0, 1, 0.1))), include.lowest = TRUE)
  data.frame(Modele = n, predit = tapply(p, decile, mean), observe = tapply(test$defaut, decile, mean))
}))
enregistrer(ggplot(calib, aes(predit, observe, colour = Modele)) +
  geom_abline(linetype = 2, colour = "grey60") + geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_colour_manual(values = COULEURS, name = NULL) + coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
  labs(title = "Calibration : probabilité annoncée vs défaut réel (déciles)",
       subtitle = "Sur la diagonale, une probabilité de 30 % correspond bien à 30 % de défauts",
       x = "Probabilité de défaut prédite", y = "Taux de défaut observé"), "calibration.png", 8, 8)

# 3. Importance des variables de XGBoost (valeurs de Shapley moyennes sur le test)
xgb_final <- modeles_finaux[["XGBoost"]]
x_test <- matrice_modele(appliquer_preparation(test, xgb_final$params), "arbres")[, xgb_final$colonnes]
contributions <- predict(xgb_final$booster, xgb.DMatrix(x_test), predcontrib = TRUE)
contributions <- contributions[, colnames(contributions) != "(Intercept)" & colnames(contributions) != "BIAS", drop = FALSE]
importance <- data.frame(colonne = colnames(contributions), valeur = colMeans(abs(contributions)))
importance$Variable <- libelle_colonne(importance$colonne)
importance <- head(importance[order(-importance$valeur), ], 12)
write.csv(arrondir(importance[, c("Variable", "valeur")]), "resultats/importance_xgboost.csv", row.names = FALSE)
enregistrer(ggplot(importance, aes(valeur, reorder(Variable, valeur))) +
  geom_col(fill = "#2563eb") +
  theme(plot.title.position = "plot") +
  labs(title = "Variables les plus influentes (XGBoost)",
       subtitle = "Contribution moyenne à la décision (valeurs de Shapley, en valeur absolue)",
       x = "Contribution moyenne (échelle log-odds)", y = NULL), "importance_xgboost.png")

# 4. Coût attendu selon le seuil (meilleur modèle, prédictions hors pli)
s <- seq(0.02, 0.9, by = 0.005)
couts <- data.frame(seuil = s, cout = vapply(s, function(x) cout_moyen(apprentissage$defaut, predictions$hors_pli[[meilleur]], x), numeric(1)))
enregistrer(ggplot(couts, aes(seuil, cout)) + geom_line(linewidth = 0.9, colour = "#2563eb") +
  geom_vline(xintercept = seuils[[meilleur]], linetype = 2, colour = "#dc2626") +
  geom_vline(xintercept = 0.5, linetype = 3, colour = "grey50") +
  annotate("text", x = seuils[[meilleur]], y = max(couts$cout), hjust = -0.1, colour = "#dc2626",
           label = sprintf("seuil retenu : %.3f", seuils[[meilleur]])) +
  annotate("text", x = 0.5, y = max(couts$cout) * 0.9, hjust = -0.1, colour = "grey40", label = "seuil 0,5 (v1)") +
  labs(title = sprintf("Choix du seuil de décision (%s)", meilleur),
       subtitle = sprintf("Hypothèse : accorder un prêt à un futur défaillant coûte %d fois plus que refuser un bon client", RATIO_COUT),
       x = "Seuil de probabilité au-delà duquel le prêt est refusé", y = "Coût moyen par dossier"), "cout_seuil.png")

saveRDS(list(seuils = seuils, test = resultats_test, validation = validation, comparaisons = comparaisons, coherence = coherence,
             sans_note = sans_note, meilleur = meilleur, courbes = courbes, calibration = calib, importance = importance),
        "donnees/derivees/evaluation.rds")
cat("\nModèle de décision :", meilleur, "\nFigures enregistrées dans resultats/figures/\n")
