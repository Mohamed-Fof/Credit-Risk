# =============================================================================
# 05 — Chiffres du guide (docs/guide_projet.tex)
# Écrit docs/chiffres.tex : chaque nombre cité dans le guide vient des résultats,
# jamais d'une saisie manuelle. Relancer après R/executer_tout.R, puis recompiler.
# =============================================================================
# Valeurs exactes (non arrondies) : les CSV de resultats/ sont arrondis à 4 décimales,
# ce qui créerait des doubles arrondis (77,15 % affiché 77,1 %).
evaluation <- readRDS("donnees/derivees/evaluation.rds")
test <- evaluation$test
coherence <- evaluation$coherence
sens <- evaluation$sans_note
journal <- read.csv("resultats/journal_nettoyage.csv")
reglages <- readRDS("donnees/derivees/reglages.rds")
apprentissage <- readRDS("donnees/derivees/apprentissage.rds")
test_brut <- readRDS("donnees/derivees/test.rds")

v <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = "{,}")
p <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d, decimal.mark = "{,}"), "\\,\\%")
n <- function(x) gsub(",", "\\,", formatC(x, format = "d", big.mark = ","), fixed = TRUE)   # espace fine LaTeX
ligne <- function(m) test[test$Modele == m, ]
cmd <- function(nom, valeur) sprintf("\\newcommand{\\%s}{%s}", nom, valeur)

xgb <- ligne("XGBoost"); libre <- ligne("XGBoost non contraint"); v1 <- ligne("Logistique v1 (soutenance)")
logit <- ligne("Régression logistique"); grille <- ligne("Grille de score"); nb <- ligne("Naïve Bayes"); en <- ligne("Elastic net")
retenu <- reglages$xgboost[reglages$xgboost$monotone, ]; retenu <- retenu[which.max(retenu$auc_cv), ]
libre_cv <- reglages$xgboost[!reglages$xgboost$monotone, ]; libre_cv <- libre_cv[which.max(libre_cv$auc_cv), ]
tous <- rbind(apprentissage, test_brut); tous$te <- tous$montant_pret / tous$revenu_annuel
regle <- subset(tous, statut_logement == "locataire" & note_risque %in% c("A", "B") & te >= 0.32)
co <- function(m, s) coherence[coherence$Modele == m, s]

lignes <- c(
  "% Fichier généré par R/05_chiffres_document.R — ne pas modifier à la main.",
  cmd("nBrut", n(journal$lignes[1])), cmd("nDoublons", n(journal$lignes[1] - journal$lignes[2])),
  cmd("nFinal", n(tail(journal$lignes, 1))), cmd("nApprentissage", n(nrow(apprentissage))), cmd("nTest", n(nrow(test_brut))),
  cmd("tauxDefaut", p(mean(tous$defaut))),
  cmd("nRegle", n(nrow(regle))), cmd("tauxRegle", p(mean(regle$defaut))),
  cmd("aucXgb", v(xgb$AUC)), cmd("icXgb", gsub("\\.", "{,}", xgb$AUC_IC95)), cmd("giniXgb", v(xgb$Gini)), cmd("ksXgb", v(xgb$KS)),
  cmd("brierXgb", v(xgb$Brier, 4)), cmd("seuilXgb", p(xgb$Seuil)), cmd("rappelXgb", p(xgb$Rappel)), cmd("precisionXgb", p(xgb$Precision)),
  cmd("specXgb", p(xgb$Specificite)), cmd("coutXgb", v(xgb$Cout)),
  cmd("aucLibre", v(libre$AUC)), cmd("icLibre", gsub("\\.", "{,}", libre$AUC_IC95)), cmd("rappelLibre", p(libre$Rappel)), cmd("coutLibre", v(libre$Cout)),
  cmd("aucVun", v(v1$AUC)), cmd("rappelVun", p(v1$Rappel)), cmd("precisionVun", p(v1$Precision)), cmd("coutVun", v(v1$Cout)),
  cmd("aucLogit", v(logit$AUC)), cmd("aucGrille", v(grille$AUC)), cmd("aucNb", v(nb$AUC)), cmd("aucEn", v(en$AUC)),
  cmd("gainCout", p(1 - xgb$Cout / v1$Cout, 0)), cmd("prixCoherence", v(libre$AUC - xgb$AUC)),
  cmd("aucSansNote", v(sens$AUC_test[2])),
  cmd("aucCvRetenu", v(retenu$auc_cv)), cmd("aucCvLibre", v(libre_cv$auc_cv)),
  cmd("profondeurRetenue", retenu$max_depth), cmd("poidsRetenu", retenu$min_child_weight), cmd("arbresRetenus", retenu$nrounds),
  cmd("aucNbGauss", v(reglages$naive_bayes[["gaussien"]])), cmd("aucNbNoyau", v(reglages$naive_bayes[["noyau"]])),
  cmd("alphaEn", v(reglages$elastic_net$alpha[which.max(reglages$elastic_net$auc_cv)], 2)),
  cmd("cohRevenuLibre", p(co("XGBoost non contraint", "Revenu +20 %"))), cmd("cohMontantLibre", p(co("XGBoost non contraint", "Montant +20 %"))),
  cmd("cohTauxLibre", p(co("XGBoost non contraint", "Taux +2 points"))), cmd("cohMontantLogit", p(co("Régression logistique", "Montant +20 %")))
)
dir.create("docs", showWarnings = FALSE)
writeLines(lignes, "docs/chiffres.tex", useBytes = TRUE)
cat("docs/chiffres.tex :", length(lignes) - 1, "chiffres écrits\n")
