# =============================================================================
# Reproduit l'intégralité du projet, des données brutes à l'application.
# Usage, depuis la racine du dépôt :  Rscript R/executer_tout.R
# Durée : une quinzaine de minutes (réglage de XGBoost compris).
# =============================================================================
etapes <- c("R/01_preparation.R", "R/02_modelisation.R", "R/03_evaluation.R", "R/04_export_application.R",
            "R/05_chiffres_document.R")
for (e in etapes) {
  cat("\n==========", e, "==========\n")
  debut <- Sys.time()
  source(e, encoding = "UTF-8", echo = FALSE, local = new.env())
  cat(sprintf("(%s terminé en %.1f min)\n", e, as.numeric(Sys.time() - debut, units = "mins")))
}
# Versions exactes de R et des paquets, pour pouvoir reproduire les résultats à l'identique.
writeLines(capture.output(sessionInfo()), "resultats/session_R.txt")
cat("\nVersions enregistrées dans resultats/session_R.txt\n")
