# =============================================================================
# Image de l'application — pensée pour démarrer vite sur Render
#  - image R minimale (rocker/r-ver, 365 Mo) au lieu de rocker/shiny (580 Mo) ;
#  - paquets binaires précompilés (Posit Package Manager) : pas de compilation,
#    construction en quelques minutes ;
#  - versions figées à la date d'entraînement des modèles : l'application lit le
#    booster XGBoost avec la même version que celle qui l'a créé ;
#  - seulement 5 paquets et le dossier app/ (2,3 Mo de modèles), lancés sans shiny-server.
# =============================================================================
FROM rocker/r-ver:4.5.1

ARG INSTANTANE_CRAN=2026-10-09
RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") \
 && R -q -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/${CODENAME}/${INSTANTANE_CRAN}')); \
             install.packages(c('shiny', 'bslib', 'ggplot2', 'xgboost', 'naivebayes'), Ncpus = 4); \
             invisible(lapply(c('shiny', 'bslib', 'ggplot2', 'xgboost', 'naivebayes'), library, character.only = TRUE))" \
 && rm -rf /tmp/downloaded_packages

RUN useradd --create-home appli
WORKDIR /app
COPY --chown=appli:appli app/ /app/
USER appli

# Render fournit le port dans $PORT (10000 par défaut).
ENV PORT=10000
EXPOSE 10000
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '10000')), launch.browser = FALSE)"]
