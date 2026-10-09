# =============================================================================
# Image de l'application — pensée pour démarrer vite sur Render
#  - image R minimale (rocker/r-ver, 365 Mo) au lieu de rocker/shiny (580 Mo) ;
#  - paquets binaires précompilés (Posit Package Manager) : pas de compilation,
#    construction en quelques minutes ;
#  - versions figées à la date d'entraînement des modèles : l'application lit le
#    booster XGBoost avec la même version que celle qui l'a créé ;
#  - seulement 4 paquets et le dossier app/ (modèles et images : 1,5 Mo), lancés sans shiny-server.
# =============================================================================
FROM rocker/r-ver:4.5.1

# Locale UTF-8 explicite : sans elle, R ne lit pas les accents des scripts (« Régression »,
# « Naïve Bayes ») et l'application ne se charge pas.
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
# Un seul fil OpenMP : le conteneur ne dispose que d'une fraction de processeur ; plusieurs fils
# (XGBoost en lance un par cœur de la machine hôte) se disputeraient ce quota en attente active.
ENV OMP_NUM_THREADS=1

# Bibliothèque système requise par le paquet fs (utilisé par sass pour compiler le thème).
# Les paquets précompilés de Posit ne l'embarquent pas : sans elle, la page d'accueil
# plante au premier affichage (liste fournie par l'API sysreqs de Posit).
RUN apt-get update \
 && apt-get install -y --no-install-recommends libuv1 \
 && rm -rf /var/lib/apt/lists/*

ARG INSTANTANE_CRAN=2026-10-09
RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") \
 && R -q -e "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/${CODENAME}/${INSTANTANE_CRAN}')); \
             install.packages(c('shiny', 'bslib', 'xgboost', 'naivebayes'), Ncpus = 4); \
             invisible(lapply(c('shiny', 'bslib', 'xgboost', 'naivebayes'), library, character.only = TRUE))" \
 && rm -rf /tmp/downloaded_packages

RUN useradd --create-home appli
WORKDIR /app
COPY --chown=appli:appli app/ /app/
USER appli

# Contrôle à la construction : l'application doit se charger ET la page d'accueil s'afficher
# (thème compilé, toutes les bibliothèques chargées). Sinon la construction échoue et Render
# garde la version précédente en ligne.
RUN Rscript -e "setwd('/app'); e <- new.env(); app <- source('app.R', local = e)\$value; \
                page <- htmltools::renderTags(e\$ui); \
                stopifnot(inherits(app, 'shiny.appobj'), grepl('Scoring de risque', page\$html), length(page\$dependencies) > 0)"

# Render fournit le port dans $PORT (10000 par défaut).
ENV PORT=10000
EXPOSE 10000
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '10000')), launch.browser = FALSE)"]
