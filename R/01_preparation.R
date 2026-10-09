# =============================================================================
# 01 — Préparation des données
# Lecture, renommage en français, nettoyage, découpage apprentissage / test.
# Sortie : donnees/derivees/{apprentissage,test}.rds et le journal de nettoyage.
# =============================================================================
source("app/preparation.R")
source("R/fonctions.R")

brut <- read.csv("donnees/credit_risk_dataset.csv")
df <- renommer_donnees_brutes(brut)
journal <- data.frame(etape = "Jeu de données brut", lignes = nrow(df))
noter <- function(etape) journal <<- rbind(journal, data.frame(etape = etape, lignes = nrow(df)))

# 1. Doublons exacts : retirés AVANT le découpage, sinon un même dossier peut se
#    retrouver à la fois en apprentissage et en test (fuite d'information).
df <- df[!duplicated(df), ]
noter("Après suppression des doublons exacts")

# 2. Valeurs impossibles
df <- df[df$age <= 100, ]
noter("Après suppression des âges supérieurs à 100 ans")
df <- df[is.na(df$anciennete_emploi) | df$anciennete_emploi <= df$age - 14, ]
noter("Après suppression des anciennetés incompatibles avec l'âge")

# 3. Découpage stratifié 80 / 20. Le jeu de test n'est plus touché jusqu'à l'évaluation finale.
indices_test <- decoupage_stratifie(df$defaut)
apprentissage <- df[-indices_test, ]
test <- df[indices_test, ]

dir.create("donnees/derivees", showWarnings = FALSE)
saveRDS(apprentissage, "donnees/derivees/apprentissage.rds")
saveRDS(test, "donnees/derivees/test.rds")
write.csv(journal, "resultats/journal_nettoyage.csv", row.names = FALSE)

cat("Journal de nettoyage :\n"); print(journal, row.names = FALSE)
cat(sprintf("\nApprentissage : %d dossiers (%.1f %% de défauts) | Test : %d dossiers (%.1f %% de défauts)\n",
            nrow(apprentissage), 100 * mean(apprentissage$defaut), nrow(test), 100 * mean(test$defaut)))
