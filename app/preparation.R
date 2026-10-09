# =============================================================================
# Préparation des données — source unique
# -----------------------------------------------------------------------------
# Ce fichier est utilisé à la fois par les scripts d'entraînement (R/) et par
# l'application Shiny (app/). Un client saisi dans l'application est ainsi
# transformé exactement comme les données d'entraînement : aucun écart possible
# entre ce que le modèle a appris et ce qu'on lui présente en production.
# =============================================================================

# --- Dictionnaire : noms d'origine (anglais) -> noms de travail (français) ----
NOMS_VARIABLES <- c(
  person_age                 = "age",
  person_income              = "revenu_annuel",
  person_home_ownership      = "statut_logement",
  person_emp_length          = "anciennete_emploi",
  loan_intent                = "motif_pret",
  loan_grade                 = "note_risque",
  loan_amnt                  = "montant_pret",
  loan_int_rate              = "taux_interet",
  loan_status                = "defaut",
  loan_percent_income        = "taux_effort_arrondi",
  cb_person_default_on_file  = "defaut_anterieur",
  cb_person_cred_hist_length = "anciennete_credit"
)

# Modalités : codes de travail (sans accent ni espace) et libellés affichés.
MODALITES <- list(
  statut_logement = c(RENT = "locataire", OWN = "proprietaire",
                      MORTGAGE = "credit_immobilier", OTHER = "autre"),
  motif_pret = c(EDUCATION = "etudes", MEDICAL = "sante", VENTURE = "creation_entreprise",
                 PERSONAL = "personnel", HOMEIMPROVEMENT = "travaux",
                 DEBTCONSOLIDATION = "regroupement_dettes"),
  defaut_anterieur = c(N = "non", Y = "oui"),
  note_risque = c(A = "A", B = "B", C = "C", D = "D", E = "E", F = "F", G = "G")
)

LIBELLES_MODALITES <- list(
  statut_logement = c(locataire = "Locataire", proprietaire = "Propriétaire",
                      credit_immobilier = "Crédit immobilier en cours", autre = "Autre"),
  motif_pret = c(etudes = "Études", sante = "Santé", creation_entreprise = "Création d'entreprise",
                 personnel = "Personnel", travaux = "Travaux", regroupement_dettes = "Regroupement de dettes"),
  defaut_anterieur = c(non = "Non", oui = "Oui"),
  note_risque = setNames(LETTERS[1:7], LETTERS[1:7])
)

LIBELLES_VARIABLES <- c(
  age                  = "Âge",
  revenu_annuel        = "Revenu annuel ($)",
  statut_logement      = "Statut résidentiel",
  anciennete_emploi    = "Ancienneté professionnelle (années)",
  motif_pret           = "Motif du prêt",
  note_risque          = "Note de risque du prêteur",
  montant_pret         = "Montant du prêt ($)",
  taux_interet         = "Taux d'intérêt (%)",
  taux_effort          = "Taux d'effort (montant / revenu)",
  defaut_anterieur     = "Défaut antérieur",
  anciennete_credit    = "Ancienneté de l'historique de crédit (années)",
  taux_manquant        = "Taux d'intérêt non renseigné",
  anciennete_manquante = "Ancienneté professionnelle non renseignée",
  charge_interets      = "Charge d'intérêts annuelle / revenu"
)

VARIABLES_NUMERIQUES <- c("age", "revenu_annuel", "anciennete_emploi", "montant_pret",
                          "taux_interet", "taux_effort", "anciennete_credit")
VARIABLES_QUALITATIVES <- c("statut_logement", "motif_pret", "note_risque", "defaut_anterieur")

# --- 1. Renommage du jeu de données brut ------------------------------------
renommer_donnees_brutes <- function(brut) {
  df <- brut[, names(NOMS_VARIABLES)]
  names(df) <- unname(NOMS_VARIABLES)
  for (v in names(MODALITES)) df[[v]] <- unname(MODALITES[[v]][df[[v]]])
  df$taux_effort_arrondi <- NULL   # arrondi à 2 décimales : on le recalcule exactement plus bas
  df
}

# --- 2. Paramètres appris sur l'échantillon d'entraînement uniquement -------
ajuster_preparation <- function(train) {
  list(
    mediane_taux       = median(train$taux_interet, na.rm = TRUE),
    mediane_anciennete = median(train$anciennete_emploi, na.rm = TRUE)
  )
}

# --- 3. Transformation (entraînement, test et application) ------------------
appliquer_preparation <- function(df, params) {
  # Les valeurs manquantes sont imputées, mais leur absence est conservée comme
  # information : un taux non renseigné n'est pas un hasard dans un dossier de crédit.
  df$taux_manquant        <- as.integer(is.na(df$taux_interet))
  df$anciennete_manquante <- as.integer(is.na(df$anciennete_emploi))
  df$taux_interet[is.na(df$taux_interet)]           <- params$mediane_taux
  df$anciennete_emploi[is.na(df$anciennete_emploi)] <- params$mediane_anciennete

  df$taux_effort     <- df$montant_pret / df$revenu_annuel
  df$charge_interets <- df$taux_effort * df$taux_interet / 100

  for (v in VARIABLES_QUALITATIVES) {
    df[[v]] <- factor(df[[v]], levels = unname(MODALITES[[v]]))
  }
  df
}

# --- 4. Matrices de modélisation à colonnes fixes ---------------------------
# « arbres »   : variables brutes, note de risque en rang (1 à 7) — pour XGBoost.
# « lineaire » : logarithmes des montants et indicatrices — pour les modèles linéaires.
indicatrices <- function(x, nom) {
  niveaux <- levels(x)[-1]   # première modalité = référence
  m <- sapply(niveaux, function(n) as.numeric(x == n))
  m <- matrix(m, nrow = length(x))
  colnames(m) <- paste0(nom, "_", niveaux)
  m
}

matrice_modele <- function(df, type = c("arbres", "lineaire")) {
  type <- match.arg(type)
  if (type == "arbres") {
    base <- cbind(
      as.matrix(df[, VARIABLES_NUMERIQUES]),
      note_risque_rang = as.integer(df$note_risque),
      defaut_anterieur = as.numeric(df$defaut_anterieur == "oui"),
      taux_manquant = df$taux_manquant,
      anciennete_manquante = df$anciennete_manquante
    )
    return(cbind(base, indicatrices(df$statut_logement, "statut_logement"),
                 indicatrices(df$motif_pret, "motif_pret")))
  }
  cbind(
    age = df$age,
    log_revenu = log(df$revenu_annuel),
    anciennete_emploi = pmin(df$anciennete_emploi, 40),
    log_montant = log(df$montant_pret),
    taux_interet = df$taux_interet,
    taux_effort = df$taux_effort,
    anciennete_credit = df$anciennete_credit,
    taux_manquant = df$taux_manquant,
    anciennete_manquante = df$anciennete_manquante,
    defaut_anterieur = as.numeric(df$defaut_anterieur == "oui"),
    indicatrices(df$statut_logement, "statut_logement"),
    indicatrices(df$motif_pret, "motif_pret"),
    indicatrices(df$note_risque, "note_risque")
  )
}

# --- 5. Contrôle d'un dossier saisi dans l'application ------------------------
# Sécurité : Shiny ne vérifie PAS côté serveur qu'une valeur de liste déroulante fait
# partie des choix proposés ; un client modifié peut envoyer n'importe quoi. Tout est donc
# revérifié ici (type, liste blanche, bornes) avant d'atteindre les modèles.
BORNES <- list(
  age = c(18, 100), revenu_annuel = c(1000, 1e7), anciennete_emploi = c(0, 60),
  montant_pret = c(100, 1e6), taux_interet = c(1, 40), anciennete_credit = c(0, 60)
)

verifier_dossier <- function(d) {
  erreurs <- character()
  for (v in VARIABLES_QUALITATIVES) {
    valeur <- d[[v]]
    if (length(valeur) != 1 || !is.character(valeur) || !valeur %in% names(LIBELLES_MODALITES[[v]])) {
      erreurs <- c(erreurs, sprintf("Valeur non autorisée pour « %s ».", LIBELLES_VARIABLES[v]))
    }
  }
  for (v in names(BORNES)) {
    x <- d[[v]]
    b <- BORNES[[v]]
    if (length(x) != 1 || !is.numeric(x) || is.na(x) || !is.finite(x)) {
      erreurs <- c(erreurs, sprintf("« %s » doit être un nombre.", LIBELLES_VARIABLES[v]))
    } else if (x < b[1] || x > b[2]) {
      erreurs <- c(erreurs, sprintf("« %s » doit être compris entre %s et %s.", LIBELLES_VARIABLES[v],
                                    format(b[1], big.mark = " ", scientific = FALSE), format(b[2], big.mark = " ", scientific = FALSE)))
    }
  }
  if (length(erreurs)) return(erreurs)
  if (d$anciennete_emploi > d$age - 14) erreurs <- c(erreurs, "L'ancienneté professionnelle est incompatible avec l'âge.")
  if (d$anciennete_credit > d$age - 16) erreurs <- c(erreurs, "L'historique de crédit est incompatible avec l'âge.")
  if (d$montant_pret > 5 * d$revenu_annuel) erreurs <- c(erreurs, "Le montant du prêt dépasse cinq années de revenu.")
  erreurs
}

# Variables d'un dossier situées hors de l'intervalle observé à l'entraînement :
# la prédiction reste calculée, mais elle repose sur une extrapolation.
hors_domaine <- function(d, plages) {
  v <- names(plages)[vapply(names(plages), function(n) d[[n]] < plages[[n]][1] || d[[n]] > plages[[n]][2], logical(1))]
  unname(LIBELLES_VARIABLES[v])
}

# --- 6. Libellé français d'une colonne de matrice (explications, importances) -
libelle_colonne <- function(colonne) {
  vapply(colonne, function(c) {
    if (c %in% names(LIBELLES_VARIABLES)) return(unname(LIBELLES_VARIABLES[c]))
    if (c == "note_risque_rang") return("Note de risque du prêteur")
    for (v in c("statut_logement", "motif_pret", "note_risque")) {
      prefixe <- paste0(v, "_")
      if (startsWith(c, prefixe)) {
        code <- substring(c, nchar(prefixe) + 1)
        return(paste0(LIBELLES_VARIABLES[v], " : ", LIBELLES_MODALITES[[v]][code]))
      }
    }
    c
  }, character(1), USE.NAMES = FALSE)
}
