# =============================================================================
# Application « Scoring de risque de crédit » — Mohamed Fofana & Abdou Fall
# Tout est chargé une seule fois au démarrage : modèles allégés (moins de 1 Mo)
# et résultats pré-calculés. Aucun calcul lourd n'est fait pendant la navigation.
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(xgboost); library(naivebayes)
})
# Sécurité : en cas d'erreur, l'utilisateur voit un message générique, jamais le détail technique.
options(shiny.sanitize.errors = TRUE)
source("preparation.R", encoding = "UTF-8")
source("modeles_application.R", encoding = "UTF-8")

MODELES <- readRDS("modeles/modeles.rds")
MODELES$xgboost <- xgb.load("modeles/xgboost.ubj")
# Un seul fil de calcul : un dossier se prédit en moins d'une milliseconde, et sur un serveur
# partagé (fraction de processeur), plusieurs fils se disputeraient le quota et ralentiraient tout.
xgb.model.parameters(MODELES$xgboost) <- list(nthread = 1)
EVAL <- readRDS("modeles/evaluation.rds")

BLEU <- "#2563eb"; VIOLET <- "#7c3aed"; ROUGE <- "#dc2626"; VERT <- "#16a34a"
pct <- function(x, d = 1) {
  borne <- 10^-(d + 2)   # aucun modèle n'est certain : on affiche « < 0,1 % » plutôt que « 0,0 % »
  ifelse(x < borne, paste0("< ", formatC(100 * borne, format = "f", digits = d, decimal.mark = ","), " %"),
  ifelse(x > 1 - borne, paste0("> ", formatC(100 * (1 - borne), format = "f", digits = d, decimal.mark = ","), " %"),
         paste0(formatC(100 * x, format = "f", digits = d, decimal.mark = ","), " %")))
}
nombre <- function(x, d = 3) formatC(x, format = "f", digits = d, decimal.mark = ",")
choix <- function(v) setNames(names(LIBELLES_MODALITES[[v]]), unname(LIBELLES_MODALITES[[v]]))

# Images dessinées à l'export (R/04_export_application.R) : rien à calculer pendant la visite.
image_figure <- function(fichier, texte) tags$img(src = file.path("figures", fichier), alt = texte, loading = "lazy",
                                                  style = "width: 100%; height: auto;")

# Graphique d'explication en HTML pur : instantané, net sur tous les écrans, sans moteur graphique.
barres_explication <- function(e) {
  maxi <- max(abs(e$contribution), 1e-9)
  tags$div(class = "barres", lapply(seq_len(nrow(e)), function(i) {
    c <- e$contribution[i]
    largeur <- sprintf("%.1f%%", 100 * abs(c) / maxi)
    barre <- tags$span(class = paste("barre", if (c > 0) "hausse" else "baisse"), style = paste0("width:", largeur),
                       title = sprintf("%+.2f (log-odds)", c))
    tags$div(class = "ligne-barre",
      tags$span(class = "libelle-barre", e$libelle[i]),
      tags$span(class = "moitie gauche", if (c < 0) barre),
      tags$span(class = "moitie droite", if (c > 0) barre))
  }))
}

theme_appli <- bs_theme(version = 5, primary = BLEU, secondary = VIOLET,
                        base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"))

carte_kpi <- function(titre, sortie, sous_titre = NULL) {
  card(class = "text-center", card_body(
    tags$div(class = "text-muted small text-uppercase fw-semibold", titre),
    tags$div(class = "fs-2 fw-bold", textOutput(sortie, inline = TRUE)),
    if (!is.null(sous_titre)) tags$div(class = "text-muted small", sous_titre)
  ))
}

# =============================================================================
# Interface
# =============================================================================
ui <- page_navbar(
  title = "Scoring de risque de crédit",
  theme = theme_appli,
  lang = "fr",
  fillable = FALSE,
  window_title = "Scoring de risque de crédit — Mohamed Fofana",
  header = tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"), tags$style(HTML("
    .decision { border-radius: 14px; padding: 1.2rem 1.4rem; color: #fff; }
    .decision h2 { margin: 0; font-weight: 700; }
    .decision.refus { background: linear-gradient(135deg, #dc2626, #b91c1c); }
    .decision.accord { background: linear-gradient(135deg, #16a34a, #15803d); }
    .grille-decision { display: grid; grid-template-columns: 2fr 1fr 1fr; gap: 1rem; align-items: start; }
    @media (max-width: 767.98px) { .grille-decision { grid-template-columns: 1fr; } }
    .table td, .table th { vertical-align: middle; }
    .barres { display: flex; flex-direction: column; gap: 0.55rem; padding: 0.4rem 0; }
    .ligne-barre { display: grid; grid-template-columns: minmax(8rem, 38%) 1fr 1fr; align-items: center; gap: 0; font-size: 0.9rem; }
    .libelle-barre { padding-right: 0.8rem; text-align: right; line-height: 1.2; }
    .moitie { display: flex; height: 1.15rem; }
    .moitie.gauche { justify-content: flex-end; border-right: 2px solid #94a3b8; }
    .barre { display: block; height: 100%; border-radius: 3px; }
    .barre.hausse { background: #dc2626; }
    .barre.baisse { background: #16a34a; }
  "))),

  nav_panel("Simulateur", icon = icon("calculator"),
    layout_sidebar(
      # Sur téléphone, le formulaire s'affiche au-dessus des résultats au lieu d'être replié.
      sidebar = sidebar(width = 340, title = "Dossier de l'emprunteur", open = list(desktop = "open", mobile = "always-above"),
        numericInput("age", "Âge", 30, min = 18, max = 100),
        numericInput("revenu_annuel", "Revenu annuel ($)", 50000, min = 1000, max = 1e7, step = 1000),
        selectInput("statut_logement", "Statut résidentiel", choix("statut_logement"), selected = "locataire"),
        numericInput("anciennete_emploi", "Ancienneté professionnelle (années)", 5, min = 0, max = 60),
        numericInput("anciennete_credit", "Ancienneté de l'historique de crédit (années)", 4, min = 0, max = 60),
        selectInput("defaut_anterieur", "Défaut de paiement antérieur", choix("defaut_anterieur")),
        tags$hr(),
        numericInput("montant_pret", "Montant du prêt ($)", 10000, min = 100, max = 1e6, step = 500),
        selectInput("motif_pret", "Motif du prêt", choix("motif_pret"), selected = "personnel"),
        numericInput("taux_interet", "Taux d'intérêt (%)", 11, min = 1, max = 40, step = 0.1),
        selectInput("note_risque", "Note de risque attribuée par le prêteur", choix("note_risque"), selected = "B"),
        tags$small(class = "text-muted", "A = meilleure note, G = la plus risquée.")
      ),
      uiOutput("erreurs"),
      uiOutput("bloc_decision"),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Pourquoi cette décision ?"),
             uiOutput("explication"),
             card_footer(tags$small(class = "text-muted",
               "Contribution de chaque caractéristique à la probabilité de défaut estimée par XGBoost (valeurs de Shapley). ",
               "En rouge : ce qui augmente le risque ; en vert : ce qui le réduit."))),
        card(card_header("Grille de score bancaire"),
             uiOutput("score_points"),
             tableOutput("detail_grille"))
      ),
      card(card_header("Avis des cinq modèles"), tableOutput("avis_modeles"),
           card_footer(tags$small(class = "text-muted", textOutput("note_seuils", inline = TRUE))))
    )
  ),

  nav_panel("Comparaison des modèles", icon = icon("chart-line"),
    layout_columns(col_widths = c(3, 3, 3, 3),
      carte_kpi("Modèle retenu", "kpi_modele", "avec contraintes de cohérence"),
      carte_kpi("AUC sur le test", "kpi_auc", "jeu jamais vu pendant l'entraînement"),
      carte_kpi("Défauts détectés", "kpi_rappel", "au seuil optimal"),
      carte_kpi("Gain vs version initiale", "kpi_gain", "réduction du coût d'erreur")
    ),
    card(card_header("Performances sur le jeu de test"), tableOutput("tableau_test"),
         card_footer(tags$small(class = "text-muted", uiOutput("glossaire", inline = TRUE)))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Courbes ROC"), image_figure("roc.png", "Courbes ROC des modèles sur le jeu de test")),
      card(card_header("Calibration des probabilités"), image_figure("calibration.png", "Calibration : probabilité prédite contre taux de défaut observé"))
    ),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Variables les plus influentes (XGBoost)"), image_figure("importance.png", "Importance des variables selon les valeurs de Shapley")),
      card(card_header("La note du prêteur est-elle indispensable ?"), uiOutput("sensibilite"))
    ),
    card(card_header("Cohérence des décisions : pourquoi le modèle retenu n'est pas celui à la plus haute AUC"),
         tableOutput("coherence"),
         card_footer(tags$small(class = "text-muted",
           "Part des dossiers de test dont le risque évolue à contresens (de plus d'un point) quand on modifie une seule caractéristique. ",
           "Sans contrainte, XGBoost gagne quelques points d'AUC mais peut, par exemple, juger un emprunteur plus risqué quand son revenu augmente : ",
           "un tel modèle n'est ni crédible ni défendable pour une décision de crédit.")))
  ),

  nav_panel("Exploration des données", icon = icon("magnifying-glass-chart"),
    layout_columns(col_widths = c(3, 3, 3, 3),
      carte_kpi("Dossiers", "kpi_dossiers", "après nettoyage"),
      carte_kpi("Taux de défaut", "kpi_taux"),
      carte_kpi("Variables", "kpi_variables", "explicatives"),
      carte_kpi("Doublons retirés", "kpi_doublons")
    ),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Variable numérique selon le statut du prêt"),
           selectInput("var_num", NULL, setNames(VARIABLES_NUMERIQUES, LIBELLES_VARIABLES[VARIABLES_NUMERIQUES])),
           uiOutput("graphe_num")),
      card(card_header("Taux de défaut par modalité"),
           selectInput("var_qual", NULL, setNames(VARIABLES_QUALITATIVES, LIBELLES_VARIABLES[VARIABLES_QUALITATIVES])),
           uiOutput("graphe_qual"))
    )
  ),

  nav_panel("Méthode", icon = icon("book"),
    card(card_body(class = "fs-6", uiOutput("methode")))
  ),

  nav_spacer(),
  nav_item(tags$a(icon("github"), "Code source", href = "https://github.com/Mohamed-Fof/Credit-Risk", target = "_blank", rel = "noopener noreferrer"))
)

# Performance : construire cette page coûte environ 0,2 s (compilation du thème Sass), soit
# près de 3 s sur le serveur gratuit de Render. Elle est donc rendue UNE fois au démarrage,
# puis resservie telle quelle (environ 2 ms par visite).
figer_page <- function(page, theme) {
  # Hors d'une visite, Shiny ignore le thème actif et produirait une navigation au format
  # Bootstrap 3 : on le déclare, comme le ferait bslib au premier affichage.
  shiny::shinyOptions(bootstrapTheme = theme)
  rendu <- htmltools::renderTags(page)
  corps <- regmatches(rendu$html, regexec("^\\s*<body([^>]*)>(.*)</body>\\s*$", rendu$html))[[1]]
  stopifnot(length(corps) == 3)
  classe <- sub('.*class="([^"]*)".*', "\\1", corps[2])
  fige <- htmltools::attachDependencies(
    htmltools::tagList(htmltools::tags$head(htmltools::HTML(rendu$head)),
                       htmltools::tags$body(class = classe, htmltools::HTML(corps[3]))),
    rendu$dependencies)
  attr(fige, "lang") <- attr(page, "lang")
  fige
}
ui <- figer_page(ui, theme_appli)

# =============================================================================
# Serveur
# =============================================================================
server <- function(input, output, session) {

  # Le dossier reste une simple liste tant qu'il n'est pas vérifié : une valeur vide ou
  # forgée par un client modifié ne peut pas faire planter sa mise en forme.
  dossier <- reactive({
    list(
      age = input$age, revenu_annuel = input$revenu_annuel, statut_logement = input$statut_logement,
      anciennete_emploi = input$anciennete_emploi, motif_pret = input$motif_pret, note_risque = input$note_risque,
      montant_pret = input$montant_pret, taux_interet = input$taux_interet,
      defaut_anterieur = input$defaut_anterieur, anciennete_credit = input$anciennete_credit
    )
  }) |> debounce(300)

  erreurs <- reactive(verifier_dossier(dossier()))
  valide <- reactive(length(erreurs()) == 0)
  dossier_valide <- reactive({ req(valide()); as.data.frame(dossier(), stringsAsFactors = FALSE) })

  output$erreurs <- renderUI({
    if (!valide()) {
      return(div(class = "alert alert-warning", tags$strong("Dossier incomplet ou incohérent :"), tags$ul(lapply(erreurs(), tags$li))))
    }
    hors <- hors_domaine(dossier(), MODELES$plages)
    if (length(hors)) {
      div(class = "alert alert-info", tags$strong("Extrapolation : "),
          sprintf("%s en dehors des valeurs observées dans les données d'entraînement. La prédiction est donnée à titre indicatif.",
                  paste(hors, collapse = ", ")))
    }
  })

  probabilites <- reactive(predire_tous(dossier_valide(), MODELES))

  output$bloc_decision <- renderUI({
    p <- probabilites()[["XGBoost"]]
    s <- MODELES$seuils[["XGBoost"]]
    refus <- p >= s
    # Grille CSS simple plutôt que layout_columns : un composant bslib dans un élément redessiné
    # recompile ses styles Sass et recopie ses fichiers à CHAQUE saisie (mesuré au profileur).
    div(class = paste("decision mb-3", if (refus) "refus" else "accord"),
      div(class = "grille-decision",
        div(tags$div(class = "small text-uppercase opacity-75", "Décision recommandée"),
            h2(if (refus) "Prêt déconseillé" else "Prêt envisageable"),
            tags$div(class = "small opacity-75", sprintf("Seuil de refus : %s de probabilité de défaut", pct(s)))),
        div(tags$div(class = "small text-uppercase opacity-75", "Probabilité de défaut"), h2(pct(p))),
        div(tags$div(class = "small text-uppercase opacity-75", "Modèle"), h2("XGBoost"),
            tags$div(class = "small opacity-75", "le plus performant sur le test"))
      ))
  })

  output$explication <- renderUI({
    e <- expliquer(dossier_valide(), MODELES)
    barres_explication(head(e[order(-abs(e$contribution)), ], 8))
  })

  output$score_points <- renderUI({
    points <- points_grille(appliquer_preparation(dossier_valide(), MODELES$params), MODELES$grille_score)
    p <- probabilite_depuis_points(points)
    tagList(
      tags$div(class = "fs-2 fw-bold text-nowrap", paste(round(points), "points")),
      tags$div(class = "text-muted mb-2", sprintf("soit %s de risque · avis : %s", pct(p),
                                                  if (p >= MODELES$seuils[["Grille de score"]]) "refus" else "accord")),
      tags$p(class = "small text-muted", "Méthode des banques : chaque caractéristique rapporte ou retire des points. ",
             "600 points correspondent à 5 % de risque ; 50 points de plus divisent la cote de risque par deux. ",
             "C'est un modèle distinct de XGBoost, plus simple et entièrement lisible.")
    )
  })

  output$detail_grille <- renderTable({
    x <- appliquer_preparation(dossier_valide(), MODELES$params)
    d <- detail_points(x[1, , drop = FALSE], MODELES$grille_score)
    d <- d[order(-abs(d$points)), ]
    data.frame(`Caractéristique` = unname(LIBELLES_VARIABLES[d$variable]), Points = sprintf("%+d", as.integer(d$points)), check.names = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%")

  output$avis_modeles <- renderTable({
    p <- probabilites()
    data.frame(
      `Modèle` = names(p),
      `Probabilité de défaut` = vapply(p, pct, character(1)),
      `Seuil du modèle` = vapply(names(p), function(n) pct(MODELES$seuils[[n]]), character(1)),
      `Avis` = ifelse(unlist(p) >= MODELES$seuils[names(p)], "Refus", "Accord"),
      check.names = FALSE
    )
  }, striped = TRUE, width = "100%")

  output$note_seuils <- renderText(sprintf(
    "Chaque seuil minimise le coût attendu en validation croisée, avec l'hypothèse qu'accorder un prêt à un futur défaillant coûte %d fois plus que refuser un bon client.",
    EVAL$ratio_cout))

  # --- Comparaison ---------------------------------------------------------------
  t_test <- EVAL$test
  meilleur <- t_test[t_test$Modele == EVAL$meilleur, ]
  v1 <- t_test[t_test$Modele == "Logistique v1 (soutenance)", ]
  output$kpi_modele <- renderText(EVAL$meilleur)
  output$kpi_auc <- renderText(nombre(meilleur$AUC))
  output$kpi_rappel <- renderText(pct(meilleur$Rappel, 0))
  output$kpi_gain <- renderText(paste0("−", pct(1 - meilleur$Cout / v1$Cout, 0)))

  output$tableau_test <- renderTable({
    t <- t_test[order(-t_test$AUC), ]
    data.frame(
      `Modèle` = t$Modele, AUC = nombre(t$AUC), `IC 95 %` = gsub("\\.", ",", t$AUC_IC95), Gini = nombre(t$Gini), KS = nombre(t$KS),
      Brier = nombre(t$Brier, 4), Seuil = pct(t$Seuil), `Défauts détectés` = pct(t$Rappel), `Refus justifiés` = pct(t$Precision),
      `Bons clients acceptés` = pct(t$Specificite), `Coût moyen` = nombre(t$Cout), check.names = FALSE)
  }, striped = TRUE, width = "100%")

  output$glossaire <- renderUI(HTML(paste(
    "<b>AUC</b> : probabilité qu'un futur défaillant reçoive un score plus risqué qu'un bon payeur (0,5 = hasard, 1 = parfait).",
    "<b>Gini</b> = 2 × AUC − 1, l'indicateur usuel des banques. <b>KS</b> : écart maximal entre les deux populations.",
    "<b>Brier</b> : erreur quadratique des probabilités (plus bas = mieux calibré).",
    sprintf("Jeu de test : %s dossiers, jamais utilisés pour entraîner ni régler les modèles.", format(EVAL$n_test, big.mark = " ")))))

  output$coherence <- renderTable({
    co <- EVAL$coherence
    auc_de <- setNames(t_test$AUC, t_test$Modele)
    tableau <- data.frame(`Modèle` = co$Modele, AUC = nombre(auc_de[co$Modele]), check.names = FALSE)
    for (scenario in names(co)[-1]) tableau[[scenario]] <- ifelse(co[[scenario]] == 0, "0 %", pct(co[[scenario]]))
    tableau
  }, striped = TRUE, width = "100%")

  output$sensibilite <- renderUI({
    s <- EVAL$sans_note
    tagList(
      tags$p(sprintf("AUC de XGBoost avec la note du prêteur : %s", nombre(s$AUC_test[1]))),
      tags$p(sprintf("AUC sans cette note : %s", nombre(s$AUC_test[2]))),
      tags$p(class = "text-muted", "La note A à G est attribuée par le prêteur lui-même : c'est déjà une évaluation du risque. ",
             "Mesurer le modèle sans elle montre ce qu'il apprend réellement des caractéristiques de l'emprunteur et du prêt.")
    )
  })

  # --- Exploration -----------------------------------------------------------------
  output$kpi_dossiers <- renderText(format(EVAL$n_dossiers, big.mark = " "))
  output$kpi_taux <- renderText(pct(EVAL$taux_defaut))
  output$kpi_variables <- renderText(length(c(VARIABLES_NUMERIQUES, VARIABLES_QUALITATIVES)) - 1)
  output$kpi_doublons <- renderText(EVAL$journal$lignes[1] - EVAL$journal$lignes[2])

  # Choix vérifié contre la liste des variables : un client modifié ne peut pas viser un autre fichier.
  output$graphe_num <- renderUI({
    v <- input$var_num
    if (length(v) != 1 || !v %in% VARIABLES_NUMERIQUES) return(NULL)
    image_figure(paste0("exploration_", v, ".png"), paste("Distribution de", LIBELLES_VARIABLES[v], "selon le statut du prêt"))
  })

  output$graphe_qual <- renderUI({
    v <- input$var_qual
    if (length(v) != 1 || !v %in% VARIABLES_QUALITATIVES) return(NULL)
    image_figure(paste0("exploration_", v, ".png"), paste("Taux de défaut selon", LIBELLES_VARIABLES[v]))
  })

  # --- Méthode ---------------------------------------------------------------------
  output$methode <- renderUI({
    j <- EVAL$journal
    tagList(
      h4("Données"),
      tags$p("Jeu public « Credit Risk Dataset » (Kaggle) : prêts à la consommation, montants en dollars, défaut = 21,9 % des dossiers."),
      tags$ul(lapply(seq_len(nrow(j)), function(i) tags$li(sprintf("%s : %s dossiers", j$etape[i], format(j$lignes[i], big.mark = " "))))),
      h4("Protocole"),
      tags$ol(
        tags$li("Découpage stratifié 80 / 20 avant toute transformation : le jeu de test n'est utilisé qu'une fois, à la fin."),
        tags$li("Valeurs manquantes (taux d'intérêt, ancienneté) remplacées par la médiane de l'échantillon d'apprentissage, avec un indicateur « non renseigné » conservé comme information."),
        tags$li("Six modèles comparés avec les mêmes 5 plis de validation croisée ; préparation réapprise dans chaque pli."),
        tags$li("Hyperparamètres réglés par validation croisée : densités par noyau (Naïve Bayes), alpha et lambda (elastic net), profondeur, poids minimal et arrêt précoce (XGBoost)."),
        tags$li(sprintf("Seuil de décision choisi pour minimiser le coût attendu (un défaut accordé coûte %d fois un refus à tort), sur les prédictions hors pli.", EVAL$ratio_cout)),
        tags$li(local({
          r <- EVAL$reglage_xgboost
          sprintf("XGBoost est entraîné avec des contraintes de monotonie : toutes choses égales par ailleurs, le risque ne peut que croître avec le montant, le taux d'effort, le taux d'intérêt, la note et un défaut antérieur, et baisser quand le revenu augmente. Elles coûtent un peu de performance (AUC en validation croisée de %s contre %s sans), mais garantissent des décisions cohérentes et défendables.",
                  nombre(max(r$auc_cv[r$monotone])), nombre(max(r$auc_cv[!r$monotone])))
        })),
        tags$li("Le jeu de données contient des règles quasi déterministes (par exemple, 100 % de défaut chez les locataires notés A ou B au-delà de 32 % de taux d'effort) : il est en partie simulé. Les performances obtenues ici ne se transposeraient pas telles quelles à un portefeuille réel.")
      ),
      h4("Limites"),
      tags$ul(
        tags$li("Données publiques sans date : impossible de vérifier la stabilité du modèle dans le temps."),
        tags$li("Le ratio de coût 5 pour 1 est une hypothèse ; une banque le calibrerait sur ses pertes réelles."),
        tags$li("Outil pédagogique : il ne constitue pas une décision de crédit.")
      ),
      h4("Auteurs"),
      tags$p("Mohamed Fofana et Abdou Fall — projet de Compléments de mathématiques, L3 MIASHS, Université Grenoble Alpes, amélioré en 2026.")
    )
  })
}

shinyApp(ui, server)
