library(shiny)
library(shinydashboard)
library(ggplot2)
library(corrplot)
library(DT)
library(caret)
library(rpart)
library(glmnet)
library(naivebayes)

# --- 1. CHARGEMENT DES DONNÉES ---
# Données brutes pour l'exploration (léger, chargé une fois)
donnees <- read.csv("credit_risk_dataset.csv")
donnees$risk_score <- donnees$loan_percent_income * donnees$loan_int_rate
donnees$high_risk_grade <- as.factor(as.integer(donnees$loan_grade %in% c("D", "E", "F", "G")))

# Identification des variables pour les sélecteurs
vars_num <- c("person_age", "person_income", "person_emp_length", "loan_amnt", "loan_int_rate", "loan_percent_income", "cb_person_cred_hist_length", "risk_score")
vars_cat <- c("person_home_ownership", "loan_intent", "loan_grade", "cb_person_default_on_file", "high_risk_grade")

# Transformation de la cible en facteur pour les graphiques bivariés
donnees_visu <- donnees
donnees_visu$loan_status <- as.factor(donnees_visu$loan_status)

# --- 2. INTERFACE UTILISATEUR (UI) ---
ui <- dashboardPage(
  dashboardHeader(title = "Risque de Crédit"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Aperçu des données", tabName = "apercu", icon = icon("table")),
      menuItem("Analyse univariée", tabName = "univarie", icon = icon("chart-bar")),
      menuItem("Analyse bivariée", tabName = "bivarie", icon = icon("chart-line")),
      menuItem("Prédiction", tabName = "prediction", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # --- Onglet 1 : Aperçu ---
      tabItem(tabName = "apercu",
              fluidRow(
                box(title = "Jeu de données (50 premières lignes)", status = "primary", width = 12, solidHeader = TRUE,
                    DTOutput("table_donnees")),
                box(title = "Résumé statistique", status = "info", width = 6, solidHeader = TRUE,
                    verbatimTextOutput("resume_stat")),
                box(title = "Valeurs manquantes (NA)", status = "warning", width = 6, solidHeader = TRUE,
                    tableOutput("table_na"))
              ),
              fluidRow(
                box(title = "Répartition de la cible (loan_status)", status = "danger", width = 12, solidHeader = TRUE,
                    splitLayout(cellWidths = c("30%", "70%"), tableOutput("table_cible"), plotOutput("plot_cible")))
              )
      ),
      
      # --- Onglet 2 : Univariée ---
      tabItem(tabName = "univarie",
              fluidRow(
                box(title = "Variables numériques", status = "primary", width = 6, solidHeader = TRUE,
                    selectInput("var_num", "Sélectionner une variable :", choices = vars_num),
                    plotOutput("hist_num"),
                    plotOutput("box_num")),
                box(title = "Variables catégorielles", status = "success", width = 6, solidHeader = TRUE,
                    selectInput("var_cat", "Sélectionner une variable :", choices = vars_cat),
                    plotOutput("bar_cat"))
              )
      ),
      
      # --- Onglet 3 : Bivariée ---
      tabItem(tabName = "bivarie",
              fluidRow(
                box(title = "Variables Numériques vs Défaut", status = "primary", width = 6, solidHeader = TRUE,
                    selectInput("var_num_bi", "Sélectionner une variable numérique :", choices = vars_num),
                    plotOutput("box_bi")),
                box(title = "Variables Catégorielles vs Défaut", status = "success", width = 6, solidHeader = TRUE,
                    selectInput("var_cat_bi", "Sélectionner une variable catégorielle :", choices = vars_cat),
                    plotOutput("bar_bi"))
              ),
              fluidRow(
                box(title = "Matrice de corrélation (Variables numériques)", status = "warning", width = 12, solidHeader = TRUE,
                    plotOutput("corr_plot"))
              )
      ),
      
      # --- Onglet 4 : Prédiction (Code Existant Migré) ---
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Informations de l'emprunteur", status = "primary", width = 4, solidHeader = TRUE,
                    numericInput("person_age", "Âge :", value = 30, min = 18, max = 100),
                    numericInput("person_income", "Revenu annuel (USD) :", value = 50000, min = 1000),
                    selectInput("person_home_ownership", "Statut résidentiel :", choices = c("RENT", "MORTGAGE", "OWN", "OTHER")),
                    numericInput("person_emp_length", "Ancienneté pro (années) :", value = 5, min = 0),
                    selectInput("loan_intent", "Motif du prêt :", choices = c("PERSONAL", "EDUCATION", "MEDICAL", "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")),
                    selectInput("loan_grade", "Note de risque (Grade) :", choices = c("A", "B", "C", "D", "E", "F", "G")),
                    numericInput("loan_amnt", "Montant du prêt (USD) :", value = 10000, min = 500),
                    numericInput("loan_int_rate", "Taux d'intérêt (%) :", value = 10.5, min = 1, step = 0.1),
                    selectInput("cb_person_default_on_file", "Historique de défaut (Y/N) :", choices = c("N", "Y")),
                    numericInput("cb_person_cred_hist_length", "Durée historique crédit (années) :", value = 3, min = 0),
                    hr(),
                    actionButton("predict_btn", "Lancer la prédiction", class = "btn-primary", width = "100%")
                ),
                column(width = 8,
                       box(title = "Classification Bayésienne Naïve", status = "info", width = 12, solidHeader = TRUE,
                           h2(textOutput("pred_class_nb")),
                           h4("Probabilité de défaut :"),
                           h3(textOutput("pred_prob_nb"), style = "color: steelblue;")
                       ),
                       box(title = "Régression Logistique", status = "success", width = 12, solidHeader = TRUE,
                           h2(textOutput("pred_class_logit")),
                           h4("Probabilité de défaut :"),
                           h3(textOutput("pred_prob_logit"), style = "color: forestgreen;")
                       )
                )
              )
      )
    )
  )
)

# --- 3. SERVEUR ---
server <- function(input, output) {
  
  # --- Onglet 1 : Aperçu ---
  output$table_donnees <- renderDT({
    datatable(head(donnees, 50), options = list(scrollX = TRUE, pageLength = 5))
  })
  output$resume_stat <- renderPrint({ summary(donnees) })
  output$table_na <- renderTable({
    na_count <- colSums(is.na(donnees))
    data.frame(Variable = names(na_count), Valeurs_Manquantes = na_count)
  }, rownames = FALSE)
  output$table_cible <- renderTable({
    tbl <- table(donnees$loan_status)
    prop <- prop.table(tbl) * 100
    data.frame(Statut = names(tbl), Effectif = as.numeric(tbl), Pourcentage = paste0(round(as.numeric(prop), 2), "%"))
  })
  output$plot_cible <- renderPlot({
    ggplot(donnees_visu, aes(x = loan_status, fill = loan_status)) +
      geom_bar() +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = "Status (0 = Remboursé, 1 = Défaut)", y = "Count") +
      scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"))
  })
  
  # --- Onglet 2 : Univariée ---
  output$hist_num <- renderPlot({
    ggplot(donnees, aes_string(x = input$var_num)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = paste("Histogramme :", input$var_num), y = "Fréquence")
  })
  output$box_num <- renderPlot({
    ggplot(donnees, aes_string(y = input$var_num)) +
      geom_boxplot(fill = "lightblue") +
      theme_minimal() +
      labs(title = paste("Boxplot :", input$var_num))
  })
  output$bar_cat <- renderPlot({
    ggplot(donnees, aes_string(x = input$var_cat)) +
      geom_bar(fill = "orange", color = "black") +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Effectifs :", input$var_cat), y = "Compte")
  })
  
  # --- Onglet 3 : Bivariée ---
  output$box_bi <- renderPlot({
    ggplot(donnees_visu, aes_string(x = "loan_status", y = input$var_num_bi, fill = "loan_status")) +
      geom_boxplot() +
      theme_minimal() +
      scale_fill_manual(values = c("0" = "lightblue", "1" = "lightpink")) +
      labs(title = paste(input$var_num_bi, "selon le défaut"), x = "Défaut (0/1)")
  })
  output$bar_bi <- renderPlot({
    ggplot(donnees_visu, aes_string(x = input$var_cat_bi, fill = "loan_status")) +
      geom_bar(position = "fill") +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato")) +
      labs(title = paste("Proportion de défaut par", input$var_cat_bi), y = "Proportion")
  })
  output$corr_plot <- renderPlot({
    donnees_num <- donnees[, vars_num]
    # On enlève les NAs pour la corrélation
    cor_mat <- cor(donnees_num, use = "complete.obs")
    corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex = 0.7)
  })
  
  # --- Onglet 4 : Prédiction ---
  observeEvent(input$predict_btn, {
    # Chargement des modèles uniquement à la demande (économie mémoire)
    modele_nb    <- readRDS("modele_nb.rds")
    modele_logit <- readRDS("modele_logit.rds")

    nouvelle_donnee <- data.frame(
      person_age = input$person_age,
      person_income = input$person_income,
      person_home_ownership = factor(input$person_home_ownership, levels = c("MORTGAGE", "OTHER", "OWN", "RENT")),
      person_emp_length = input$person_emp_length,
      loan_intent = factor(input$loan_intent, levels = c("DEBTCONSOLIDATION", "EDUCATION", "HOMEIMPROVEMENT", "MEDICAL", "PERSONAL", "VENTURE")),
      loan_grade = factor(input$loan_grade, levels = c("A", "B", "C", "D", "E", "F", "G")),
      loan_amnt = input$loan_amnt,
      loan_int_rate = input$loan_int_rate,
      loan_percent_income = round(input$loan_amnt / input$person_income, 2),
      cb_person_default_on_file = factor(input$cb_person_default_on_file, levels = c("N", "Y")),
      cb_person_cred_hist_length = input$cb_person_cred_hist_length,
      risk_score = round(input$loan_amnt / input$person_income, 2) * input$loan_int_rate,
      high_risk_grade = as.integer(input$loan_grade %in% c("D", "E", "F", "G"))
    )
    
    # Bayésienne Naïve
    prob_nb <- predict(modele_nb, newdata = nouvelle_donnee, type = "prob")[, "Defaut"]
    class_nb <- ifelse(prob_nb > 0.5, "DÉFAUT À RISQUE", "REMBOURSEMENT SÛR")
    output$pred_prob_nb <- renderText({ paste0(round(prob_nb * 100, 2), " %") })
    output$pred_class_nb <- renderText({ class_nb })
    
    # Régression Logistique
    prob_logit <- predict(modele_logit, newdata = nouvelle_donnee, type = "prob")[, "Defaut"]
    class_logit <- ifelse(prob_logit > 0.5, "DÉFAUT À RISQUE", "REMBOURSEMENT SÛR")
    output$pred_prob_logit <- renderText({ paste0(round(prob_logit * 100, 2), " %") })
    output$pred_class_logit <- renderText({ class_logit })
  })
}

# --- 4. LANCEMENT ---
shinyApp(ui = ui, server = server)