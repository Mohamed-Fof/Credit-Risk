knitr::opts_chunk$set(
  echo    = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  out.width = "80%"
)

library(tidyverse)
library(ggplot2)
library(caret)
library(naivebayes)
library(pROC)
library(knitr)
library(kableExtra)
library(corrplot)

data <- read.csv("credit_risk_dataset.csv")

head(data)
dim(data)
str(data)
summary(data)

# Personnes avec un âge < 18 ans
age_moins_18 <- data[data$person_age < 18, "person_age"]
cat("Nombre de personnes avec un âge < 18 ans :", length(age_moins_18), "\n")
if (length(age_moins_18) > 0) print(age_moins_18)

# Personnes avec un âge > 100 ans
age_plus_100 <- data[data$person_age > 100, "person_age"]
cat("Nombre de personnes avec un âge > 100 ans :", length(age_plus_100), "\n")
if (length(age_plus_100) > 0) print(age_plus_100)

na_counts <- colSums(is.na(data))
na_df <- data.frame(
  Variable       = names(na_counts),
  Manquants      = as.integer(na_counts),
  Pourcentage    = round(100 * na_counts / nrow(data), 2)
)
na_df <- na_df[na_df$Manquants > 0, ]
kable(na_df, row.names = FALSE,
      caption = "Variables présentant des valeurs manquantes") %>%
  kable_styling(latex_options = "HOLD_position")

freq_cible <- prop.table(table(data$loan_status))
cat("Distribution de loan_status :\n")
print(round(freq_cible * 100, 1))

ggplot(data, aes(x = factor(loan_status,
                             labels = c("Non-défaut (0)", "Défaut (1)")))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))),
           fill = c("steelblue", "tomato"), width = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution de la variable cible loan_status",
       x = NULL, y = "Proportion") +
  theme_minimal(base_size = 12)

vars_quant <- c("person_age", "person_income", "person_emp_length",
                "loan_amnt", "loan_int_rate", "loan_percent_income",
                "cb_person_cred_hist_length")

# Statistiques descriptives
stats_quant <- data %>%
  select(all_of(vars_quant)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") %>%
  group_by(Variable) %>%
  summarise(
    n       = sum(!is.na(Valeur)),
    Moyenne = round(mean(Valeur, na.rm = TRUE), 2),
    Médiane = round(median(Valeur, na.rm = TRUE), 2),
    Éc.type = round(sd(Valeur, na.rm = TRUE), 2),
    Min     = round(min(Valeur, na.rm = TRUE), 2),
    Max     = round(max(Valeur, na.rm = TRUE), 2)
  )
kable(stats_quant, caption = "Statistiques descriptives des variables quantitatives") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"))

# Histogrammes
data %>%
  select(all_of(vars_quant)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") %>%
  ggplot(aes(x = Valeur)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  labs(title = "Distribution des variables quantitatives", x = NULL, y = "Effectif") +
  theme_minimal(base_size = 10)

vars_qual <- c("person_home_ownership", "loan_intent",
               "loan_grade", "cb_person_default_on_file")

# Tableau de fréquences pour chaque variable
for (v in vars_qual) {
  freq_v <- sort(table(data[[v]]), decreasing = TRUE)
  df_v   <- data.frame(
    Modalité    = names(freq_v),
    Effectif    = as.integer(freq_v),
    Fréquence   = paste0(round(100 * freq_v / sum(freq_v), 1), " %")
  )
  print(kable(df_v, row.names = FALSE,
              caption = paste("Répartition de", gsub("_", " ", v))) %>%
          kable_styling(latex_options = "HOLD_position"))
  cat("\n\n")
}

# Diagrammes en barres
data %>%
  select(all_of(vars_qual)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Modalité") %>%
  ggplot(aes(x = Modalité)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  labs(title = "Distribution des variables qualitatives", x = NULL, y = "Effectif") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

cor_mat <- cor(data %>% select(all_of(vars_quant)), use = "complete.obs")
corrplot(cor_mat, method = "color", type = "upper",
         tl.cex = 0.8, addCoef.col = "black", number.cex = 0.65,
         title = "Matrice de corrélation des variables quantitatives",
         mar = c(0, 0, 1, 0))

data %>%
  select(all_of(vars_quant), loan_status) %>%
  mutate(loan_status = factor(loan_status,
                              labels = c("Non-défaut", "Défaut"))) %>%
  pivot_longer(-loan_status, names_to = "Variable", values_to = "Valeur") %>%
  ggplot(aes(x = loan_status, y = Valeur, fill = loan_status)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(title = "Variables quantitatives selon le statut du prêt",
       x = NULL, y = NULL, fill = "Statut") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

data %>%
  select(all_of(vars_qual), loan_status) %>%
  mutate(loan_status = factor(loan_status,
                              labels = c("Non-défaut", "Défaut"))) %>%
  pivot_longer(-loan_status, names_to = "Variable", values_to = "Modalité") %>%
  ggplot(aes(x = Modalité, fill = loan_status)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 2) +
  labs(title = "Proportion de défauts par modalité des variables qualitatives",
       x = NULL, y = "Proportion", fill = "Statut") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

set.seed(42)

# -- Étape 1 : nettoyage des âges aberrants ------------------------------------
n_avant <- nrow(data)
data <- data[data$person_age <= 100, ]
cat("Observations supprimées (âge > 100) :", n_avant - nrow(data), "\n")
cat("Taille du jeu de données après nettoyage :", nrow(data), "\n")

# -- Étape 2 : conversion de la variable cible et des variables qualitatives ---
data$loan_status <- factor(data$loan_status, levels = c(0, 1),
                           labels = c("NonDefaut", "Defaut"))
data$person_home_ownership     <- factor(data$person_home_ownership)
data$loan_intent               <- factor(data$loan_intent)
data$loan_grade                <- factor(data$loan_grade,
                                         levels = c("A","B","C","D","E","F","G"))
data$cb_person_default_on_file <- factor(data$cb_person_default_on_file)

# -- Étape 3 : découpage stratifié AVANT imputation et feature engineering -----
# Indispensable pour éviter tout data leakage vers le jeu de test.
index_train <- createDataPartition(data$loan_status, p = 0.8, list = FALSE)
train <- data[ index_train, ]
test  <- data[-index_train, ]

cat("Taille train :", nrow(train),
    "| Taille test :", nrow(test), "\n")
prop.table(table(train$loan_status))
prop.table(table(test$loan_status))

# -- Étape 4 : imputation par la médiane du TRAIN uniquement ------------------

med_int_rate   <- median(train$loan_int_rate,    na.rm = TRUE)
med_emp_length <- median(train$person_emp_length, na.rm = TRUE)

train$loan_int_rate[is.na(train$loan_int_rate)]         <- med_int_rate
train$person_emp_length[is.na(train$person_emp_length)] <- med_emp_length
test$loan_int_rate[is.na(test$loan_int_rate)]           <- med_int_rate
test$person_emp_length[is.na(test$person_emp_length)]   <- med_emp_length

cat("Médiane loan_int_rate (train) :", med_int_rate, "\n")
cat("Médiane person_emp_length (train) :", med_emp_length, "\n")
cat("Valeurs manquantes restantes - train :", sum(is.na(train)), "\n")
cat("Valeurs manquantes restantes - test  :", sum(is.na(test)),  "\n")

# -- Étape 5 : feature engineering (après imputation, sans statistique externe)

train <- train %>%
  mutate(
    risk_score      = loan_percent_income * loan_int_rate,
    high_risk_grade = as.integer(loan_grade %in% c("D", "E", "F", "G"))
  )
test <- test %>%
  mutate(
    risk_score      = loan_percent_income * loan_int_rate,
    high_risk_grade = as.integer(loan_grade %in% c("D", "E", "F", "G"))
  )
cat("Variables créées : risk_score, high_risk_grade\n")
cat("Valeurs manquantes finales - train :", sum(is.na(train)), "\n")
cat("Valeurs manquantes finales - test  :", sum(is.na(test)),  "\n")

# Fonction utilitaire : visualisation graphique de la matrice de confusion
plot_confusion_matrix <- function(cm, titre) {
  df <- as.data.frame(cm$table)
  names(df) <- c("Prediction", "Reference", "n")
  df <- df %>%
    mutate(
      type = case_when(
        Prediction == "Defaut"    & Reference == "Defaut"    ~
          paste0("Vrai Positif\n(VP = ", n, ")"),
        Prediction == "Defaut"    & Reference == "NonDefaut" ~
          paste0("Faux Positif\n(FP = ", n, ")"),
        Prediction == "NonDefaut" & Reference == "Defaut"    ~
          paste0("Faux Négatif\n(FN = ", n, ")"),
        Prediction == "NonDefaut" & Reference == "NonDefaut" ~
          paste0("Vrai Négatif\n(VN = ", n, ")")
      ),
      couleur = ifelse(Prediction == Reference, "correct", "erreur")
    )

  ggplot(df, aes(x = Reference, y = Prediction, fill = couleur)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = type), size = 4.5, fontface = "bold", color = "white") +
    scale_fill_manual(
      values = c("correct" = "#2196F3", "erreur" = "#E53935"),
      guide  = "none"
    ) +
    scale_x_discrete(
      labels = c("Defaut" = "Défaut (1)", "NonDefaut" = "Non-défaut (0)")
    ) +
    scale_y_discrete(
      labels = c("Defaut" = "Défaut (1)", "NonDefaut" = "Non-défaut (0)")
    ) +
    labs(title = titre, x = "Valeur réelle", y = "Prédiction") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

ctrl <- trainControl(method          = "cv",
                     number          = 10,
                     classProbs      = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

grid_nb <- expand.grid(laplace   = c(0, 0.5, 1, 2, 5),
                       usekernel = FALSE,
                       adjust    = 1)

set.seed(42)
nb_cv <- train(loan_status ~ .,
               data      = train,
               method    = "naive_bayes",
               trControl = ctrl,
               tuneGrid  = grid_nb,
               metric    = "ROC")

plot(nb_cv,
     main = "AUC-ROC en fonction du paramètre de lissage de Laplace")
cat("Meilleur laplace :", nb_cv$bestTune$laplace, "\n")

nb_final <- naive_bayes(loan_status ~ ., data = train,
                        laplace = nb_cv$bestTune$laplace)
print(nb_final)

pred_nb      <- predict(nb_final, newdata = test)
pred_nb_prob <- predict(nb_final, newdata = test, type = "prob")[, "Defaut"]

cm_nb <- confusionMatrix(pred_nb, test$loan_status, positive = "Defaut")
print(cm_nb)

plot_confusion_matrix(cm_nb, "Matrice de confusion - Naïve Bayes")

roc_nb <- roc(test$loan_status, pred_nb_prob,
              levels = c("NonDefaut", "Defaut"))
plot(roc_nb, col = "steelblue", lwd = 2, legacy.axes = TRUE,
     xlim = c(1, 0), ylim = c(0, 1), xaxs = "i", yaxs = "i",
     main = paste0("Courbe ROC - Naïve Bayes (AUC = ",
                   round(auc(roc_nb), 3), ")"))
abline(a = 1, b = -1, lty = 2, col = "grey60")

set.seed(42)
logit_cv <- train(loan_status ~ .,
                  data       = train,
                  method     = "glm",
                  family     = "binomial",
                  trControl  = ctrl,
                  preProcess = c("center", "scale"),
                  metric     = "ROC")

cat("AUC-ROC moyenne en validation croisée :",
    round(max(logit_cv$results$ROC), 3), "\n")

pred_logit      <- predict(logit_cv, newdata = test)
pred_logit_prob <- predict(logit_cv, newdata = test, type = "prob")[, "Defaut"]

cm_logit <- confusionMatrix(pred_logit, test$loan_status, positive = "Defaut")
print(cm_logit)

plot_confusion_matrix(cm_logit, "Matrice de confusion - Régression Logistique")

roc_logit <- roc(test$loan_status, pred_logit_prob,
                 levels = c("NonDefaut", "Defaut"))
plot(roc_logit,
     col = "forestgreen", lwd = 2,
     legacy.axes = TRUE,
     xlim = c(1, 0), ylim = c(0, 1), xaxs = "i", yaxs = "i",
     main = paste0("Courbe ROC - Régression Logistique (AUC = ",
                   round(auc(roc_logit), 3), ")"))

abline(a = 1, b = -1, lty = 2, col = "grey60")

extract_metrics <- function(cm, roc_obj) {
  data.frame(
    Accuracy  = round(cm$overall["Accuracy"], 3),
    Recall    = round(cm$byClass["Sensitivity"], 3),
    Precision = round(cm$byClass["Pos Pred Value"], 3),
    F1        = round(cm$byClass["F1"], 3),
    AUC       = round(auc(roc_obj), 3)
  )
}

metrics <- rbind(
  extract_metrics(cm_nb, roc_nb),
  extract_metrics(cm_logit, roc_logit)
)
rownames(metrics) <- c("Naïve Bayes", "Régression Logistique")

kable(metrics, booktabs = TRUE,
      caption = "Comparaison des métriques de performance sur le jeu de test") %>%
  kable_styling(latex_options = "HOLD_position")

plot(roc_nb, col = "steelblue", lwd = 2, legacy.axes = TRUE,
     xlim = c(1, 0), ylim = c(0, 1), xaxs = "i", yaxs = "i",
     main = "Superposition des courbes ROC")
lines(roc_logit, col = "forestgreen", lwd = 2)
abline(a = 1, b = -1, lty = 2, col = "grey60")
legend("bottomright",
       legend = c(paste0("Naïve Bayes (AUC = ", round(auc(roc_nb), 3), ")"),
                  paste0("Régression Log. (AUC = ", round(auc(roc_logit), 3), ")")),
       col    = c("steelblue", "forestgreen"),
       lwd    = 2, bty = "n")

saveRDS(nb_final,    "modele_nb.rds")
saveRDS(logit_cv,    "modele_logit.rds")
