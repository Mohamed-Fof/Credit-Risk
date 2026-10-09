# =============================================================================
# Fonctions communes : découpage stratifié et mesures de performance
# =============================================================================

GRAINE <- 2026

# Découpage apprentissage / test stratifié sur la cible (même taux de défaut des deux côtés).
decoupage_stratifie <- function(y, part_test = 0.2, graine = GRAINE) {
  set.seed(graine)
  unlist(lapply(split(seq_along(y), y), function(i) sample(i, round(length(i) * part_test))))
}

# Plis de validation croisée stratifiés, identiques pour tous les modèles.
plis_stratifies <- function(y, k = 5, graine = GRAINE) {
  set.seed(graine)
  pli <- integer(length(y))
  for (classe in split(seq_along(y), y)) pli[classe] <- sample(rep_len(seq_len(k), length(classe)))
  pli
}

auc <- function(y, p) {
  # Statistique de Mann-Whitney : probabilité qu'un défaut ait un score plus élevé qu'un non-défaut.
  r <- rank(p)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

ks <- function(y, p) {
  # Écart maximal entre les fonctions de répartition des scores des deux populations.
  o <- order(p, decreasing = TRUE)
  y <- y[o]
  max(abs(cumsum(y == 1) / sum(y == 1) - cumsum(y == 0) / sum(y == 0)))
}

brier <- function(y, p) mean((p - y)^2)

log_vraisemblance <- function(y, p) {
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# Coût attendu d'une règle de décision : un défaut accepté (faux négatif) coûte
# RATIO_COUT fois plus qu'un bon client refusé (faux positif).
RATIO_COUT <- 5
cout_moyen <- function(y, p, seuil, ratio = RATIO_COUT) {
  refuse <- p >= seuil
  (ratio * sum(y == 1 & !refuse) + sum(y == 0 & refuse)) / length(y)
}

seuil_optimal <- function(y, p, ratio = RATIO_COUT) {
  candidats <- seq(0.02, 0.9, by = 0.005)
  couts <- vapply(candidats, function(s) cout_moyen(y, p, s, ratio), numeric(1))
  candidats[which.min(couts)]
}

mesures <- function(y, p, seuil) {
  refuse <- p >= seuil
  vp <- sum(refuse & y == 1); fp <- sum(refuse & y == 0)
  fn <- sum(!refuse & y == 1); vn <- sum(!refuse & y == 0)
  a <- auc(y, p)
  data.frame(
    AUC = a, Gini = 2 * a - 1, KS = ks(y, p),
    Brier = brier(y, p), LogLoss = log_vraisemblance(y, p),
    Seuil = seuil,
    Rappel = vp / (vp + fn),            # part des défauts détectés
    Precision = vp / max(vp + fp, 1),   # part des refus justifiés
    Specificite = vn / (vn + fp),       # part des bons clients acceptés
    Cout = cout_moyen(y, p, seuil)
  )
}
