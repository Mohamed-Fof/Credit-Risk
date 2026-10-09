# Journal des modifications

## Version 2 — octobre 2026

Reprise complète du projet présenté en soutenance (L3 MIASHS, avril 2026). La version d'origine est conservée sous l'étiquette git `v1-soutenance`.

### Résultats
- Modèle de décision : **XGBoost avec contraintes de cohérence**, AUC **0,923** sur le jeu de test, contre **0,864** pour la régression logistique de la version 1 mesurée sur les mêmes dossiers.
- Défauts détectés : **76 %** au lieu de 59 %, avec 76 % de refus justifiés ; coût moyen des erreurs réduit de **37 %**.
- XGBoost sans contrainte atteint 0,947 mais réagit à contresens (risque en hausse quand le revenu augmente pour 23 % des dossiers) : conservé comme référence, pas pour décider.
- Correction d'un chiffre : l'ancien README annonçait une AUC « ~0,90 » ; la reproduction exacte de la version 1 donne 0,876 sur son propre jeu de test et 0,864 sur le nouveau.

### Données
- Variables et modalités renommées en français (`age`, `revenu_annuel`, `statut_logement`, `motif_pret`, `note_risque`, `taux_effort`…), via un dictionnaire unique.
- Suppression des 165 doublons exacts **avant** le découpage (un même dossier pouvait se trouver en apprentissage et en test).
- Suppression de 2 anciennetés professionnelles impossibles (jusqu'à 123 ans), en plus des 5 âges supérieurs à 100 ans déjà retirés.
- Taux d'effort recalculé exactement (montant / revenu) au lieu de la colonne arrondie à 2 décimales.
- Indicateurs « taux non renseigné » et « ancienneté non renseignée » : l'absence d'information est conservée au lieu d'être effacée par l'imputation.
- Imputation réapprise dans chaque pli de validation croisée.

### Modèles
- Quatre modèles ajoutés : **XGBoost**, **grille de score bancaire** (WoE, valeur d'information, points), **elastic net** et une reproduction de la **logistique v1** pour mesurer le progrès.
- Naïve Bayes : densités par noyau (AUC 0,872 contre 0,847 en gaussien).
- Suppression de `high_risk_grade`, entièrement déduite de la note de risque : la régression ne pouvait pas l'estimer (coefficient « NA ») et elle faussait le Naïve Bayes.
- XGBoost réglé par validation croisée (12 combinaisons, arrêt précoce), avec contraintes de monotonie sur le montant, le taux d'effort, le revenu, le taux, la note et le défaut antérieur.
- Audit de cohérence automatique (part des décisions à contresens quand on modifie une seule caractéristique).
- Grille de score : variables à coefficient de signe incohérent retirées (le défaut antérieur rapportait des points).
- Mêmes 5 plis de validation croisée pour tous les modèles ; graine fixée.

### Évaluation
- Seuil de décision optimisé selon le coût des erreurs (hypothèse 5 pour 1), choisi en validation croisée, au lieu du seuil fixe de 0,5.
- Nouveaux indicateurs : Gini, KS, score de Brier, log-loss, intervalles de confiance de l'AUC, tests de DeLong, calibration.
- Analyse de sensibilité : performance sans la note de risque du prêteur.
- Mise en évidence de règles quasi déterministes dans les données (100 % de défaut dans certains segments), documentées comme limite.
- Valeurs de Shapley pour l'importance des variables.
- Figures et tableaux enregistrés dans `resultats/`.

### Application
- Refonte complète avec `bslib`, entièrement en français, utilisable sur téléphone.
- Explication de chaque décision (valeurs de Shapley) et détail des points de la grille de score.
- Onglet de comparaison des modèles et onglet de méthode.
- Contrôle des saisies (bornes, cohérence âge / ancienneté / historique, montant / revenu) et alerte d'extrapolation.
- Modèles allégés : 2,3 Mo au total, chargés une seule fois (la v1 rechargeait un modèle de 40 Mo à chaque clic).
- Contrôle automatique : l'application reproduit exactement les prédictions de l'entraînement.

### Sécurité
- Revérification côté serveur de toutes les saisies, y compris les listes déroulantes (liste blanche) : testé contre un client modifié envoyant du HTML, du code R, des valeurs vides ou des nombres géants.
- Messages d'erreur techniques masqués (`shiny.sanitize.errors`).
- Conteneur exécuté sans droits d'administrateur.

### Déploiement
- Image Docker minimale (`rocker/r-ver` au lieu de `rocker/shiny`), paquets précompilés figés à la date d'entraînement, lancement sans shiny-server.
- Tâche GitHub Actions qui garde l'application éveillée sur Render (fin du réveil de 72 secondes).
- Locale UTF-8 explicite et bibliothèque système `libuv1` (requise par `fs`/`sass`, absente des paquets précompilés).
- Contrôle à la construction : l'image n'est produite que si la page d'accueil s'affiche réellement.
- Intégration continue GitHub Actions : construction de l'image, lancement du conteneur et test de la page à chaque envoi.

### Tests
- 26 tests automatiques (`tests/tests.R`) : contrôle des saisies face à un client malveillant, préparation, reproduction exacte des prédictions, additivité des valeurs de Shapley, cohérence du modèle sur tous les dossiers de test, grille de score.

### Performance (serveur gratuit de Render, environ 0,1 processeur)
- Page construite une seule fois au démarrage puis resservie : 223 ms → 1,3 ms par visite (la compilation du thème Sass était refaite à chaque visite, soit près de 3 s sur Render).
- Graphiques des onglets Comparaison et Exploration dessinés à l'export, servis comme images ; explication de la décision en barres HTML ; ggplot2 n'est plus chargé (4 paquets au lieu de 5).
- Correction : l'onglet Exploration plantait sur la variable « Taux d'effort ».
- Bandeau de décision en grille CSS : le composant bslib qu'il contenait recompilait ses styles Sass et recopiait ses fichiers à chaque saisie (identifié au profileur).
- XGBoost limité à un fil de calcul (`nthread = 1`, `OMP_NUM_THREADS=1`) : sur une fraction de processeur, un fil par cœur de la machine hôte provoquait une contention.
- Réveil anticipé : le portfolio contacte l'application en arrière-plan dès qu'un visiteur arrive ; tâche de réveil GitHub décalée sur des minutes moins encombrées.

### Organisation du code
- Script unique découpé en quatre étapes (`R/01` à `R/04`) et un lanceur `R/executer_tout.R`.
- Préparation des données partagée entre l'entraînement et l'application : aucun écart possible entre les deux.
- Fichiers de la soutenance archivés dans `rapport_L3/`.

## Version 1 — avril 2026

Projet de Compléments de mathématiques : Naïve Bayes et régression logistique, application Shiny déployée sur Render.
