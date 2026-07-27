library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cluster)
library(writexl)
library(stringr)
library(flextable)
library(officer)

# ============================================================
# 0. PARAMETRES D'IMPORT
# ============================================================

FICHIER <- "essais_MCST_M0.xlsx"
FICHIER_SCORES_CLASSIQUES <- "description_scores_classiques_et_donnees_sociodemographiques.xlsx"
FICHIER_VARIABLES_DYNAMIQUES <- "description_variables_dynamiques_MCST_M0.xlsx"
FEUILLE_VARIABLES_DYNAMIQUES <- "indicateurs_dynamiques_patien"

base_essais <- read_excel(FICHIER)

# ============================================================
# 1. NETTOYAGE
# ============================================================

colonnes_attendues <- c("patient", "essai", "COR", "PE", "ERR", "abandon")

if (!all(colonnes_attendues %in% names(base_essais))) {
  stop("Colonnes manquantes. Il faut : patient, essai, COR, PE, ERR, abandon")
}

base_essais <- base_essais %>%
  mutate(
    patient = as.character(patient),
    patient = if_else(
      !is.na(patient) & str_detect(patient, "^\\d+$"),
      str_pad(patient, width = 3, side = "left", pad = "0"),
      patient
    ),
    essai = as.integer(essai),
    across(
      all_of(c("COR", "PE", "ERR", "abandon")),
      ~ replace_na(as.numeric(.x), 0)
    ),
    COR = if_else(COR > 0, 1, 0),
    PE = if_else(PE > 0, 1, 0),
    ERR = if_else(ERR > 0, 1, 0),
    abandon = if_else(abandon > 0, 1, 0)
  )


incoherences <- base_essais %>%
  filter(COR == 1 & (PE == 1 | ERR == 1))

if (nrow(incoherences) > 0) {
  message("Attention : essais avec COR=1 et PE/ERR=1. Ils seront traites comme corrects.")
  print(incoherences)
}

base_essais <- base_essais %>%
  mutate(
    PE = if_else(COR == 1, 0, PE),
    ERR = if_else(COR == 1, 0, ERR),
    essai_reel = COR == 1 | PE == 1 | ERR == 1 | abandon == 1
  ) %>%
  filter(essai_reel) %>%
  arrange(patient, essai)

sd0 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(0)
  sd(x)
}

# ============================================================
# 2. IDENTIFICATION DES CR
# ============================================================

identifier_CR_patient <- function(df_patient) {
  
  df_patient <- df_patient %>%
    arrange(essai)
  
  n <- nrow(df_patient)
  compteur_COR <- 0L
  nb_categories <- 0L
  
  n_categories_avant_essai <- integer(n)
  categorie_completee <- integer(n)
  
  for (i in seq_len(n)) {
    
    n_categories_avant_essai[i] <- nb_categories
    
    if (df_patient$COR[i] == 1) {
      
      compteur_COR <- compteur_COR + 1L
      
      if (compteur_COR == 6L) {
        categorie_completee[i] <- 1L
        nb_categories <- nb_categories + 1L
        compteur_COR <- 0L
      }
      
    } else if (
      df_patient$PE[i] == 1 |
      df_patient$ERR[i] == 1 |
      df_patient$abandon[i] == 1
    ) {
      compteur_COR <- 0L
    }
  }
  
  df_patient %>%
    mutate(
      n_categories_avant_essai = n_categories_avant_essai,
      categorie_completee = categorie_completee,
      CR = if_else(
        n_categories_avant_essai >= 1,
        n_categories_avant_essai,
        NA_integer_
      )
    )
}

essais_CR_tous <- base_essais %>%
  group_by(patient) %>%
  group_modify(~ identifier_CR_patient(.x)) %>%
  ungroup()


statut_patients <- essais_CR_tous %>%
  group_by(patient) %>%
  summarise(
    abandon = as.integer(any(abandon == 1, na.rm = TRUE)),
    .groups = "drop"
  )

# ============================================================
# 3. TABLES INTERNES NECESSAIRES AU CLUSTERING
# ============================================================

resume_categories_tous <- essais_CR_tous %>%
  group_by(patient) %>%
  summarise(
    nb_essais = n(),
    nb_categories_completees = sum(categorie_completee, na.rm = TRUE),
    .groups = "drop"
  )


# Table interne patient x CR nécessaire pour calculer les variables patient
# Elle ne sera pas exportée comme fiche CR.
table_CR_base <- essais_CR_tous %>%
  filter(!is.na(CR)) %>%
  group_by(patient, CR) %>%
  summarise(
    nb_CORR = sum(COR, na.rm = TRUE),
    nb_ERR = sum(ERR, na.rm = TRUE),
    nb_PE = sum(PE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(statut_patients, by = "patient")

# ============================================================
# 4. RESUME PATIENTS
# Les abandons restent dans la même table que les autres patients.
# ============================================================

resume_patient_depuis_CR <- function(table_CR, patients_ref) {
  
  patients_ref %>%
    left_join(resume_categories_tous, by = "patient") %>%
    left_join(
      table_CR %>%
        mutate(CR_avec_PE = as.integer(nb_PE > 0)) %>%
        group_by(patient) %>%
        summarise(
          nb_CR = n(),
          
          nb_CORR_CR_total = sum(nb_CORR, na.rm = TRUE),
          nb_CORR_CR_moy = mean(nb_CORR, na.rm = TRUE),
          nb_CORR_CR_sd = sd0(nb_CORR),
          nb_CORR_CR_med = median(nb_CORR, na.rm = TRUE),
          
          nb_ERR_CR_total = sum(nb_ERR, na.rm = TRUE),
          nb_ERR_CR_moy = mean(nb_ERR, na.rm = TRUE),
          nb_ERR_CR_sd = sd0(nb_ERR),
          nb_ERR_CR_med = median(nb_ERR, na.rm = TRUE),
          
          nb_PE_CR_total = sum(nb_PE, na.rm = TRUE),
          nb_PE_CR_moy = mean(nb_PE, na.rm = TRUE),
          nb_PE_CR_sd = sd0(nb_PE),
          nb_PE_CR_med = median(nb_PE, na.rm = TRUE),
          
          nb_CR_avec_PE = sum(CR_avec_PE, na.rm = TRUE),
          CR_avec_PE_moy = mean(CR_avec_PE, na.rm = TRUE),
          CR_avec_PE_sd = sd0(CR_avec_PE),
          CR_avec_PE_med = median(CR_avec_PE, na.rm = TRUE),
          
          .groups = "drop"
        ),
      by = "patient"
    ) %>%
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 0))
    ) %>%
    arrange(patient)
}

table_patients_base <- resume_patient_depuis_CR(
  table_CR = table_CR_base,
  patients_ref = statut_patients
)

table_patients_analyse <- table_patients_base %>%
  filter(abandon == 0)

# ============================================================
# 5. CLUSTERING WARD.D2
# Variables : nb_PE_CR_total + nb_CR_avec_PE
# Les abandons ne participent pas au clustering.
# ============================================================

variables_clustering <- c("nb_PE_CR_total", "nb_CR_avec_PE")

X_CR <- table_patients_analyse %>%
  select(all_of(variables_clustering)) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale() %>%
  as.data.frame()

dist_CR <- dist(X_CR, method = "euclidean")
hc_CR <- hclust(dist_CR, method = "ward.D2")

# ============================================================
# 6. SILHOUETTE ET CHOIX DE K
# ============================================================

K_MIN <- 2
K_MAX <- min(6, nrow(X_CR) - 1)

table_silhouette <- map_dfr(K_MIN:K_MAX, function(k) {
  
  clusters_k <- cutree(hc_CR, k = k)
  sil_k <- silhouette(clusters_k, dist_CR)
  
  tibble(
    k = k,
    silhouette_moyenne = round(mean(sil_k[, "sil_width"]), 3),
    tailles_clusters = paste(
      names(table(clusters_k)),
      as.integer(table(clusters_k)),
      sep = "=",
      collapse = " / "
    )
  )
})

print(table_silhouette)
View(table_silhouette)

plot_silhouette <- ggplot(
  table_silhouette,
  aes(x = k, y = silhouette_moyenne)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = K_MIN:K_MAX) +
  labs(
    title = "Silhouette moyenne selon le nombre de clusters",
    x = "Nombre de clusters",
    y = "Silhouette moyenne"
  ) +
  theme_minimal()

print(plot_silhouette)

ggsave(
  "silhouette_Ward_CR.pdf",
  plot_silhouette,
  width = 8,
  height = 5,
  dpi = 300
)

K_CHOISI <- 5




# ============================================================
# 7. ATTRIBUTION DES CLUSTERS
# Les clusters 1:K_CHOISI viennent de Ward.
# Les patients ayant abandonné sont exclus définitivement.
# ============================================================

clusters_bruts <- cutree(hc_CR, k = K_CHOISI)

ordre_clusters <- table_patients_analyse %>%
  mutate(cluster_brut = clusters_bruts) %>%
  group_by(cluster_brut) %>%
  summarise(
    PE_moyen = mean(nb_PE_CR_total, na.rm = TRUE),
    CR_avec_PE_moyen = mean(nb_CR_avec_PE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(PE_moyen, CR_avec_PE_moyen) %>%
  mutate(cluster = row_number())

table_patients <- table_patients_analyse %>%
  mutate(cluster_brut = clusters_bruts) %>%
  left_join(
    ordre_clusters %>%
      select(cluster_brut, cluster),
    by = "cluster_brut"
  ) %>%
  select(-cluster_brut) %>%
  mutate(
    cluster = as.integer(cluster),
    statut = "Analyse"
  ) %>%
  select(
    patient,
    cluster,
    abandon,
    nb_essais,
    nb_categories_completees,
    nb_CR,
    nb_CORR_CR_total,
    nb_CORR_CR_moy,
    nb_CORR_CR_sd,
    nb_CORR_CR_med,
    nb_ERR_CR_total,
    nb_ERR_CR_moy,
    nb_ERR_CR_sd,
    nb_ERR_CR_med,
    nb_PE_CR_total,
    nb_PE_CR_moy,
    nb_PE_CR_sd,
    nb_PE_CR_med,
    nb_CR_avec_PE,
    CR_avec_PE_moy,
    CR_avec_PE_sd,
    CR_avec_PE_med
  ) %>%
  arrange(cluster, patient)

View(table_patients)

# ============================================================
# 7bis. AJOUT DES SCORES CLASSIQUES ET DES INDICATEURS
# DYNAMIQUES APRES ATTRIBUTION DES CLUSTERS
# ============================================================

# ------------------------------------------------------------
# 7bis.1 Scores classiques
# ------------------------------------------------------------

scores_classiques_FC <- read_excel(
  FICHIER_SCORES_CLASSIQUES,
  sheet = "scores_patient_par_patient"
) %>%
  transmute(
    patient = str_pad(
      as.character(patient),
      width = 3,
      side = "left",
      pad = "0"
    ),
    
    temps = as.numeric(temps),
    categories = as.numeric(categories),
    cartes_6_cat = as.numeric(cartes_6_cat),
    erreurs_totales = as.numeric(erreurs_totales),
    perseverations = as.numeric(perseverations),
    pct_perseverations = as.numeric(pct_perseverations),
    
    # Variables sociodémographiques et cliniques
    age_M0 = as.numeric(age_M0),
    sexe_M0 = as.character(sexe_M0),
    Etudes = as.numeric(Etudes),
    madrstot_M0 = as.numeric(madrstot_M0),
    staiatot_M0 = as.numeric(staiatot_M0)
  )


# ------------------------------------------------------------
# 7bis.2 Indicateurs dynamiques
# Le fichier ne contient qu'une seule feuille.
# ------------------------------------------------------------

variables_dynamiques <- read_excel(
  FICHIER_VARIABLES_DYNAMIQUES,
  sheet = FEUILLE_VARIABLES_DYNAMIQUES
) %>%
  transmute(
    patient = str_pad(
      as.character(patient),
      width = 3,
      side = "left",
      pad = "0"
    ),
    
    probabilite_changement_apres_feedback_negatif =
      as.numeric(probabilite_changement_apres_feedback_negatif),
    
    probabilite_maintien_apres_feedback_positif =
      as.numeric(probabilite_maintien_apres_feedback_positif),
    
    nb_essais_nouvelle_categorie_moy =
      as.numeric(nb_essais_nouvelle_categorie_moy),
    
    longueur_sequences_PE_moy =
      as.numeric(longueur_sequences_PE_moy),
    
    longueur_sequences_PE_max =
      as.numeric(longueur_sequences_PE_max),
    
    nb_ruptures_set_apres_3_COR =
      as.numeric(nb_ruptures_set_apres_3_COR),
    
    probabilite_rupture_set_apres_3_COR =
      as.numeric(probabilite_rupture_set_apres_3_COR),
    
    proportion_PE_parmi_erreurs =
      as.numeric(proportion_PE_parmi_erreurs)
  )


# Vérification : une seule ligne par patient
doublons_variables_dynamiques <- variables_dynamiques %>%
  count(patient) %>%
  filter(n > 1)

if (nrow(doublons_variables_dynamiques) > 0) {
  stop(
    "Le fichier des variables dynamiques contient plusieurs lignes pour certains patients."
  )
}

# ------------------------------------------------------------
# 7bis.3 Jointure avec le tableau patient
# ------------------------------------------------------------

table_patients <- table_patients %>%
  left_join(
    scores_classiques_FC,
    by = "patient"
  ) %>%
  left_join(
    variables_dynamiques,
    by = "patient"
  )

# ============================================================
# 8. TABLE DESCRIPTIVE DES CLUSTERS
# Beau tableau avec effectif du cluster + variables comprehensibles
# ============================================================

# ------------------------------------------------------------
# 8.1 Noms lisibles des variables
# ------------------------------------------------------------

variables_clusters_lisibles <- tibble(
  variable_code = c(
    # Variables sociodémographiques et cliniques
    "age_M0",
    "Etudes",
    "madrstot_M0",
    "staiatot_M0",
    
    # Scores classiques et variables liées aux changements de règle
    "categories",
    "cartes_6_cat",
    "erreurs_totales",
    "perseverations",
    "temps",
    "nb_essais",
    "nb_CR",
    "nb_ERR_CR_total",
    "nb_PE_CR_total",
    "nb_CR_avec_PE",
    "nb_CORR_CR_total",
    
    # Indicateurs dynamiques
    "probabilite_changement_apres_feedback_negatif",
    "probabilite_maintien_apres_feedback_positif",
    "nb_essais_nouvelle_categorie_moy",
    "longueur_sequences_PE_moy",
    "longueur_sequences_PE_max",
    "nb_ruptures_set_apres_3_COR",
    "probabilite_rupture_set_apres_3_COR",
    "proportion_PE_parmi_erreurs"
  ),
  
  Variable = c(
    # Variables sociodémographiques et cliniques
    "Âge à M0",
    "Nombre d’années d’études",
    "Score MADRS total à M0",
    "Score STAI-A total à M0",
    
    # Scores classiques et variables liées aux changements de règle
    "Nombre de catégories complétées",
    "Nombre de cartes pour compléter 6 catégories",
    "Nombre total d’erreurs",
    "Nombre total d’EP",
    "Temps de passation",
    "Nombre total d’essais",
    "Nombre de changements de règle",
    "Nombre d’erreurs simples après changement de règle",
    "Nombre d’erreurs de persévération après changement de règle",
    "Nombre de changements de règle avec erreur de persévération",
    "Nombre d’essais corrects après changement de règle",
    
    # Indicateurs dynamiques
    "Probabilité de changer de règle après un feedback négatif",
    "Probabilité de maintenir la règle après un feedback positif",
    "Nombre d’essais nécessaires pour compléter une nouvelle catégorie",
    "Longueur moyenne des séquences persévératives",
    "Longueur maximale des séquences persévératives",
    "Nombre de ruptures de set après au moins 3 réponses correctes",
    "Probabilité de rupture de set après au moins 3 réponses correctes",
    "Proportion d’EP parmi les erreurs totales"
  ),
  
  # Ordre d'affichage dans le tableau final.
  # La valeur 2 est réservée au sexe, traité séparément car catégoriel.
  ordre = c(
    1, 3, 4, 5,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28
  )
)

# ------------------------------------------------------------
# 8.2 Ligne effectif du cluster
# ------------------------------------------------------------

effectifs_clusters <- table_patients %>%
  group_by(cluster) %>%
  summarise(
    Valeur = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cluster = paste0("Cluster ", cluster),
    Variable = "Effectif du cluster",
    Indicateur = "N",
    ordre = 0
  ) %>%
  select(
    ordre,
    Variable,
    Indicateur,
    cluster,
    Valeur
  )

# ------------------------------------------------------------
# 8.3 Description des variables quantitatives par cluster
# ------------------------------------------------------------

description_clusters_long <- table_patients %>%
  select(
    cluster,
    patient,
    all_of(variables_clusters_lisibles$variable_code)
  ) %>%
  pivot_longer(
    cols = all_of(variables_clusters_lisibles$variable_code),
    names_to = "variable_code",
    values_to = "valeur"
  ) %>%
  left_join(
    variables_clusters_lisibles,
    by = "variable_code"
  ) %>%
  group_by(cluster, Variable, ordre) %>%
  summarise(
    Valeur = paste0(
      round(mean(valeur, na.rm = TRUE), 2),
      " (",
      round(median(valeur, na.rm = TRUE), 2),
      " ; ",
      round(sd0(valeur), 2),
      ")"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    cluster = paste0("Cluster ", cluster),
    Indicateur = "Moyenne (Médiane ; ET)"
  )

# ------------------------------------------------------------
# 8.3bis Description du sexe par cluster
# Variable catégorielle : effectif et pourcentage, pas moyenne/ET.
# Le codage 1/2 est conservé tant que sa signification n'est pas confirmée.
# ------------------------------------------------------------

modalites_sexe <- sort(unique(na.omit(as.character(table_patients$sexe_M0))))

description_sexe_clusters_long <- table_patients %>%
  mutate(sexe_M0 = as.character(sexe_M0)) %>%
  filter(!is.na(sexe_M0)) %>%
  count(cluster, sexe_M0, name = "n") %>%
  complete(
    cluster = sort(unique(table_patients$cluster)),
    sexe_M0 = modalites_sexe,
    fill = list(n = 0)
  ) %>%
  group_by(cluster) %>%
  mutate(
    n_sexe_disponible = sum(n),
    Valeur = if_else(
      n_sexe_disponible > 0,
      paste0(
        n,
        " (",
        round(100 * n / n_sexe_disponible, 1),
        " %)"
      ),
      NA_character_
    )
  ) %>%
  ungroup() %>%
  mutate(
    cluster = paste0("Cluster ", cluster),
    Variable = "Sexe",
    Indicateur = paste0("Code ", sexe_M0, " : N (%)"),
    ordre = 2 + match(sexe_M0, modalites_sexe) / 100
  ) %>%
  select(
    ordre,
    Variable,
    Indicateur,
    cluster,
    Valeur
  )

# ------------------------------------------------------------
# 8.4 Fusion effectifs + statistiques descriptives
# ------------------------------------------------------------

table_clusters <- bind_rows(
  effectifs_clusters %>%
    mutate(Valeur = as.character(Valeur)),
  
  description_clusters_long %>%
    mutate(Valeur = as.character(Valeur)) %>%
    select(
      ordre,
      Variable,
      Indicateur,
      cluster,
      Valeur
    ),
  
  description_sexe_clusters_long
) %>%
  arrange(ordre, Indicateur) %>%
  select(-ordre) %>%
  pivot_wider(
    names_from = cluster,
    values_from = Valeur
  )

# ============================================================
# 9. FIGURES
# ============================================================

# ------------------------------------------------------------
# 9.1 Heatmap patient x CR
# ------------------------------------------------------------

max_CR <- max(table_CR_base$CR, na.rm = TRUE)

if (!is.finite(max_CR) || max_CR < 1) {
  max_CR <- 1
}

patients_figures <- table_patients %>%
  select(
    patient,
    cluster,
    abandon,
    nb_PE_CR_total,
    nb_CR_avec_PE
  ) %>%
  mutate(
    patient_label = paste0(
      patient,
      " | PE=",
      nb_PE_CR_total,
      " | CR PE=",
      nb_CR_avec_PE,
      if_else(abandon == 1, " | ABANDON", "")
    )
  )

data_heatmap_CR <- patients_figures %>%
  crossing(CR = 1:max_CR) %>%
  left_join(
    table_CR_base %>%
      select(patient, CR, nb_PE),
    by = c("patient", "CR")
  ) %>%
  mutate(
    nb_PE = replace_na(nb_PE, 0)
  )

ordre_patients_CR <- patients_figures %>%
  arrange(
    cluster,
    nb_CR_avec_PE,
    nb_PE_CR_total,
    patient
  ) %>%
  pull(patient_label)

data_heatmap_CR <- data_heatmap_CR %>%
  mutate(
    patient_label = factor(patient_label, levels = ordre_patients_CR),
    cluster = factor(cluster)
  )

max_PE_CR <- max(data_heatmap_CR$nb_PE, na.rm = TRUE)

pdf(
  "heatmap_PE_par_CR_et_cluster.pdf",
  width = 11,
  height = 8
)

for (cl in levels(data_heatmap_CR$cluster)) {
  
  data_cl <- data_heatmap_CR %>%
    filter(cluster == cl)
  
  n_patients <- n_distinct(data_cl$patient)
  
  p <- ggplot(
    data_cl,
    aes(
      x = CR,
      y = patient_label,
      fill = nb_PE
    )
  ) +
    geom_tile(color = "grey85") +
    geom_text(
      aes(label = ifelse(nb_PE == 0, "", nb_PE)),
      size = 3,
      color = "white"
    ) +
    scale_x_continuous(
      breaks = 1:max_CR,
      expand = c(0, 0)
    ) +
    scale_fill_gradient(
      low = "grey95",
      high = "black",
      limits = c(0, max_PE_CR),
      name = "Nb PE"
    ) +
    labs(
      title = paste0("Cluster ", cl, " - PE par CR"),
      subtitle = paste0(n_patients, " patient(s)"),
      x = "CR",
      y = "Patient"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10),
      panel.grid = element_blank()
    )
  
  print(p)
}

dev.off()

# ------------------------------------------------------------
# 9.2 Trajectoires individuelles
# ------------------------------------------------------------

data_changements_CR <- essais_CR_tous %>%
  filter(categorie_completee == 1) %>%
  left_join(
    patients_figures,
    by = "patient"
  ) %>%
  mutate(
    x_changement = essai + 0.5
  )

data_dernier_essai_CR <- essais_CR_tous %>%
  group_by(patient) %>%
  summarise(
    dernier_essai = max(essai, na.rm = TRUE),
    .groups = "drop"
  )


data_traj_CR <- patients_figures %>%
  crossing(essai = 1:48) %>%
  left_join(
    essais_CR_tous %>%
      select(patient, essai, COR, ERR, PE, categorie_completee),
    by = c("patient", "essai")
  ) %>%
  left_join(
    data_dernier_essai_CR,
    by = "patient"
  ) %>%
  mutate(
    essai_observe = essai <= dernier_essai,
    PE_plot = if_else(
      essai_observe,
      replace_na(PE, 0),
      NA_real_
    ),
    type_reponse = case_when(
      !essai_observe ~ "non_observe",
      PE_plot == 1 ~ "PE",
      TRUE ~ "non_PE"
    ),
    patient_label = factor(patient_label, levels = ordre_patients_CR),
    cluster = factor(cluster)
  )

data_changements_CR <- data_changements_CR %>%
  mutate(
    patient_label = factor(patient_label, levels = ordre_patients_CR),
    cluster = factor(cluster)
  )


pdf(
  "trajectoires_individuelles_PE_essai_par_essai_par_cluster.pdf",
  width = 14,
  height = 11
)

for (cl in levels(data_traj_CR$cluster)) {
  
  data_cl <- data_traj_CR %>%
    filter(cluster == cl) %>%
    droplevels()
  
  changements_cl <- data_changements_CR %>%
    filter(cluster == cl) %>%
    droplevels()
  
  n_patients <- n_distinct(data_cl$patient)
  
  p <- ggplot(
    data_cl,
    aes(
      x = essai,
      y = PE_plot
    )
  ) +
    geom_vline(
      data = changements_cl,
      aes(xintercept = x_changement),
      inherit.aes = FALSE,
      color = "blue",
      linetype = "dashed",
      linewidth = 0.4,
      alpha = 0.6
    ) +
    geom_hline(
      yintercept = 0,
      color = "grey80",
      linewidth = 0.3
    ) +
    geom_point(
      data = data_cl %>% filter(type_reponse == "non_PE"),
      aes(x = essai, y = PE_plot),
      color = "grey70",
      size = 1.1,
      alpha = 0.8
    ) +
    geom_point(
      data = data_cl %>% filter(type_reponse == "PE"),
      aes(x = essai, y = PE_plot),
      color = "black",
      size = 2.2
    ) +
    geom_segment(
      data = data_cl %>% filter(type_reponse == "PE"),
      aes(
        x = essai,
        xend = essai,
        y = 0,
        yend = 1
      ),
      color = "black",
      linewidth = 0.5
    ) +
    facet_wrap(
      ~ patient_label,
      ncol = 4
    ) +
    scale_x_continuous(
      breaks = seq(0, 48, by = 6),
      limits = c(1, 48)
    ) +
    scale_y_continuous(
      breaks = c(0, 1),
      labels = c("non-PE", "PE"),
      limits = c(-0.1, 1.1)
    ) +
    labs(
      title = paste0("Cluster ", cl, " - trajectoires PE essai par essai"),
      subtitle = paste0(
        n_patients,
        " patient(s) - noir = PE ; gris = non-PE ; bleu = categorie completee"
      ),
      x = "Essai",
      y = "Reponse"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  print(p)
}

dev.off()

# ============================================================
# 10. DENDROGRAMME
# Le dendrogramme concerne uniquement les patients non-abandons.
# ============================================================

pdf(
  "dendrogramme_Ward_CR.pdf",
  width = 12,
  height = 8
)

plot(
  hc_CR,
  labels = table_patients_analyse$patient,
  main = "Dendrogramme Ward.D2 - variables CR",
  xlab = "Patients",
  ylab = "Distance"
)

rect.hclust(hc_CR, k = K_CHOISI, border = 2:(K_CHOISI + 1))

dev.off()

# ============================================================
# 11. EXPORT EXCEL ET WORD
# ============================================================

table_patients_export <- table_patients %>%
  rename(
    numero_patient = patient,
    numero_cluster = cluster
  )

table_clusters_export <- table_clusters

print(table_patients_export)
print(table_clusters_export)

View(table_patients_export)
View(table_clusters_export)

# ------------------------------------------------------------
# 11.1 Export Excel
# ------------------------------------------------------------

write_xlsx(
  list(
    patients = table_patients_export,
    clusters = table_clusters_export,
    silhouette = table_silhouette,
    resume_CR = table_CR_base
  ),
  "clustering_MCST_tabl.xlsx"
)

# ------------------------------------------------------------
# 11.2 Export Word avec beau tableau
# ------------------------------------------------------------

tableau_clusters_word <- table_clusters_export %>%
  flextable() %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = c("Variable", "Indicateur"), align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 9, part = "all") %>%
  set_caption(
    caption = "Tableau. Description sociodémographique, clinique et cognitive des clusters"
  )

document_word_clusters <- read_docx() %>%
  body_add_par(
    "Description des clusters de flexibilité cognitive",
    style = "heading 1"
  ) %>%
  body_add_flextable(tableau_clusters_word)

print(
  document_word_clusters,
  target = "tableau_description_clusters_MCST_M0.docx"
)

