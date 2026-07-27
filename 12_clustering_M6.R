library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cluster)
library(writexl)
library(stringr)

# ============================================================
# CLUSTERING MCST M6
# ============================================================

# ============================================================
# 0. PARAMETRES D'IMPORT
# ============================================================

FICHIER_M6 <- "essais_MCST_M6.xlsx"

base_essais_M6 <- read_excel(FICHIER_M6)

# ============================================================
# 1. NETTOYAGE
# ============================================================

colonnes_attendues_M6 <- c("patient", "essai", "COR", "PE", "ERR", "abandon")

if (!all(colonnes_attendues_M6 %in% names(base_essais_M6))) {
  stop("Colonnes manquantes. Il faut : patient, essai, COR, PE, ERR, abandon")
}

base_essais_M6 <- base_essais_M6 %>%
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

incoherences_M6 <- base_essais_M6 %>%
  filter(COR == 1 & (PE == 1 | ERR == 1))

if (nrow(incoherences_M6) > 0) {
  message("Attention : essais M6 avec COR=1 et PE/ERR=1. Ils seront traites comme corrects.")
  print(incoherences_M6)
}

base_essais_M6 <- base_essais_M6 %>%
  mutate(
    PE = if_else(COR == 1, 0, PE),
    ERR = if_else(COR == 1, 0, ERR),
    essai_reel = COR == 1 | PE == 1 | ERR == 1 | abandon == 1
  ) %>%
  filter(essai_reel) %>%
  arrange(patient, essai)

# ------------------------------------------------------------
# Exclusion complète des patients ayant abandonné à M6
# ------------------------------------------------------------
# L'identification est faite AVANT le calcul des CR.
# Ces patients ne participent ensuite ni au clustering,
# ni aux tableaux descriptifs, ni aux figures M6.

patients_abandon_M6 <- base_essais_M6 %>%
  group_by(patient) %>%
  summarise(
    abandon_M6 = any(abandon == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(abandon_M6)

print(patients_abandon_M6$patient)

cat(
  "\nNombre de patients exclus pour abandon à M6 : ",
  nrow(patients_abandon_M6),
  "\n",
  sep = ""
)

if (nrow(patients_abandon_M6) > 0) {
  cat(
    "Patients exclus : ",
    paste(patients_abandon_M6$patient, collapse = ", "),
    "\n\n",
    sep = ""
  )
}

base_essais_M6 <- base_essais_M6 %>%
  filter(!patient %in% patients_abandon_M6$patient)

sd0 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(0)
  sd(x)
}

# ============================================================
# 2. IDENTIFICATION DES CR M6
# ============================================================

identifier_CR_patient_M6 <- function(df_patient) {
  
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
      df_patient$ERR[i] == 1
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

essais_CR_tous_M6 <- base_essais_M6 %>%
  group_by(patient) %>%
  group_modify(~ identifier_CR_patient_M6(.x)) %>%
  ungroup()

patients_ref_M6 <- essais_CR_tous_M6 %>%
  distinct(patient)

# ============================================================
# 3. TABLES INTERNES NECESSAIRES AU CLUSTERING M6
# ============================================================

resume_categories_tous_M6 <- essais_CR_tous_M6 %>%
  group_by(patient) %>%
  summarise(
    nb_essais = n(),
    nb_categories_completees = sum(categorie_completee, na.rm = TRUE),
    .groups = "drop"
  )

# Table interne patient x CR nécessaire pour calculer les variables patient.
# Elle ne sera pas exportée comme fiche CR.
table_CR_base_M6 <- essais_CR_tous_M6 %>%
  filter(!is.na(CR)) %>%
  group_by(patient, CR) %>%
  summarise(
    nb_CORR = sum(COR, na.rm = TRUE),
    nb_ERR = sum(ERR, na.rm = TRUE),
    nb_PE = sum(PE, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 4. RESUME PATIENTS M6
# Les patients ayant abandonné à M6 ont déjà été exclus.
# ============================================================

resume_patient_depuis_CR_M6 <- function(table_CR_M6, patients_ref_M6) {
  
  patients_ref_M6 %>%
    left_join(resume_categories_tous_M6, by = "patient") %>%
    left_join(
      table_CR_M6 %>%
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

table_patients_base_M6 <- resume_patient_depuis_CR_M6(
  table_CR_M6 = table_CR_base_M6,
  patients_ref_M6 = patients_ref_M6
)

table_patients_analyse_M6 <- table_patients_base_M6

# ============================================================
# 5. CLUSTERING WARD.D2 M6
# Variables : nb_PE_CR_total + nb_CR_avec_PE
# Les abandons ne participent pas au clustering.
# ============================================================

variables_clustering_M6 <- c("nb_PE_CR_total", "nb_CR_avec_PE")

X_CR_M6 <- table_patients_analyse_M6 %>%
  select(all_of(variables_clustering_M6)) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale() %>%
  as.data.frame()

dist_CR_M6 <- dist(X_CR_M6, method = "euclidean")
hc_CR_M6 <- hclust(dist_CR_M6, method = "ward.D2")

# ============================================================
# 6. SILHOUETTE ET CHOIX DE K M6
# ============================================================

K_MIN_M6 <- 2
K_MAX_M6 <- min(6, nrow(X_CR_M6) - 1)

table_silhouette_M6 <- map_dfr(K_MIN_M6:K_MAX_M6, function(k) {
  
  clusters_k_M6 <- cutree(hc_CR_M6, k = k)
  sil_k_M6 <- silhouette(clusters_k_M6, dist_CR_M6)
  
  tibble(
    k = k,
    silhouette_moyenne = round(mean(sil_k_M6[, "sil_width"]), 3),
    tailles_clusters = paste(
      names(table(clusters_k_M6)),
      as.integer(table(clusters_k_M6)),
      sep = "=",
      collapse = " / "
    )
  )
})

print(table_silhouette_M6)
View(table_silhouette_M6)

plot_silhouette_M6 <- ggplot(
  table_silhouette_M6,
  aes(x = k, y = silhouette_moyenne)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = K_MIN_M6:K_MAX_M6) +
  labs(
    title = "M6 - Silhouette moyenne selon le nombre de clusters",
    x = "Nombre de clusters",
    y = "Silhouette moyenne"
  ) +
  theme_minimal()

print(plot_silhouette_M6)

ggsave(
  "silhouette_Ward_CR_M6.pdf",
  plot_silhouette_M6,
  width = 8,
  height = 5,
  dpi = 300
)

K_CHOISI_M6 <- 5

# ============================================================
# 7. ATTRIBUTION DES CLUSTERS M6
# Les clusters 1:K_CHOISI_M6 viennent de Ward.
# Aucun cluster prédéfini n'est créé pour les abandons.
# ============================================================

clusters_bruts_M6 <- cutree(hc_CR_M6, k = K_CHOISI_M6)

ordre_clusters_M6 <- table_patients_analyse_M6 %>%
  mutate(cluster_brut_M6 = clusters_bruts_M6) %>%
  group_by(cluster_brut_M6) %>%
  summarise(
    PE_moyen = mean(nb_PE_CR_total, na.rm = TRUE),
    CR_avec_PE_moyen = mean(nb_CR_avec_PE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(PE_moyen, CR_avec_PE_moyen) %>%
  mutate(cluster_M6 = row_number())

table_patients_analyse_clusterisee_M6 <- table_patients_analyse_M6 %>%
  mutate(cluster_brut_M6 = clusters_bruts_M6) %>%
  left_join(
    ordre_clusters_M6 %>%
      select(cluster_brut_M6, cluster_M6),
    by = "cluster_brut_M6"
  ) %>%
  select(-cluster_brut_M6)

table_patients_M6 <- table_patients_analyse_clusterisee_M6 %>%
  mutate(
    cluster_M6 = as.integer(cluster_M6)
  ) %>%
  select(
    patient,
    cluster_M6,
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
  arrange(cluster_M6, patient)

View(table_patients_M6)

# ============================================================
# 8. TABLE DESCRIPTIVE DES CLUSTERS M6
# 1 colonne par cluster
# ============================================================

resume_cluster_M6 <- table_patients_M6 %>%
  group_by(cluster_M6) %>%
  summarise(
    nb_patients = n(),
    
    nb_essais_moy = round(mean(nb_essais, na.rm = TRUE), 2),
    nb_essais_sd = round(sd0(nb_essais), 2),
    nb_essais_med = median(nb_essais, na.rm = TRUE),
    
    nb_categories_completees_moy = round(mean(nb_categories_completees, na.rm = TRUE), 2),
    nb_categories_completees_sd = round(sd0(nb_categories_completees), 2),
    nb_categories_completees_med = median(nb_categories_completees, na.rm = TRUE),
    
    nb_CR_moy = round(mean(nb_CR, na.rm = TRUE), 2),
    nb_CR_sd = round(sd0(nb_CR), 2),
    nb_CR_med = median(nb_CR, na.rm = TRUE),
    
    nb_CORR_CR_moy = round(mean(nb_CORR_CR_moy, na.rm = TRUE), 2),
    nb_CORR_CR_sd = round(sd0(nb_CORR_CR_moy), 2),
    nb_CORR_CR_med = median(nb_CORR_CR_moy, na.rm = TRUE),
    
    nb_ERR_CR_total_moy = round(mean(nb_ERR_CR_total, na.rm = TRUE), 2),
    nb_ERR_CR_total_sd = round(sd0(nb_ERR_CR_total), 2),
    nb_ERR_CR_total_med = median(nb_ERR_CR_total, na.rm = TRUE),
    
    nb_ERR_CR_moy_moy = round(mean(nb_ERR_CR_moy, na.rm = TRUE), 2),
    nb_ERR_CR_moy_sd = round(sd0(nb_ERR_CR_moy), 2),
    nb_ERR_CR_moy_med = median(nb_ERR_CR_moy, na.rm = TRUE),
    
    nb_PE_CR_total_moy = round(mean(nb_PE_CR_total, na.rm = TRUE), 2),
    nb_PE_CR_total_sd = round(sd0(nb_PE_CR_total), 2),
    nb_PE_CR_total_med = median(nb_PE_CR_total, na.rm = TRUE),
    
    nb_PE_CR_moy_moy = round(mean(nb_PE_CR_moy, na.rm = TRUE), 2),
    nb_PE_CR_moy_sd = round(sd0(nb_PE_CR_moy), 2),
    nb_PE_CR_moy_med = median(nb_PE_CR_moy, na.rm = TRUE),
    
    nb_CR_avec_PE_moy = round(mean(nb_CR_avec_PE, na.rm = TRUE), 2),
    nb_CR_avec_PE_sd = round(sd0(nb_CR_avec_PE), 2),
    nb_CR_avec_PE_med = median(nb_CR_avec_PE, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(cluster_M6)

table_clusters_M6 <- resume_cluster_M6 %>%
  mutate(
    cluster_M6 = paste0("cluster_", cluster_M6),
    across(-cluster_M6, as.character)
  ) %>%
  pivot_longer(
    cols = -cluster_M6,
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  pivot_wider(
    names_from = cluster_M6,
    values_from = valeur
  )

print(table_clusters_M6)
View(table_clusters_M6)

# ============================================================
# 9. FIGURES M6
# Scatter plot supprimé
# ============================================================

# ------------------------------------------------------------
# 9.1 Heatmap patient x CR M6
# ------------------------------------------------------------

max_CR_M6 <- max(table_CR_base_M6$CR, na.rm = TRUE)

if (!is.finite(max_CR_M6) || max_CR_M6 < 1) {
  max_CR_M6 <- 1
}

patients_figures_M6 <- table_patients_M6 %>%
  select(
    patient,
    cluster_M6,
    nb_PE_CR_total,
    nb_CR_avec_PE
  ) %>%
  mutate(
    patient_label = paste0(
      patient,
      " | PE M6=",
      nb_PE_CR_total,
      " | CR PE M6=",
      nb_CR_avec_PE
    )
  )

data_heatmap_CR_M6 <- patients_figures_M6 %>%
  crossing(CR = 1:max_CR_M6) %>%
  left_join(
    table_CR_base_M6 %>%
      select(patient, CR, nb_PE),
    by = c("patient", "CR")
  ) %>%
  mutate(
    nb_PE = replace_na(nb_PE, 0)
  )

ordre_patients_CR_M6 <- patients_figures_M6 %>%
  arrange(
    cluster_M6,
    nb_CR_avec_PE,
    nb_PE_CR_total,
    patient
  ) %>%
  pull(patient_label)

data_heatmap_CR_M6 <- data_heatmap_CR_M6 %>%
  mutate(
    patient_label = factor(patient_label, levels = ordre_patients_CR_M6),
    cluster_M6 = factor(cluster_M6)
  )

max_PE_CR_M6 <- max(data_heatmap_CR_M6$nb_PE, na.rm = TRUE)

pdf(
  "heatmap_PE_par_CR_et_cluster_M6.pdf",
  width = 11,
  height = 8
)

for (cl in levels(data_heatmap_CR_M6$cluster_M6)) {
  
  data_cl_M6 <- data_heatmap_CR_M6 %>%
    filter(cluster_M6 == cl)
  
  n_patients_M6 <- n_distinct(data_cl_M6$patient)
  
  p <- ggplot(
    data_cl_M6,
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
      breaks = 1:max_CR_M6,
      expand = c(0, 0)
    ) +
    scale_fill_gradient(
      low = "grey95",
      high = "black",
      limits = c(0, max_PE_CR_M6),
      name = "Nb PE M6"
    ) +
    labs(
      title = paste0("Cluster M6 ", cl, " - PE par CR"),
      subtitle = paste0(n_patients_M6, " patient(s)"),
      x = "CR M6",
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
# 9.2 Trajectoires individuelles M6
# ------------------------------------------------------------

data_changements_CR_M6 <- essais_CR_tous_M6 %>%
  filter(categorie_completee == 1) %>%
  left_join(
    patients_figures_M6,
    by = "patient"
  ) %>%
  mutate(
    x_changement = essai + 0.5
  )

data_dernier_essai_CR_M6 <- essais_CR_tous_M6 %>%
  group_by(patient) %>%
  summarise(
    dernier_essai = max(essai, na.rm = TRUE),
    .groups = "drop"
  )

data_traj_CR_M6 <- patients_figures_M6 %>%
  crossing(essai = 1:48) %>%
  left_join(
    essais_CR_tous_M6 %>%
      select(patient, essai, COR, ERR, PE, categorie_completee),
    by = c("patient", "essai")
  ) %>%
  left_join(
    data_dernier_essai_CR_M6,
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
    patient_label = factor(patient_label, levels = ordre_patients_CR_M6),
    cluster_M6 = factor(cluster_M6)
  )

data_changements_CR_M6 <- data_changements_CR_M6 %>%
  mutate(
    patient_label = factor(patient_label, levels = ordre_patients_CR_M6),
    cluster_M6 = factor(cluster_M6)
  )

pdf(
  "trajectoires_individuelles_PE_essai_par_essai_par_cluster_M6.pdf",
  width = 14,
  height = 11
)

for (cl in levels(data_traj_CR_M6$cluster_M6)) {
  
  data_cl_M6 <- data_traj_CR_M6 %>%
    filter(cluster_M6 == cl) %>%
    droplevels()
  
  changements_cl_M6 <- data_changements_CR_M6 %>%
    filter(cluster_M6 == cl) %>%
    droplevels()
  
  n_patients_M6 <- n_distinct(data_cl_M6$patient)
  
  p <- ggplot(
    data_cl_M6,
    aes(
      x = essai,
      y = PE_plot
    )
  ) +
    geom_vline(
      data = changements_cl_M6,
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
      data = data_cl_M6 %>% filter(type_reponse == "non_PE"),
      aes(x = essai, y = PE_plot),
      color = "grey70",
      size = 1.1,
      alpha = 0.8
    ) +
    geom_point(
      data = data_cl_M6 %>% filter(type_reponse == "PE"),
      aes(x = essai, y = PE_plot),
      color = "black",
      size = 2.2
    ) +
    geom_segment(
      data = data_cl_M6 %>% filter(type_reponse == "PE"),
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
      title = paste0("Cluster M6 ", cl, " - trajectoires PE essai par essai"),
      subtitle = paste0(
        n_patients_M6,
        " patient(s) - noir = PE ; gris = non-PE ; bleu = categorie completee"
      ),
      x = "Essai M6",
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
# 10. DENDROGRAMME M6
# Les patients ayant abandonné à M6 ont été exclus en amont.
# ============================================================

pdf(
  "dendrogramme_Ward_CR_M6.pdf",
  width = 12,
  height = 8
)

plot(
  hc_CR_M6,
  labels = table_patients_analyse_M6$patient,
  main = "M6 - Dendrogramme Ward.D2 - variables CR",
  xlab = "Patients",
  ylab = "Distance"
)

rect.hclust(hc_CR_M6, k = K_CHOISI_M6, border = 2:(K_CHOISI_M6 + 1))

dev.off()

# ============================================================
# 11. EXPORT EXCEL M6
# Pas de fiche CR exportée
# ============================================================

table_patients_export_M6 <- table_patients_M6 %>%
  rename(
    numero_patient = patient,
    numero_cluster_M6 = cluster_M6
  )

table_clusters_export_M6 <- table_clusters_M6

print(table_patients_export_M6)
print(table_clusters_export_M6)

View(table_patients_export_M6)
View(table_clusters_export_M6)

write_xlsx(
  list(
    patients_M6 = table_patients_export_M6,
    clusters_M6 = table_clusters_export_M6,
    silhouette_M6 = table_silhouette_M6
  ),
  "clustering_MCST_M6_tabl.xlsx"
)
