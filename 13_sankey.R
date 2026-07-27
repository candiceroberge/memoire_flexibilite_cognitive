library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggalluvial)
library(writexl)

# ============================================================
# SANKEY / ALLUVIAL
# Trajectoire des patients entre clusters M0 et M6
#
# Les fichiers de clustering M0 et M6 ne contiennent que les
# patients ayant un clustering valide à chaque temps.
# L'inner_join conserve donc uniquement les patients disposant
# d'un cluster à M0 ET à M6 et exclut de fait les abandons
# survenus à l'un ou l'autre temps.
# ============================================================

# ------------------------------------------------------------
# 0. Parametres
# ------------------------------------------------------------

FICHIER_M0 <- "clustering_MCST_tabl.xlsx"
FICHIER_M6 <- "clustering_MCST_M6_tabl.xlsx"

# ------------------------------------------------------------
# 1. Fonctions utiles
# ------------------------------------------------------------

normaliser_patient <- function(x) {
  x <- as.character(x)
  if_else(
    !is.na(x) & str_detect(x, "^\\d+$"),
    str_pad(x, width = 3, side = "left", pad = "0"),
    x
  )
}

ordonner_clusters <- function(x) {
  ux <- unique(as.character(x))
  
  if (all(str_detect(ux, "^\\d+$"))) {
    as.character(sort(as.numeric(ux)))
  } else {
    sort(ux)
  }
}

# ------------------------------------------------------------
# 2. Import M0
# ------------------------------------------------------------

feuille_M0 <- if ("patients" %in% excel_sheets(FICHIER_M0)) {
  "patients"
} else {
  excel_sheets(FICHIER_M0)[1]
}

patients_M0 <- read_excel(FICHIER_M0, sheet = feuille_M0)

if ("patient" %in% names(patients_M0)) {
  patients_M0 <- patients_M0 %>%
    rename(numero_patient = patient)
}

if ("cluster" %in% names(patients_M0)) {
  patients_M0 <- patients_M0 %>%
    rename(numero_cluster = cluster)
}

patients_M0 <- patients_M0 %>%
  mutate(
    numero_patient = normaliser_patient(numero_patient),
    cluster_M0 = as.character(numero_cluster)
  ) %>%
  select(numero_patient, cluster_M0)

# ------------------------------------------------------------
# 3. Import M6
# ------------------------------------------------------------

feuille_M6 <- case_when(
  "patients_M6" %in% excel_sheets(FICHIER_M6) ~ "patients_M6",
  "patients" %in% excel_sheets(FICHIER_M6) ~ "patients",
  TRUE ~ excel_sheets(FICHIER_M6)[1]
)

patients_M6 <- read_excel(FICHIER_M6, sheet = feuille_M6)

if ("patient" %in% names(patients_M6)) {
  patients_M6 <- patients_M6 %>%
    rename(numero_patient = patient)
}

if ("cluster" %in% names(patients_M6)) {
  patients_M6 <- patients_M6 %>%
    rename(numero_cluster_M6 = cluster)
}

if ("numero_cluster" %in% names(patients_M6) &&
    !"numero_cluster_M6" %in% names(patients_M6)) {
  patients_M6 <- patients_M6 %>%
    rename(numero_cluster_M6 = numero_cluster)
}

if ("cluster_table_M6" %in% names(patients_M6) &&
    !"numero_cluster_M6" %in% names(patients_M6)) {
  patients_M6 <- patients_M6 %>%
    rename(numero_cluster_M6 = cluster_table_M6)
}

patients_M6 <- patients_M6 %>%
  mutate(
    numero_patient = normaliser_patient(numero_patient),
    cluster_M6 = as.character(numero_cluster_M6)
  ) %>%
  select(numero_patient, cluster_M6)

# ------------------------------------------------------------
# 4. Jointure M0 x M6
# ------------------------------------------------------------

trajectoires_patients <- patients_M0 %>%
  inner_join(patients_M6, by = "numero_patient") %>%
  filter(
    !is.na(cluster_M0),
    !is.na(cluster_M6),
    cluster_M0 != "",
    cluster_M6 != ""
  )

cat(
  "\nNombre de patients représentés dans le Sankey : ",
  nrow(trajectoires_patients),
  "\n",
  sep = ""
)

niveaux_M0 <- ordonner_clusters(trajectoires_patients$cluster_M0)
niveaux_M6 <- ordonner_clusters(trajectoires_patients$cluster_M6)

trajectoires_patients <- trajectoires_patients %>%
  mutate(
    cluster_M0 = factor(cluster_M0, levels = niveaux_M0),
    cluster_M6 = factor(cluster_M6, levels = niveaux_M6)
  )

# ------------------------------------------------------------
# 5. Table de transitions
# ------------------------------------------------------------

table_transitions <- trajectoires_patients %>%
  count(cluster_M0, cluster_M6, name = "n") %>%
  arrange(cluster_M0, cluster_M6)

print(table_transitions)
View(table_transitions)

# ------------------------------------------------------------
# 6. Sankey / alluvial avec effectifs dans les clusters
# ------------------------------------------------------------

plot_sankey_clusters <- ggplot(
  table_transitions,
  aes(
    axis1 = cluster_M0,
    axis2 = cluster_M6,
    y = n
  )
) +
  geom_alluvium(
    aes(fill = cluster_M0),
    width = 0.2,
    alpha = 0.8
  ) +
  geom_stratum(
    width = 0.2,
    fill = "grey85",
    color = "grey40"
  ) +
  geom_text(
    stat = "stratum",
    aes(
      label = paste0(
        "Cluster ",
        after_stat(stratum),
        "\n(n = ",
        after_stat(count),
        ")"
      )
    ),
    size = 4
  ) +
  scale_x_discrete(
    limits = c("M0", "M6"),
    expand = c(0.15, 0.15)
  ) +
  labs(
    title = "Évolution des profils de flexibilité cognitive entre M0 et M6",
    subtitle = paste0(
      "N = ",
      nrow(trajectoires_patients),
      " patients disposant d'un cluster à M0 et à M6"
    ),
    x = NULL,
    y = NULL,
    fill = "Cluster M0"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(plot_sankey_clusters)

# ------------------------------------------------------------
# 7. Sauvegarde
# ------------------------------------------------------------


ggsave(
  "sankey_trajectoire_clusters_M0_M6.pdf",
  plot_sankey_clusters,
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 8. Export Excel des transitions
# ------------------------------------------------------------

write_xlsx(
  list(
    trajectoires_patients = trajectoires_patients,
    transitions = table_transitions
  ),
  "trajectoires_clusters_M0_M6.xlsx"
)
