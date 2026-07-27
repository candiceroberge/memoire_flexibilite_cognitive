# ============================================================
# 01 - CALCUL DES CENTROIDES ET DE LA DISPERSION DES EP A M0
# + GRAPHIQUE GLOBAL AVEC TOUS LES PATIENTS ANALYSES
#
# Un point = centroide temporel d'un patient
# Un segment = centroide +/- une dispersion temporelle
# La couleur = cluster M0 (1 a 5)
# Les patients ayant abandonne sont absents du fichier source
# et ne sont donc pas inclus dans l'analyse ni dans le graphique.
# Une croix = patient sans aucune EP
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)

# ------------------------------------------------------------
# 0. Parametres
# ------------------------------------------------------------

FICHIER_CLUSTER <- "clustering_MCST_tabl.xlsx"
FEUILLE_PATIENTS <- "patients"
FEUILLE_CR <- "resume_CR"


FICHIER_SORTIE <- "centroides_dispersion_EP_M0.xlsx"
FIGURE_PDF <- "centroides_dispersion_EP_tous_patients_M0.pdf"
FIGURE_PNG <- "centroides_dispersion_EP_tous_patients_M0.png"

# ------------------------------------------------------------
# 1. Fonctions utiles
# ------------------------------------------------------------

as_numeric_clean <- function(x) {
  suppressWarnings(
    as.numeric(
      str_replace(
        str_trim(as.character(x)),
        ",",
        "."
      )
    )
  )
}

normaliser_patient <- function(x) {
  x <- str_extract(as.character(x), "\\d+")
  str_pad(x, width = 3, side = "left", pad = "0")
}

trouver_colonne <- function(df, candidats, nom_variable) {
  colonne <- candidats[candidats %in% names(df)][1]
  
  if (is.na(colonne)) {
    stop(
      paste0(
        "\nColonne introuvable pour : ", nom_variable,
        "\nColonnes recherchees : ", paste(candidats, collapse = ", "),
        "\n\nColonnes disponibles :\n", paste(names(df), collapse = ", ")
      )
    )
  }
  
  colonne
}

# ------------------------------------------------------------
# 2. Verification des feuilles
# ------------------------------------------------------------

feuilles_cluster <- excel_sheets(FICHIER_CLUSTER)

if (!(FEUILLE_PATIENTS %in% feuilles_cluster)) {
  stop(
    paste0(
      "La feuille '", FEUILLE_PATIENTS,
      "' est absente du fichier ", FICHIER_CLUSTER, "."
    )
  )
}

if (!(FEUILLE_CR %in% feuilles_cluster)) {
  stop(
    paste0(
      "La feuille '", FEUILLE_CR,
      "' est absente du fichier ", FICHIER_CLUSTER, "."
    )
  )
}

# ------------------------------------------------------------
# 3. Import des patients et des clusters
# ------------------------------------------------------------

patients_brut <- read_excel(
  FICHIER_CLUSTER,
  sheet = FEUILLE_PATIENTS
)

col_patient_patients <- trouver_colonne(
  patients_brut,
  c("numero_patient", "patient", "ID", "id"),
  "numero du patient dans la feuille patients"
)

col_cluster <- trouver_colonne(
  patients_brut,
  c("numero_cluster", "cluster", "cluster_M0", "numero_cluster_m0"),
  "numero du cluster"
)

clusters <- patients_brut %>%
  transmute(
    numero_patient = normaliser_patient(.data[[col_patient_patients]]),
    numero_cluster = str_trim(as.character(.data[[col_cluster]]))
  ) %>%
  filter(
    !is.na(numero_patient),
    !is.na(numero_cluster),
    numero_cluster != ""
  ) %>%
  distinct(numero_patient, .keep_all = TRUE) %>%
  mutate(
    groupe_cluster = paste0("Cluster ", numero_cluster),
    groupe_cluster = factor(
      groupe_cluster,
      levels = c(
        "Cluster 1",
        "Cluster 2",
        "Cluster 3",
        "Cluster 4",
        "Cluster 5"
      )
    )
  )


# ------------------------------------------------------------
# 4. Import de la table patient x changement de regle
# ------------------------------------------------------------

CR_brut <- read_excel(
  FICHIER_CLUSTER,
  sheet = FEUILLE_CR
)

col_patient_CR <- trouver_colonne(
  CR_brut,
  c("patient", "numero_patient", "ID", "id"),
  "numero du patient dans la table CR"
)

col_rang_CR <- trouver_colonne(
  CR_brut,
  c("CR", "rang_CR", "numero_CR", "cr"),
  "rang du changement de regle"
)

col_EP_CR <- trouver_colonne(
  CR_brut,
  c("nb_PE", "nb_PE_CR", "nb_PE_bloc", "nombre_PE"),
  "nombre d'EP pour le changement de regle"
)

donnees_CR <- CR_brut %>%
  transmute(
    numero_patient = normaliser_patient(.data[[col_patient_CR]]),
    rang_CR = as.integer(as_numeric_clean(.data[[col_rang_CR]])),
    nb_PE_CR = as_numeric_clean(.data[[col_EP_CR]])
  ) %>%
  filter(
    !is.na(numero_patient),
    !is.na(rang_CR),
    rang_CR >= 1,
    !is.na(nb_PE_CR),
    nb_PE_CR >= 0
  ) %>%
  arrange(numero_patient, rang_CR)

if (nrow(donnees_CR) == 0) {
  stop("Aucune donnee exploitable n'a ete trouvee dans la feuille resume_CR.")
}

CR_MAX <- max(donnees_CR$rang_CR, na.rm = TRUE)

# ------------------------------------------------------------
# 5. Calcul du centroide et de la dispersion ponderes
# ------------------------------------------------------------

parametres_calcules <- donnees_CR %>%
  group_by(numero_patient) %>%
  summarise(
    nb_CR_observes = n_distinct(rang_CR),
    
    nb_PE_total = sum(nb_PE_CR, na.rm = TRUE),
    
    nb_PE_CR_moy = mean(nb_PE_CR, na.rm = TRUE),
    
    centroide_temporel_PE = if_else(
      nb_PE_total > 0,
      sum(rang_CR * nb_PE_CR, na.rm = TRUE) / nb_PE_total,
      NA_real_
    ),
    
    dispersion_temporelle_PE = if_else(
      nb_PE_total > 0,
      sqrt(
        sum(
          nb_PE_CR *
            (
              rang_CR -
                sum(rang_CR * nb_PE_CR, na.rm = TRUE) / nb_PE_total
            )^2,
          na.rm = TRUE
        ) / nb_PE_total
      ),
      NA_real_
    ),
    
    .groups = "drop"
  )

# On part de la table des patients afin de conserver tous les patients
# analyses, y compris ceux n'ayant commis aucune EP.
profils_temporels <- clusters %>%
  left_join(
    parametres_calcules,
    by = "numero_patient"
  ) %>%
  mutate(
    nb_CR_observes = coalesce(nb_CR_observes, 0L),
    nb_PE_total = coalesce(nb_PE_total, 0),
    nb_PE_CR_moy = coalesce(nb_PE_CR_moy, 0),
    
    rang_centroide_arrondi = if_else(
      nb_PE_total > 0,
      floor(centroide_temporel_PE + 0.5),
      NA_real_
    ),
    
    groupe_centroide_PE = case_when(
      nb_PE_total == 0 ~ "Aucune EP",
      rang_centroide_arrondi >= 6 ~ "CR 6 et plus",
      TRUE ~ paste0("CR ", as.integer(rang_centroide_arrondi))
    ),
    
    groupe_centroide_PE = factor(
      groupe_centroide_PE,
      levels = c(
        "CR 1",
        "CR 2",
        "CR 3",
        "CR 4",
        "CR 5",
        "CR 6 et plus",
        "Aucune EP"
      )
    )
  ) %>%
  arrange(groupe_cluster, numero_patient)

# ------------------------------------------------------------
# 6. Tableau descriptif par groupe
# ------------------------------------------------------------

resume_clusters <- profils_temporels %>%
  group_by(groupe_cluster) %>%
  summarise(
    n_patients = n(),
    n_sans_EP = sum(nb_PE_total == 0, na.rm = TRUE),
    centroide_moyen = mean(centroide_temporel_PE, na.rm = TRUE),
    centroide_median = median(centroide_temporel_PE, na.rm = TRUE),
    centroide_ecart_type = sd(centroide_temporel_PE, na.rm = TRUE),
    dispersion_moyenne = mean(dispersion_temporelle_PE, na.rm = TRUE),
    dispersion_mediane = median(dispersion_temporelle_PE, na.rm = TRUE),
    dispersion_ecart_type = sd(dispersion_temporelle_PE, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 7. Export des resultats
# ------------------------------------------------------------

write_xlsx(
  list(
    parametres_patients = profils_temporels,
    resume_clusters = resume_clusters,
    donnees_CR = donnees_CR
  ),
  path = FICHIER_SORTIE
)

# ------------------------------------------------------------
# 8. Preparation du graphique global
# ------------------------------------------------------------

donnees_graph <- profils_temporels %>%
  mutate(
    patient_affiche = paste0("P", numero_patient),
    
    position_graphique = if_else(
      nb_PE_total > 0,
      centroide_temporel_PE,
      0
    ),
    
    borne_inferieure = if_else(
      nb_PE_total > 0,
      pmax(1, centroide_temporel_PE - dispersion_temporelle_PE),
      NA_real_
    ),
    
    borne_superieure = if_else(
      nb_PE_total > 0,
      pmin(CR_MAX, centroide_temporel_PE + dispersion_temporelle_PE),
      NA_real_
    ),
    
    statut_EP = if_else(
      nb_PE_total > 0,
      "Au moins une EP",
      "Aucune EP"
    )
  ) %>%
  # Ordre chronologique global : centroide precoce vers centroide tardif.
  # Les patients sans EP sont places en bas du graphique.
  arrange(
    nb_PE_total == 0,
    centroide_temporel_PE,
    groupe_cluster,
    numero_patient
  )

ordre_patients <- rev(unique(donnees_graph$patient_affiche))

donnees_graph <- donnees_graph %>%
  mutate(
    patient_affiche = factor(
      patient_affiche,
      levels = ordre_patients
    ),
    statut_EP = factor(
      statut_EP,
      levels = c("Au moins une EP", "Aucune EP")
    )
  )

# ------------------------------------------------------------
# 9. Construction du graphique global
# ------------------------------------------------------------

couleurs_clusters <- c(
  "Cluster 1" = "#1B9E77",
  "Cluster 2" = "#D95F02",
  "Cluster 3" = "#7570B3",
  "Cluster 4" = "#E7298A",
  "Cluster 5" = "#66A61E"
)

graph_centroides_dispersion <- ggplot() +
  
  geom_vline(
    xintercept = seq_len(CR_MAX),
    color = "grey88",
    linewidth = 0.35
  ) +
  
  # Segment horizontal : centroide +/- une dispersion
  geom_segment(
    data = donnees_graph %>% filter(nb_PE_total > 0),
    aes(
      x = borne_inferieure,
      xend = borne_superieure,
      y = patient_affiche,
      yend = patient_affiche,
      color = groupe_cluster
    ),
    linewidth = 1.15,
    alpha = 0.65
  ) +
  
  # Point du centroide ; croix a x = 0 pour les patients sans EP
  geom_point(
    data = donnees_graph,
    aes(
      x = position_graphique,
      y = patient_affiche,
      color = groupe_cluster,
      shape = statut_EP
    ),
    size = 3,
    stroke = 1.05
  ) +
  
  scale_x_continuous(
    breaks = 0:CR_MAX,
    labels = c(
      "Aucune\nEP",
      paste0("CR ", seq_len(CR_MAX))
    ),
    limits = c(-0.15, CR_MAX + 0.15),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  scale_color_manual(
    name = "Groupe à M0",
    values = couleurs_clusters,
    drop = FALSE
  ) +
  
  scale_shape_manual(
    name = "Présence d’EP",
    values = c(
      "Au moins une EP" = 16,
      "Aucune EP" = 4
    ),
    drop = FALSE
  ) +
  
  labs(
    title = "Centroïde temporel et dispersion des erreurs persévératives à M0",
    subtitle = paste0(
      "Tous les patients analysés sont présentés sur un même graphique. ",
      "Le point indique le centroïde et le segment le centroïde ± une dispersion."
    ),
    x = "Temporalité des erreurs persévératives",
    y = "Patient"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 6.5),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 10)
  )

print(graph_centroides_dispersion)

# ------------------------------------------------------------
# 10. Export du graphique
# ------------------------------------------------------------

# Hauteur adaptee au nombre total de patients afin de conserver
# des identifiants lisibles sur l'axe vertical.
HAUTEUR_FIGURE <- max(11, 0.23 * nrow(donnees_graph))

ggsave(
  filename = FIGURE_PDF,
  plot = graph_centroides_dispersion,
  width = 10,
  height = HAUTEUR_FIGURE
)

ggsave(
  filename = FIGURE_PNG,
  plot = graph_centroides_dispersion,
  width = 10,
  height = HAUTEUR_FIGURE,
  dpi = 300
)

cat(
  "\nFichiers crees :\n",
  "- ", FICHIER_SORTIE, "\n",
  "- ", FIGURE_PDF, "\n",
  "- ", FIGURE_PNG, "\n",
  sep = ""
)
