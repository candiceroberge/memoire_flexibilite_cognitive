# ============================================================
# GRAPHIQUE PAR CLUSTER
# NOMBRE MOYEN D'EP PAR CR ET DELTA MADRS
#
# Un point = un patient
#
# Axe x   = nombre moyen de PE par changement de règle
# Axe y   = delta MADRS du patient
# Couleur = centroïde temporel des PE
#           rang du CR autour duquel les PE sont concentrées
# Taille  = dispersion temporelle des PE autour du centroïde
#
# Les patients sans aucune PE sont affichés en gris.
#
# Nomenclature dans les données et le code : PE
# Nomenclature dans le graphique : EP
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tibble)

# ------------------------------------------------------------
# 0. Fichiers et feuilles
# ------------------------------------------------------------

FICHIER_CLUSTER <- "clustering_MCST_tabl.xlsx"
FICHIER_MADRS   <- "scores_MADRS.xlsx"

FEUILLE_PATIENTS <- "patients"
FEUILLE_CR       <- "resume_CR"
FEUILLE_MADRS    <- 1

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
  
  x <- str_extract(
    as.character(x),
    "\\d+"
  )
  
  str_pad(
    x,
    width = 3,
    side = "left",
    pad = "0"
  )
}

trouver_colonne <- function(df, candidats, nom_variable) {
  
  colonne <- candidats[candidats %in% names(df)][1]
  
  if (is.na(colonne)) {
    
    stop(
      paste0(
        "\nColonne introuvable pour : ",
        nom_variable,
        "\nColonnes recherchées : ",
        paste(candidats, collapse = ", "),
        "\n\nColonnes disponibles :\n",
        paste(names(df), collapse = ", ")
      )
    )
  }
  
  colonne
}


# ------------------------------------------------------------
# 1bis. Corrélation de Spearman ajustée
# Ajustement sur âge, sexe et nombre d'années d'études
# ------------------------------------------------------------

faire_spearman_ajuste <- function(df) {
  
  df_clean <- df %>%
    transmute(
      nb_PE_CR_moy = as.numeric(nb_PE_CR_moy),
      delta_MADRS = as.numeric(delta_MADRS),
      age = as.numeric(age),
      sexe = factor(sexe),
      etudes = as.numeric(etudes)
    ) %>%
    filter(
      !is.na(nb_PE_CR_moy),
      !is.na(delta_MADRS),
      !is.na(age),
      !is.na(sexe),
      !is.na(etudes)
    )
  
  n <- nrow(df_clean)
  covariables <- c("age", "sexe", "etudes")
  
  covariables_valides <- covariables[
    sapply(covariables, function(v) length(unique(df_clean[[v]])) > 1)
  ]
  
  n_min <- length(covariables_valides) + 5
  
  if (
    n < n_min ||
    length(unique(df_clean$nb_PE_CR_moy)) < 3 ||
    length(unique(df_clean$delta_MADRS)) < 3
  ) {
    return(
      tibble(
        n_spearman = n,
        rho_spearman_ajuste = NA_real_,
        p_value_spearman_ajuste = NA_real_,
        statut_spearman = "non estimable de façon fiable"
      )
    )
  }
  
  df_rank <- df_clean %>%
    mutate(
      rang_PE = rank(nb_PE_CR_moy, ties.method = "average"),
      rang_MADRS = rank(delta_MADRS, ties.method = "average")
    )
  
  formule_PE <- as.formula(
    paste("rang_PE ~", paste(covariables_valides, collapse = " + "))
  )
  
  formule_MADRS <- as.formula(
    paste("rang_MADRS ~", paste(covariables_valides, collapse = " + "))
  )
  
  residus_PE <- residuals(lm(formule_PE, data = df_rank))
  residus_MADRS <- residuals(lm(formule_MADRS, data = df_rank))
  
  test <- cor.test(residus_PE, residus_MADRS, method = "pearson")
  
  tibble(
    n_spearman = n,
    rho_spearman_ajuste = unname(test$estimate),
    p_value_spearman_ajuste = test$p.value,
    statut_spearman = "estimé"
  )
}

# ------------------------------------------------------------
# 2. Vérification des feuilles du fichier clustering
# ------------------------------------------------------------

feuilles_cluster <- excel_sheets(FICHIER_CLUSTER)

if (!(FEUILLE_PATIENTS %in% feuilles_cluster)) {
  
  stop(
    paste0(
      "La feuille '",
      FEUILLE_PATIENTS,
      "' n'existe pas dans ",
      FICHIER_CLUSTER,
      ".\n\nFeuilles disponibles :\n",
      paste(feuilles_cluster, collapse = ", ")
    )
  )
}

if (!(FEUILLE_CR %in% feuilles_cluster)) {
  
  stop(
    paste0(
      "La feuille '",
      FEUILLE_CR,
      "' n'existe pas dans ",
      FICHIER_CLUSTER,
      ".\n\nFeuilles disponibles :\n",
      paste(feuilles_cluster, collapse = ", "),
      "\n\nDans le script de clustering, l'export doit contenir :\n",
      "resume_CR = table_CR_base"
    )
  )
}

# ------------------------------------------------------------
# 3. Import des clusters
# ------------------------------------------------------------

patients_brut <- read_excel(
  FICHIER_CLUSTER,
  sheet = FEUILLE_PATIENTS
)

col_patient_patients <- trouver_colonne(
  patients_brut,
  c(
    "numero_patient",
    "patient",
    "ID",
    "id"
  ),
  "numéro du patient dans la feuille patients"
)


col_cluster <- trouver_colonne(
  patients_brut,
  c(
    "numero_cluster",
    "cluster",
    "cluster_M0",
    "numero_cluster_m0"
  ),
  "numéro du cluster"
)

clusters <- patients_brut %>%
  transmute(
    
    numero_patient = normaliser_patient(
      .data[[col_patient_patients]]
    ),
    
    numero_cluster = factor(
      .data[[col_cluster]]
    )
  ) %>%
  filter(
    !is.na(numero_patient),
    !is.na(numero_cluster)
  ) %>%
  distinct(
    numero_patient,
    .keep_all = TRUE
  )

# ------------------------------------------------------------
# 4. Import de la table patient × CR
#
# Colonnes attendues dans la feuille resume_CR :
# patient, CR, nb_CORR, nb_ERR, nb_PE, abandon
# ------------------------------------------------------------

donnees_CR_brut <- read_excel(
  FICHIER_CLUSTER,
  sheet = FEUILLE_CR
)

col_patient_CR <- trouver_colonne(
  donnees_CR_brut,
  c(
    "patient",
    "numero_patient",
    "ID",
    "id"
  ),
  "numéro du patient dans la table CR"
)

col_rang_CR <- trouver_colonne(
  donnees_CR_brut,
  c(
    "CR",
    "rang_CR",
    "numero_CR",
    "cr"
  ),
  "rang du changement de règle"
)

col_PE_CR <- trouver_colonne(
  donnees_CR_brut,
  c(
    "nb_PE",
    "nb_PE_CR",
    "nb_PE_bloc",
    "nombre_PE"
  ),
  "nombre de PE pour le changement de règle"
)

donnees_CR <- donnees_CR_brut %>%
  transmute(
    
    numero_patient = normaliser_patient(
      .data[[col_patient_CR]]
    ),
    
    rang_CR = as.integer(
      as_numeric_clean(
        .data[[col_rang_CR]]
      )
    ),
    
    nb_PE_CR = as_numeric_clean(
      .data[[col_PE_CR]]
    )
  ) %>%
  filter(
    !is.na(numero_patient),
    !is.na(rang_CR),
    rang_CR >= 1,
    !is.na(nb_PE_CR),
    nb_PE_CR >= 0
  ) %>%
  arrange(
    numero_patient,
    rang_CR
  )

# ------------------------------------------------------------
# 5. Calcul des paramètres temporels des PE
#
# Centroïde temporel :
# somme(rang du CR × nombre de PE) / nombre total de PE
#
# Dispersion temporelle :
# écart-type pondéré des rangs de CR autour du centroïde
# ------------------------------------------------------------

profils_temporels <- donnees_CR %>%
  group_by(numero_patient) %>%
  summarise(
    
    nb_CR_observes = n_distinct(rang_CR),
    
    nb_PE_total = sum(
      nb_PE_CR,
      na.rm = TRUE
    ),
    
    nb_PE_CR_moy = mean(
      nb_PE_CR,
      na.rm = TRUE
    ),
    
    centroide_temporel_PE = if_else(
      
      nb_PE_total > 0,
      
      sum(
        rang_CR * nb_PE_CR,
        na.rm = TRUE
      ) / nb_PE_total,
      
      NA_real_
    ),
    
    dispersion_temporelle_PE = if_else(
      
      nb_PE_total > 0,
      
      sqrt(
        sum(
          nb_PE_CR *
            (
              rang_CR -
                (
                  sum(
                    rang_CR * nb_PE_CR,
                    na.rm = TRUE
                  ) / nb_PE_total
                )
            )^2,
          na.rm = TRUE
        ) / nb_PE_total
      ),
      
      NA_real_
    ),
    
    .groups = "drop"
  )

# ------------------------------------------------------------
# 6. Catégorie descriptive du centroïde temporel
# ------------------------------------------------------------

profils_temporels <- profils_temporels %>%
  mutate(
    groupe_centroide_PE = case_when(
      nb_PE_total == 0 ~ "Aucune EP",
      centroide_temporel_PE < 1.5 ~ "CR 1",
      centroide_temporel_PE < 2.5 ~ "CR 2",
      centroide_temporel_PE < 3.5 ~ "CR 3",
      centroide_temporel_PE < 4.5 ~ "CR 4",
      centroide_temporel_PE < 5.5 ~ "CR 5",
      centroide_temporel_PE >= 5.5 ~ "CR 6 et plus",
      TRUE ~ NA_character_
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
  )
# ------------------------------------------------------------
# 7. Import des données MADRS
# ------------------------------------------------------------

madrs_brut <- read_excel(
  FICHIER_MADRS,
  sheet = FEUILLE_MADRS
)

col_patient_MADRS <- trouver_colonne(
  madrs_brut,
  c(
    "numero_patient",
    "patient",
    "ID",
    "id"
  ),
  "identifiant du patient dans le fichier MADRS"
)

col_delta_MADRS <- trouver_colonne(
  madrs_brut,
  c(
    "delta_MADRS",
    "Delta MADRS",
    "delta MADRS",
    "Delta_MADRS",
    "DeltaMADRS"
  ),
  "delta MADRS"
)

col_age_M0 <- trouver_colonne(
  madrs_brut,
  c("age_M0", "Age_M0", "age", "Age"),
  "âge à M0"
)

col_sexe_M0 <- trouver_colonne(
  madrs_brut,
  c("sexe_M0", "Sexe_M0", "sexe", "Sexe"),
  "sexe à M0"
)

col_etudes <- trouver_colonne(
  madrs_brut,
  c("Etudes", "etudes", "études", "niveau_etudes", "niveau études"),
  "nombre d'années d'études"
)

madrs <- madrs_brut %>%
  transmute(
    
    numero_patient = normaliser_patient(
      .data[[col_patient_MADRS]]
    ),
    
    delta_MADRS = as_numeric_clean(
      .data[[col_delta_MADRS]]
    ),
    
    age = as_numeric_clean(.data[[col_age_M0]]),
    sexe = factor(.data[[col_sexe_M0]]),
    etudes = as_numeric_clean(.data[[col_etudes]])
  ) %>%
  filter(
    !is.na(numero_patient),
    !is.na(delta_MADRS)
  ) %>%
  distinct(
    numero_patient,
    .keep_all = TRUE
  )

# ------------------------------------------------------------
# 8. Jointure finale
# ------------------------------------------------------------

donnees_analyse <- profils_temporels %>%
  inner_join(
    clusters,
    by = "numero_patient"
  ) %>%
  inner_join(
    madrs,
    by = "numero_patient"
  ) %>%
  arrange(
    numero_cluster,
    numero_patient
  )

cat(
  "\nNombre total de patients après jointure : ",
  nrow(donnees_analyse),
  "\n",
  sep = ""
)

cat(
  "Patients avec au moins une PE : ",
  sum(donnees_analyse$nb_PE_total > 0),
  "\n",
  sep = ""
)

cat(
  "Patients sans aucune PE : ",
  sum(donnees_analyse$nb_PE_total == 0),
  "\n\n",
  sep = ""
)

cat("Effectifs par cluster :\n")

print(
  donnees_analyse %>%
    count(
      numero_cluster,
      name = "n_patients"
    )
)

# ------------------------------------------------------------
# 9. Séparation des patients avec et sans PE
# ------------------------------------------------------------

donnees_avec_PE <- donnees_analyse %>%
  filter(
    nb_PE_total > 0,
    !is.na(centroide_temporel_PE),
    !is.na(dispersion_temporelle_PE)
  )

donnees_sans_PE <- donnees_analyse %>%
  filter(
    nb_PE_total == 0
  )

# ------------------------------------------------------------
# 10. Spearman ajusté et effectifs affichés dans chaque facette
# ------------------------------------------------------------

resultats_spearman_clusters <- donnees_analyse %>%
  group_by(numero_cluster) %>%
  group_modify(~ faire_spearman_ajuste(.x)) %>%
  ungroup()

labels_n <- donnees_analyse %>%
  group_by(numero_cluster) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  left_join(
    resultats_spearman_clusters,
    by = "numero_cluster"
  ) %>%
  mutate(
    label = if_else(
      is.na(rho_spearman_ajuste),
      paste0(
        "n = ", n,
        "\nSpearman ajusté : non estimable"
      ),
      paste0(
        "n = ", n,
        "\nρs ajusté = ", sprintf("%.2f", rho_spearman_ajuste),
        "\np = ",
        if_else(
          p_value_spearman_ajuste < 0.001,
          "< 0,001",
          str_replace(
            sprintf("%.3f", p_value_spearman_ajuste),
            "\\.",
            ","
          )
        )
      )
    ),
    x = -Inf,
    y = Inf
  )

print(resultats_spearman_clusters)

# ------------------------------------------------------------
# 11. Graphique illustratif avec ajustements
# linéaire, quadratique et cubique
# ------------------------------------------------------------

# Données utilisables pour les ajustements
donnees_courbes <- donnees_analyse %>%
  filter(
    !is.na(nb_PE_CR_moy),
    !is.na(delta_MADRS)
  )

# ------------------------------------------------------------
# 11.1 Sélection des clusters dans lesquels chaque courbe
# peut être estimée
# ------------------------------------------------------------

# Régression linéaire :
# au moins 2 valeurs distinctes sur l'axe x
donnees_lineaires <- donnees_courbes %>%
  group_by(numero_cluster) %>%
  filter(
    n() >= 2,
    n_distinct(nb_PE_CR_moy) >= 2
  ) %>%
  ungroup()

# Régression quadratique :
# au moins 3 valeurs distinctes sur l'axe x
donnees_quadratiques <- donnees_courbes %>%
  group_by(numero_cluster) %>%
  filter(
    n() >= 3,
    n_distinct(nb_PE_CR_moy) >= 3
  ) %>%
  ungroup()

# Régression cubique :
# au moins 4 valeurs distinctes sur l'axe x
donnees_cubiques <- donnees_courbes %>%
  group_by(numero_cluster) %>%
  filter(
    n() >= 4,
    n_distinct(nb_PE_CR_moy) >= 4
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 11.2 Construction du graphique
# ------------------------------------------------------------

graph_centroide_temporel <- ggplot() +
  
  geom_hline(
    yintercept = 0,
    color = "grey70",
    linewidth = 0.5
  ) +
  
  # Patients ayant commis au moins une EP
  geom_point(
    data = donnees_avec_PE,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS,
      color = groupe_centroide_PE,
      size = dispersion_temporelle_PE
    ),
    alpha = 0.8
  ) +
  
  # Patients n'ayant commis aucune EP
  geom_point(
    data = donnees_sans_PE,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS
    ),
    inherit.aes = FALSE,
    color = "grey55",
    size = 3.5,
    alpha = 0.85
  ) +
  
  # ----------------------------------------------------------
# Droite de régression linéaire
# ----------------------------------------------------------

geom_smooth(
  data = donnees_lineaires,
  aes(
    x = nb_PE_CR_moy,
    y = delta_MADRS,
    linetype = "Linéaire"
  ),
  method = "lm",
  formula = y ~ x,
  se = FALSE,
  color = "black",
  linewidth = 0.8
) +
  
  # ----------------------------------------------------------
# Courbe quadratique
# ----------------------------------------------------------

geom_smooth(
  data = donnees_quadratiques,
  aes(
    x = nb_PE_CR_moy,
    y = delta_MADRS,
    linetype = "Quadratique"
  ),
  method = "lm",
  formula = y ~ poly(x, 2, raw = TRUE),
  se = FALSE,
  color = "black",
  linewidth = 0.8
) +
  
  # ----------------------------------------------------------
# Courbe cubique
# ----------------------------------------------------------

geom_smooth(
  data = donnees_cubiques,
  aes(
    x = nb_PE_CR_moy,
    y = delta_MADRS,
    linetype = "Cubique"
  ),
  method = "lm",
  formula = y ~ poly(x, 3, raw = TRUE),
  se = FALSE,
  color = "black",
  linewidth = 0.8
) +
  
  # Effectif par cluster
  geom_text(
    data = labels_n,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.2,
    size = 3.8
  ) +
  
  facet_wrap(
    ~ numero_cluster,
    scales = "free_x"
  ) +
  
  scale_color_manual(
    name = "Centroïde temporel des EP",
    values = c(
      "CR 1" = "#0057B8",
      "CR 2" = "#009E49",
      "CR 3" = "#F4D000",
      "CR 4" = "#F28C00",
      "CR 5" = "#D7191C",
      "CR 6 et plus" = "#7B2CBF"
    ),
    drop = FALSE
  ) +
  
  scale_size_continuous(
    name = "Dispersion temporelle des EP",
    range = c(2.5, 8),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  
  scale_linetype_manual(
    name = "Courbe",
    values = c(
      "Cubique" = "dotted",
      "Linéaire" = "solid",
      "Quadratique" = "dashed"
    ),
    breaks = c(
      "Cubique",
      "Linéaire",
      "Quadratique"
    )
  ) +
  
  labs(
    title = paste0(
      "Relation entre le nombre moyen d’EP par CR ",
      "et l’évolution de la MADRS"
    ),
    
    subtitle = paste0(
      "Courbes descriptives linéaire, quadratique et cubique — ",
      "ρs ajusté sur âge, sexe et années d’études"
    ),
    
    x = "Nombre moyen d’EP par changement de règle",
    
    y = "Delta MADRS (M6 - M0)"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  guides(
    color = guide_legend(
      order = 1,
      nrow = 2,
      byrow = TRUE
    ),
    
    size = guide_legend(
      order = 2
    ),
    
    linetype = guide_legend(
      order = 3
    )
  ) +
  
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.spacing.y = grid::unit(
      0.25,
      "cm"
    ),
    
    legend.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    ),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 20,
      l = 10
    ),
    
    panel.grid.minor = element_blank(),
    
    strip.text = element_text(
      face = "bold"
    )
  )

print(graph_centroide_temporel)
print(graph_centroide_temporel)

# ------------------------------------------------------------
# 12. Export du graphique
# ------------------------------------------------------------

ggsave(
  filename = "graph_centroide_temporel_EP_delta_MADRS_clusters.pdf",
  plot = graph_centroide_temporel,
  width = 12,
  height = 10
)



# ============================================================
# 14. GRAPHIQUE GLOBAL
# Tous les patients sur un même graphique
#
# Couleur = centroïde temporel des EP
# Taille  = dispersion temporelle des EP
# Forme   = cluster
# Courbes = ajustements globaux linéaire, quadratique et cubique
# ============================================================

# ------------------------------------------------------------
# 14.1 Données utilisées pour les courbes globales
# ------------------------------------------------------------

donnees_courbes_globales <- donnees_analyse %>%
  filter(
    !is.na(nb_PE_CR_moy),
    !is.na(delta_MADRS)
  )

# ------------------------------------------------------------
# 14.2 Formes attribuées aux clusters
# ------------------------------------------------------------

formes_clusters_disponibles <- c(
  16,  # cercle
  17,  # triangle
  15,  # carré
  18,  # losange
  8,   # étoile
  3,   # croix
  7,
  4
)

niveaux_clusters <- levels(
  droplevels(
    factor(donnees_analyse$numero_cluster)
  )
)

n_clusters_global <- length(niveaux_clusters)

if (n_clusters_global > length(formes_clusters_disponibles)) {
  stop(
    "Le nombre de clusters est supérieur au nombre de formes disponibles."
  )
}

formes_clusters <- formes_clusters_disponibles[
  seq_len(n_clusters_global)
]

names(formes_clusters) <- niveaux_clusters

# Vérification que le cluster est bien un facteur
donnees_analyse <- donnees_analyse %>%
  mutate(
    numero_cluster = factor(
      numero_cluster,
      levels = niveaux_clusters
    )
  )

donnees_avec_PE <- donnees_avec_PE %>%
  mutate(
    numero_cluster = factor(
      numero_cluster,
      levels = niveaux_clusters
    )
  )

donnees_sans_PE <- donnees_sans_PE %>%
  mutate(
    numero_cluster = factor(
      numero_cluster,
      levels = niveaux_clusters
    )
  )

# ------------------------------------------------------------
# 14.3 Corrélation de Spearman ajustée dans l'ensemble du groupe
# ------------------------------------------------------------

resultat_spearman_global <- faire_spearman_ajuste(donnees_analyse)

label_spearman_global <- if (
  is.na(resultat_spearman_global$rho_spearman_ajuste)
) {
  paste0(
    "n = ", resultat_spearman_global$n_spearman,
    "\nSpearman ajusté : non estimable"
  )
} else {
  paste0(
    "n = ", resultat_spearman_global$n_spearman,
    "\nρs ajusté = ",
    sprintf("%.2f", resultat_spearman_global$rho_spearman_ajuste),
    "\np = ",
    ifelse(
      resultat_spearman_global$p_value_spearman_ajuste < 0.001,
      "< 0,001",
      str_replace(
        sprintf("%.3f", resultat_spearman_global$p_value_spearman_ajuste),
        "\\.",
        ","
      )
    )
  )
}

print(resultat_spearman_global)

# ------------------------------------------------------------
# 14.4 Construction du graphique global
# ------------------------------------------------------------

graph_centroide_temporel_global <- ggplot() +
  
  geom_hline(
    yintercept = 0,
    color = "grey70",
    linewidth = 0.5
  ) +
  
  
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = label_spearman_global,
    hjust = -0.05,
    vjust = 1.15,
    size = 4.2
  ) +
  
  # Patients avec au moins une EP
  geom_point(
    data = donnees_avec_PE,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS,
      color = groupe_centroide_PE,
      size = dispersion_temporelle_PE,
      shape = numero_cluster
    ),
    alpha = 0.65,
    stroke = 0.8
  ) +
  
  # Patients sans EP
  geom_point(
    data = donnees_sans_PE,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS,
      shape = numero_cluster
    ),
    inherit.aes = FALSE,
    color = "grey55",
    size = 3.5,
    alpha = 0.9,
    stroke = 0.8
  ) +
  
  # Droite linéaire globale
  geom_smooth(
    data = donnees_courbes_globales,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS,
      linetype = "Linéaire"
    ),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "black",
    linewidth = 0.9
  ) +
  
  # Courbe quadratique globale
  geom_smooth(
    data = donnees_courbes_globales,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS,
      linetype = "Quadratique"
    ),
    method = "lm",
    formula = y ~ poly(x, 2, raw = TRUE),
    se = FALSE,
    color = "black",
    linewidth = 0.9
  ) +
  
  # Courbe cubique globale
  geom_smooth(
    data = donnees_courbes_globales,
    aes(
      x = nb_PE_CR_moy,
      y = delta_MADRS,
      linetype = "Cubique"
    ),
    method = "lm",
    formula = y ~ poly(x, 3, raw = TRUE),
    se = FALSE,
    color = "black",
    linewidth = 0.9
  ) +
  
  # Axe des x compressé pour mieux distinguer les faibles valeurs
  scale_x_continuous(
    trans = scales::pseudo_log_trans(
      sigma = 0.1
    ),
    breaks = c(
      0,
      0.25,
      0.5,
      1,
      2,
      4,
      8
    ),
    labels = c(
      "0",
      "0,25",
      "0,5",
      "1",
      "2",
      "4",
      "8"
    )
  ) +
  
  scale_color_manual(
    name = "Centroïde temporel des EP",
    values = c(
      "CR 1" = "#0057B8",
      "CR 2" = "#009E49",
      "CR 3" = "#F4D000",
      "CR 4" = "#F28C00",
      "CR 5" = "#D7191C",
      "CR 6 et plus" = "#7B2CBF"
    ),
    drop = FALSE
  ) +
  
  scale_size_continuous(
    name = "Dispersion temporelle des EP",
    range = c(
      2,
      6
    ),
    breaks = scales::pretty_breaks(
      n = 4
    )
  ) +
  
  scale_shape_manual(
    name = "Cluster",
    values = formes_clusters,
    drop = FALSE
  ) +
  
  scale_linetype_manual(
    name = "Courbe",
    values = c(
      "Cubique" = "dotted",
      "Linéaire" = "solid",
      "Quadratique" = "dashed"
    ),
    breaks = c(
      "Cubique",
      "Linéaire",
      "Quadratique"
    )
  ) +
  
  labs(
    title = paste0(
      "Relation globale entre le nombre moyen d’EP par CR ",
      "et l’évolution de la MADRS"
    ),
    
    subtitle = paste0(
      "Couleur : centroïde temporel ; ",
      "taille : dispersion temporelle ; ",
      "forme : cluster ; ρs ajusté sur âge, sexe et années d’études"
    ),
    
    x = "Nombre moyen d’EP par changement de règle",
    
    y = "Delta MADRS (M6 - M0)"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  guides(
    color = guide_legend(
      order = 1,
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        size = 4,
        shape = 16
      )
    ),
    
    size = guide_legend(
      order = 2
    ),
    
    shape = guide_legend(
      order = 3,
      nrow = 1,
      override.aes = list(
        size = 4,
        color = "black"
      )
    ),
    
    linetype = guide_legend(
      order = 4
    )
  ) +
  
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.spacing.y = grid::unit(
      0.20,
      "cm"
    ),
    
    legend.margin = margin(
      t = 4,
      r = 4,
      b = 4,
      l = 4
    ),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 15,
      l = 10
    ),
    
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 14.4 Affichage
# ------------------------------------------------------------

print(
  graph_centroide_temporel_global
)

# ------------------------------------------------------------
# 14.5 Export
# ------------------------------------------------------------

ggsave(
  filename = "graph_centroide_temporel_EP_delta_MADRS_global.pdf",
  plot = graph_centroide_temporel_global,
  width = 12,
  height = 10,
)

cat(
  "\nGraphiques globaux créés :\n",
  "- graph_centroide_temporel_EP_delta_MADRS_global.pdf\n",
  "- graph_centroide_temporel_EP_delta_MADRS_global.png\n"
)
# ------------------------------------------------------------
# 13. Export facultatif des paramètres temporels par patient
# ------------------------------------------------------------

write.csv(
  donnees_analyse,
  file = "parametres_temporels_PE_et_delta_MADRS.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)



# ============================================================
# 15. EXPORT DES CORRÉLATIONS DE SPEARMAN AJUSTÉES
# ============================================================

write.csv(
  resultats_spearman_clusters,
  "spearman_ajuste_EP_CR_delta_MADRS_par_cluster.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resultat_spearman_global,
  "spearman_ajuste_EP_CR_delta_MADRS_global.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

