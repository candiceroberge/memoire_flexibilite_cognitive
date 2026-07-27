# ============================================================
# DELTA MADRS SELON LES CLUSTERS M0
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(rstatix)
library(ggplot2)
library(writexl)
library(tibble)
library(emmeans)
library(flextable)
library(officer)

# ------------------------------------------------------------
# 0. Fichiers
# ------------------------------------------------------------

FICHIER_CLUSTER <- "clustering_MCST_tabl.xlsx"
FICHIER_MADRS   <- "scores_MADRS.xlsx"

FEUILLE_CLUSTER <- "patients"
FEUILLE_MADRS   <- 1

# ------------------------------------------------------------
# 1. Fonctions utiles
# ------------------------------------------------------------

normaliser_nom <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "")
}

trouver_colonne <- function(df, candidats, nom_variable) {
  noms_norm <- normaliser_nom(names(df))
  candidats_norm <- normaliser_nom(candidats)
  
  idx <- match(candidats_norm, noms_norm)
  idx <- idx[!is.na(idx)]
  
  if (length(idx) == 0) {
    stop(
      paste0(
        "Colonne introuvable pour ", nom_variable, ".\n",
        "Colonnes disponibles : ",
        paste(names(df), collapse = ", ")
      )
    )
  }
  
  names(df)[idx[1]]
}

as_numeric_clean <- function(x) {
  as.numeric(str_replace(as.character(x), ",", "."))
}

faire_shapiro <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) < 3) {
    return(tibble(
      n = length(x),
      W = NA_real_,
      p_value = NA_real_,
      interpretation = "test non réalisable, n < 3"
    ))
  }
  
  if (length(unique(x)) < 3) {
    return(tibble(
      n = length(x),
      W = NA_real_,
      p_value = NA_real_,
      interpretation = "test non réalisable, valeurs trop peu variables"
    ))
  }
  
  test <- shapiro.test(x)
  
  tibble(
    n = length(x),
    W = as.numeric(test$statistic),
    p_value = test$p.value,
    interpretation = ifelse(
      test$p.value > 0.05,
      "compatible avec la normalité",
      "non-normalité possible"
    )
  )
}

# ------------------------------------------------------------
# 2. Import fichier clustering
# ------------------------------------------------------------

clusters_brut <- read_excel(FICHIER_CLUSTER, sheet = FEUILLE_CLUSTER)

col_patient_cluster <- trouver_colonne(
  clusters_brut,
  c("numero_patient", "patient", "ID"),
  "numero_patient dans le fichier clustering"
)

col_cluster <- trouver_colonne(
  clusters_brut,
  c("numero_cluster", "numero_cluster_m0", "cluster", "cluster_M0"),
  "numero_cluster"
)

clusters <- clusters_brut %>%
  transmute(
    numero_patient = str_extract(as.character(.data[[col_patient_cluster]]), "\\d+"),
    numero_patient = str_pad(numero_patient, width = 3, side = "left", pad = "0"),
    numero_cluster = as.factor(.data[[col_cluster]])
  )

# ------------------------------------------------------------
# 3. Import fichier MADRS + covariables
# ------------------------------------------------------------

madrs_brut <- read_excel(FICHIER_MADRS, sheet = FEUILLE_MADRS)

col_id <- trouver_colonne(
  madrs_brut,
  c("ID", "numero_patient", "patient"),
  "ID patient"
)

col_madrs_m0 <- trouver_colonne(
  madrs_brut,
  c("madrstot_M0", "MADRS TOT M0", "MADRS_TOT_M0", "MADRS M0"),
  "MADRS M0"
)

col_madrs_m6 <- trouver_colonne(
  madrs_brut,
  c("madrstot_M6", "MADRS TOT M6", "MADRS_TOT_M6", "MADRS M6"),
  "MADRS M6"
)

col_delta <- trouver_colonne(
  madrs_brut,
  c("delta_MADRS", "Delta MADRS", "delta MADRS"),
  "delta MADRS"
)

col_age <- trouver_colonne(
  madrs_brut,
  c("age_M0", "age", "Age"),
  "âge"
)

col_sexe <- trouver_colonne(
  madrs_brut,
  c("sexe_M0", "sexe", "Sexe"),
  "sexe"
)

col_etudes <- trouver_colonne(
  madrs_brut,
  c("Etudes", "études", "etudes", "niveau_etudes", "niveau études"),
  "études"
)

madrs <- madrs_brut %>%
  mutate(
    numero_patient = str_extract(as.character(.data[[col_id]]), "\\d+"),
    numero_patient = str_pad(numero_patient, width = 3, side = "left", pad = "0"),
    
    MADRS_TOT_M0 = as_numeric_clean(.data[[col_madrs_m0]]),
    MADRS_TOT_M6 = as_numeric_clean(.data[[col_madrs_m6]]),
    delta_MADRS  = as_numeric_clean(.data[[col_delta]]),
    
    age = as_numeric_clean(.data[[col_age]]),
    sexe = as.factor(.data[[col_sexe]]),
    etudes = as_numeric_clean(.data[[col_etudes]])
  ) %>%
  select(
    numero_patient,
    MADRS_TOT_M0,
    MADRS_TOT_M6,
    delta_MADRS,
    age,
    sexe,
    etudes
  )

# ------------------------------------------------------------
# 4. Jointure
# ------------------------------------------------------------

donnees_analyse <- clusters %>%
  left_join(madrs, by = "numero_patient") %>%
  filter(!is.na(delta_MADRS))

cat("\nNombre de patients avec delta MADRS :", nrow(donnees_analyse), "\n")

cat("\nEffectifs par cluster avant exclusion des covariables manquantes :\n")
print(table(donnees_analyse$numero_cluster))

# ------------------------------------------------------------
# 5. Préparation des données pour l’ANCOVA
# ------------------------------------------------------------

donnees_ajustees <- donnees_analyse %>%
  mutate(
    numero_cluster = as.factor(numero_cluster),
    age = as.numeric(age),
    sexe = as.factor(sexe),
    etudes = as.numeric(etudes),
    delta_MADRS = as.numeric(delta_MADRS)
  ) %>%
  filter(
    !is.na(delta_MADRS),
    !is.na(numero_cluster),
    !is.na(age),
    !is.na(sexe),
    !is.na(etudes)
  )

cat("\nNombre de patients inclus dans l'ANCOVA :", nrow(donnees_ajustees), "\n")
cat("\nEffectifs par cluster dans l'ANCOVA :\n")
print(table(donnees_ajustees$numero_cluster))

# ------------------------------------------------------------
# 6. Modèle ajusté
# ------------------------------------------------------------

modele_ajuste <- lm(
  delta_MADRS ~ numero_cluster + age + sexe + etudes,
  data = donnees_ajustees
)

modele_sans_cluster <- lm(
  delta_MADRS ~ age + sexe + etudes,
  data = donnees_ajustees
)

# ------------------------------------------------------------
# 7. Normalité des résidus
# ------------------------------------------------------------

tableau_normalite_ajuste <- faire_shapiro(residuals(modele_ajuste)) %>%
  mutate(
    type = "résidus du modèle ajusté",
    groupe = "global"
  ) %>%
  select(type, groupe, n, W, p_value, interpretation) %>%
  mutate(
    W = round(W, 3),
    p_value = round(p_value, 4)
  )

# ------------------------------------------------------------
# 8. Levene sur les résidus ajustés
# ------------------------------------------------------------

donnees_ajustees_residus <- donnees_ajustees %>%
  mutate(residus_ajustes = residuals(modele_ajuste))

levene_ajuste <- donnees_ajustees_residus %>%
  levene_test(residus_ajustes ~ numero_cluster)

tableau_levene_ajuste <- levene_ajuste %>%
  transmute(
    test = "Levene sur résidus ajustés",
    statistique = round(as.numeric(statistic), 3),
    ddl1 = df1,
    ddl2 = df2,
    p_value = round(as.numeric(p), 4),
    interpretation = ifelse(
      p > 0.05,
      "homogénéité des variances compatible",
      "hétérogénéité des variances possible"
    )
  )

# ------------------------------------------------------------
# 9. Test global de l'effet du cluster ajusté
# ------------------------------------------------------------

comparaison_modeles <- anova(modele_sans_cluster, modele_ajuste) %>%
  as.data.frame() %>%
  rownames_to_column("modele")

p_cluster_ajuste <- comparaison_modeles$`Pr(>F)`[2]


tableau_test_global_ajuste <- tibble(
  test = "ANCOVA / comparaison de modèles",
  effet = "numero_cluster ajusté",
  covariables = "âge + sexe + études",
  statistique = round(as.numeric(comparaison_modeles$F[2]), 3),
  ddl1 = as.numeric(comparaison_modeles$Df[2]),
  ddl2 = as.numeric(comparaison_modeles$Res.Df[2]),
  p_value = round(as.numeric(p_cluster_ajuste), 4),
  interpretation = ifelse(
    p_cluster_ajuste < 0.05,
    "effet ajusté significatif du cluster",
    "pas d'effet ajusté significatif du cluster"
  )
)

# ------------------------------------------------------------
# 10. Taille d'effet ajustée : eta² partiel
# ------------------------------------------------------------

SS_cluster_ajuste <- comparaison_modeles$`Sum of Sq`[2]
SS_residus_ajuste <- comparaison_modeles$RSS[2]

eta2_partiel_ajuste <- SS_cluster_ajuste /
  (SS_cluster_ajuste + SS_residus_ajuste)

tableau_taille_effet_ajuste <- tibble(
  test = "ANCOVA",
  indice = "eta2 partiel",
  valeur = round(as.numeric(eta2_partiel_ajuste), 3),
  interpretation = case_when(
    eta2_partiel_ajuste < 0.01 ~ "effet très faible",
    eta2_partiel_ajuste < 0.06 ~ "effet faible",
    eta2_partiel_ajuste < 0.14 ~ "effet moyen",
    TRUE ~ "effet important"
  )
)

# ------------------------------------------------------------
# 11. Post-hoc ajusté
# Comparaisons deux à deux avec :
# - p non corrigée
# - p corrigée pour comparaisons multiples par la méthode de Tukey
# ------------------------------------------------------------

if (!is.na(p_cluster_ajuste) && p_cluster_ajuste < 0.05) {
  
  # Comparaisons sans correction
  posthoc_non_corrige <- pairs(
    emmeans(modele_ajuste, ~ numero_cluster),
    adjust = "none"
  ) %>%
    as.data.frame() %>%
    transmute(
      comparaison = contrast,
      estimation = estimate,
      erreur_standard = SE,
      ddl = df,
      statistique = t.ratio,
      p_value_non_corrigee = p.value
    )
  
  # Mêmes comparaisons avec correction de Tukey
  posthoc_corrige_tukey <- pairs(
    emmeans(modele_ajuste, ~ numero_cluster),
    adjust = "tukey"
  ) %>%
    as.data.frame() %>%
    transmute(
      comparaison = contrast,
      p_value_corrigee_tukey = p.value
    )
  
  # Fusion des deux valeurs de p dans le même tableau
  tableau_posthoc_ajuste <- posthoc_non_corrige %>%
    left_join(
      posthoc_corrige_tukey,
      by = "comparaison"
    ) %>%
    mutate(
      estimation = round(estimation, 3),
      erreur_standard = round(erreur_standard, 3),
      ddl = round(ddl, 1),
      statistique = round(statistique, 3),
      p_value_non_corrigee = round(p_value_non_corrigee, 4),
      p_value_corrigee_tukey = round(p_value_corrigee_tukey, 4)
    )
  
} else {
  
  tableau_posthoc_ajuste <- tibble(
    message = "Post-hoc de Tukey non réalisé car l'effet global ajusté du cluster n'est pas significatif."
  )
}

# ------------------------------------------------------------
# 12. Export Excel
# ------------------------------------------------------------

write_xlsx(
  list(
    normalite = tableau_normalite_ajuste,
    levene = tableau_levene_ajuste,
    test_global = tableau_test_global_ajuste,
    taille_effet = tableau_taille_effet_ajuste,
    posthoc = tableau_posthoc_ajuste
  ),
  path = "analyse_delta_MADRS_clusters_M0_ANCOVA_ajustee.xlsx"
)

# ------------------------------------------------------------
# 13. Graphique ajusté
# ------------------------------------------------------------

moyennes_ajustees_clusters <- emmeans(
  modele_ajuste,
  ~ numero_cluster
) %>%
  as.data.frame() %>%
  rename(
    delta_MADRS_ajuste = emmean
  )

graph_ajuste <- ggplot() +
  geom_point(
    data = donnees_ajustees,
    aes(x = numero_cluster, y = delta_MADRS, color = "Patient"),
    position = position_jitter(width = 0.12, height = 0),
    size = 4,
    alpha = 0.65,
    shape = 16
  ) +
  geom_point(
    data = moyennes_ajustees_clusters,
    aes(x = numero_cluster, y = delta_MADRS_ajuste, color = "Moyenne ajustée du cluster"),
    shape = 18,
    size = 6
  ) +
  geom_hline(yintercept = 0, color = "grey70") +
  scale_color_manual(
    values = c(
      "Patient" = "#8DB9EF",
      "Moyenne ajustée du cluster" = "black"
    )
  ) +
  labs(
    x = "Cluster M0",
    y = "Delta MADRS (M6-M0)",
    color = NULL,
    title = "Delta MADRS selon les clusters M0",
    subtitle = "Analyse ajustée sur âge, sexe et études"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "delta_MADRS_clusters_M0_ANCOVA_ajustee.pdf",
  plot = graph_ajuste,
  width = 8,
  height = 5.5
)

ggsave(
  filename = "delta_MADRS_clusters_M0_ANCOVA_ajustee.png",
  plot = graph_ajuste,
  width = 8,
  height = 5.5,
  dpi = 300
)







# ============================================================
# 14. EXPORT WORD DES TABLEAUX PRINCIPAUX
#
# - Tableau 1 : test global de l'ANCOVA
# - Tableau 2 : post-hoc de Tukey
#
# La taille d'effet n'est pas ajoutée au document Word.
# ============================================================

FICHIER_WORD_RESULTATS <-
  "tableaux_ANCOVA_posthoc_delta_MADRS_clusters_M0.docx"

# ------------------------------------------------------------
# 14.1 Préparation du tableau du test global
# ------------------------------------------------------------

tableau_test_global_word <- tableau_test_global_ajuste %>%
  rename(
    Test = test,
    Effet = effet,
    Covariables = covariables,
    `Statistique F` = statistique,
    `DDL 1` = ddl1,
    `DDL 2` = ddl2,
    `Valeur de p` = p_value,
    Interprétation = interpretation
  ) %>%
  flextable() %>%
  theme_booktabs() %>%
  autofit() %>%
  align(
    align = "center",
    part = "all"
  ) %>%
  align(
    j = c(
      "Test",
      "Effet",
      "Covariables",
      "Interprétation"
    ),
    align = "left",
    part = "all"
  ) %>%
  bold(
    part = "header"
  ) %>%
  fontsize(
    size = 9,
    part = "all"
  ) %>%
  set_caption(
    caption = paste0(
      "Tableau 1. Effet global ajusté du cluster M0 ",
      "sur l’évolution du score MADRS"
    )
  )

# ------------------------------------------------------------
# 14.2 Préparation du tableau post-hoc
# ------------------------------------------------------------

# Le tableau peut avoir deux formes :
# - résultats des comparaisons de Tukey ;
# - message indiquant que le post-hoc n'a pas été réalisé.

if ("comparaison" %in% names(tableau_posthoc_ajuste)) {
  
  tableau_posthoc_word <- tableau_posthoc_ajuste %>%
    rename(
      Comparaison = comparaison,
      `Différence estimée`= estimation,
      `Erreur standard` = erreur_standard,
      DDL = ddl,
      `Statistique t` = statistique,
      `Valeur de p non corrigée` = p_value_non_corrigee,
      `Valeur de p corrigée (Tukey)` = p_value_corrigee_tukey
    ) %>%
    mutate(
      Significativité = case_when(
        is.na(`Valeur de p corrigée (Tukey)`) ~ NA_character_,
        `Valeur de p corrigée (Tukey)` < 0.001 ~ "***",
        `Valeur de p corrigée (Tukey)` < 0.01 ~ "**",
        `Valeur de p corrigée (Tukey)` < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    
    flextable() %>%
    theme_booktabs() %>%
    autofit() %>%
    align(
      align = "center",
      part = "all"
    ) %>%
    align(
      j = "Comparaison",
      align = "left",
      part = "all"
    ) %>%
    bold(
      part = "header"
    ) %>%
    fontsize(
      size = 9,
      part = "all"
    ) %>%
    set_caption(
      caption = paste0(
        "Tableau 2. Comparaisons post-hoc de Tukey ",
        "entre les clusters M0"
      )
    )
  
} else {
  
  tableau_posthoc_word <- tableau_posthoc_ajuste %>%
    rename(
      Résultat = message
    ) %>%
    flextable() %>%
    theme_booktabs() %>%
    autofit() %>%
    align(
      align = "left",
      part = "all"
    ) %>%
    bold(
      part = "header"
    ) %>%
    fontsize(
      size = 9,
      part = "all"
    ) %>%
    set_caption(
      caption = paste0("Tableau 2. Comparaisons post-hoc des moyennes ajustées du delta MADRS entre les clusters M0"
      )
    )
}

# ------------------------------------------------------------
# 14.3 Création du document Word
# ------------------------------------------------------------

document_resultats <- read_docx() %>%
  
  body_add_par(
    "Analyse de l’évolution de la MADRS selon les clusters M0",
    style = "heading 1"
  ) %>%
  
  body_add_par(
    paste0(
      "Analyse ajustée sur l’âge, le sexe et le niveau d’études."
    ),
    style = "Normal"
  ) %>%
  
  body_add_par(
    "Test global",
    style = "heading 2"
  ) %>%
  
  body_add_flextable(
    tableau_test_global_word
  ) %>%
  
  body_add_par(
    "",
    style = "Normal"
  ) %>%
  
  body_add_par(
    "Analyse post-hoc",
    style = "heading 2"
  ) %>%
  
  body_add_flextable(
    tableau_posthoc_word
  ) %>%
  
  body_add_par(
    "",
    style = "Normal"
  ) %>%
  
  body_add_par(
    paste0(
      "Note : les valeurs de p non corrigées sont présentées à titre descriptif. ",
      "Les valeurs de p corrigées tiennent compte des comparaisons multiples selon la méthode de Tukey. ",
      "La significativité est interprétée à partir des valeurs de p corrigées. ",
      "*** p < 0,001 ; ** p < 0,01 ; * p < 0,05 ; ns : non significatif."
    ),
    style = "Normal"
  )

# ------------------------------------------------------------
# 14.4 Enregistrement du document Word
# ------------------------------------------------------------

print(
  document_resultats,
  target = FICHIER_WORD_RESULTATS
)

cat(
  "\nDocument Word créé : ",
  FICHIER_WORD_RESULTATS,
  "\n",
  sep = ""
)

