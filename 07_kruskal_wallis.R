library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(rstatix)
library(writexl)
library(stringr)

# ============================================================
# ANALYSES COMPARATIVES ENTRE CLUSTERS M0
# Kruskal-Wallis + Dunn post hoc + taille d'effet
# Lecture directe du fichier clustering_MCST_tabl.xlsx
# ============================================================

# ------------------------------------------------------------
# 0. Parametres
# ------------------------------------------------------------

FICHIER_M0 <- "clustering_MCST_tabl.xlsx"



SHEET_PATIENTS_M0 <- "patients"

# ------------------------------------------------------------
# 1. Preparation de la table
# ------------------------------------------------------------

donnees_tests_M0 <- read_excel(
  FICHIER_M0,
  sheet = SHEET_PATIENTS_M0
) %>%
  mutate(
    numero_patient = as.character(numero_patient),
    numero_patient = if_else(
      !is.na(numero_patient) & str_detect(numero_patient, "^\\d+$"),
      str_pad(numero_patient, width = 3, side = "left", pad = "0"),
      numero_patient
    ),
    numero_cluster = factor(numero_cluster)
  )

variables_tests_M0 <- c(
  # Scores dynamiques / CR déjà présents
  "nb_PE_CR_moy",
  "CR_avec_PE_moy",
  "nb_essais",
  "nb_categories_completees",
  "nb_ERR_CR_moy",
  "nb_CORR_CR_moy",
  "nb_CR",
  
  # Nouveaux indicateurs dynamiques
  "probabilite_changement_apres_feedback_negatif",
  "probabilite_maintien_apres_feedback_positif",
  "nb_essais_nouvelle_categorie_moy",
  "longueur_sequences_PE_moy",
  "longueur_sequences_PE_max",
  "nb_ruptures_set_apres_3_COR",
  "probabilite_rupture_set_apres_3_COR",
  "proportion_PE_parmi_erreurs",
  
  # Scores classiques
  "temps",
  "categories",
  "cartes_6_cat",
  "erreurs_totales",
  "perseverations",
  "pct_perseverations",
  
  # Variables sociodémographiques et cliniques
  "age_M0",
  "Etudes",
  "madrstot_M0",
  "staiatot_M0"
)

noms_variables_M0 <- tibble(
  variable = variables_tests_M0
)


variables_absentes_M0 <- setdiff(variables_tests_M0, names(donnees_tests_M0))

if (length(variables_absentes_M0) > 0) {
  stop(
    "Variables absentes du fichier M0 : ",
    paste(variables_absentes_M0, collapse = ", ")
  )
}

donnees_tests_M0 <- donnees_tests_M0 %>%
  mutate(
    across(
      all_of(variables_tests_M0),
      as.numeric
    )
  )

# ------------------------------------------------------------
# 2. Kruskal-Wallis global
# ------------------------------------------------------------

resultats_kruskal_M0 <- map_dfr(
  variables_tests_M0,
  function(var) {
    
    donnees_var <- donnees_tests_M0 %>%
      select(numero_cluster, all_of(var)) %>%
      rename(valeur = all_of(var)) %>%
      filter(
        !is.na(numero_cluster),
        !is.na(valeur)
      )
    
    test <- tryCatch(
      kruskal.test(
        formula = valeur ~ numero_cluster,
        data = donnees_var
      ),
      error = function(e) NULL
    )
    
    if (is.null(test)) {
      return(
        tibble(
          variable = var,
          statistique_KW = NA_real_,
          ddl = NA_real_,
          p_value = NA_real_
        )
      )
    }
    
    tibble(
      variable = var,
      statistique_KW = unname(test$statistic),
      ddl = unname(test$parameter),
      p_value = test$p.value
    )
  }
) %>%
  mutate(
    p_value = round(p_value, 5),
    significatif = if_else(!is.na(p_value) & p_value < 0.05, "oui", "non")
  ) %>%
  left_join(noms_variables_M0, by = "variable") %>%
  select(
    variable,
    statistique_KW,
    ddl,
    p_value,
    significatif
  )

# ------------------------------------------------------------
# 3. Taille d'effet Kruskal-Wallis
# epsilon carre / eta2_H
# Formule : (H - k + 1) / (n - k)
# ------------------------------------------------------------

resultats_effet_M0 <- map_dfr(
  variables_tests_M0,
  function(var) {
    
    donnees_var <- donnees_tests_M0 %>%
      select(numero_cluster, all_of(var)) %>%
      rename(valeur = all_of(var)) %>%
      filter(
        !is.na(numero_cluster),
        !is.na(valeur)
      )
    
    test <- tryCatch(
      kruskal.test(
        formula = valeur ~ numero_cluster,
        data = donnees_var
      ),
      error = function(e) NULL
    )
    
    if (is.null(test)) {
      return(
        tibble(
          variable = var,
          H = NA_real_,
          n = nrow(donnees_var),
          k = n_distinct(donnees_var$numero_cluster),
          epsilon2 = NA_real_,
          interpretation = "non estimable"
        )
      )
    }
    
    H <- unname(test$statistic)
    k <- n_distinct(donnees_var$numero_cluster)
    n <- nrow(donnees_var)
    
    epsilon2 <- (H - k + 1) / (n - k)
    
    tibble(
      variable = var,
      H = H,
      n = n,
      k = k,
      epsilon2 = epsilon2,
      interpretation = case_when(
        is.na(epsilon2) ~ "non estimable",
        epsilon2 < 0.01 ~ "negligeable",
        epsilon2 < 0.06 ~ "faible",
        epsilon2 < 0.14 ~ "moderee",
        TRUE ~ "forte"
      )
    )
  }
) %>%
  mutate(
    epsilon2 = round(epsilon2, 3)
  ) %>%
  left_join(noms_variables_M0, by = "variable") %>%
  select(
    variable,
    H,
    n,
    k,
    epsilon2,
    interpretation
  )



# ------------------------------------------------------------
# 4. Post hoc de Dunn
# Comparaison de chaque paire de clusters
# Correction BH = Benjamini-Hochberg
# ------------------------------------------------------------

faire_dunn_M0 <- function(var) {
  
  donnees_var <- donnees_tests_M0 %>%
    select(numero_cluster, all_of(var)) %>%
    rename(valeur = all_of(var)) %>%
    filter(
      !is.na(numero_cluster),
      !is.na(valeur)
    )
  
  if (nrow(donnees_var) < 3 || n_distinct(donnees_var$numero_cluster) < 2) {
    return(
      tibble(
        variable = var,
        group1 = NA_character_,
        group2 = NA_character_,
        n1 = NA_integer_,
        n2 = NA_integer_,
        statistic = NA_real_,
        p = NA_real_,
        p.adj = NA_real_,
        p.adj.signif = NA_character_
      )
    )
  }
  
  tailles_clusters <- donnees_var %>%
    count(numero_cluster, name = "n_cluster") %>%
    mutate(numero_cluster = as.character(numero_cluster))
  
  res <- tryCatch(
    {
      donnees_var %>%
        dunn_test(
          formula = valeur ~ numero_cluster,
          p.adjust.method = "BH"
        )
    },
    error = function(e) {
      tibble(
        group1 = NA_character_,
        group2 = NA_character_,
        statistic = NA_real_,
        p = NA_real_,
        p.adj = NA_real_,
        p.adj.signif = NA_character_
      )
    }
  )
  
  res %>%
    select(-any_of(c("n1", "n2"))) %>%
    mutate(
      variable = var,
      group1 = as.character(group1),
      group2 = as.character(group2)
    ) %>%
    left_join(
      tailles_clusters %>%
        rename(group1 = numero_cluster, n1 = n_cluster),
      by = "group1"
    ) %>%
    left_join(
      tailles_clusters %>%
        rename(group2 = numero_cluster, n2 = n_cluster),
      by = "group2"
    ) %>%
    select(
      variable,
      group1,
      group2,
      n1,
      n2,
      statistic,
      p,
      p.adj,
      p.adj.signif
    )
}

resultats_dunn_M0 <- map_dfr(
  variables_tests_M0,
  faire_dunn_M0
) %>%
  left_join(
    noms_variables_M0,
    by = "variable"
  ) %>%
  select(
    variable,
    group1,
    group2,
    n1,
    n2,
    statistic,
    p,
    p.adj,
    p.adj.signif
  )

print(resultats_dunn_M0)
View(resultats_dunn_M0)

# ------------------------------------------------------------
# 5. Export Excel
# ------------------------------------------------------------

write_xlsx(
  list(
    kruskal_wallis = resultats_kruskal_M0,
    taille_effet = resultats_effet_M0,
    posthoc_dunn = resultats_dunn_M0
  ),
  "comparaisons_clusters_M0_variables_selectionnees_KW_Dunn_effets.xlsx"
)

