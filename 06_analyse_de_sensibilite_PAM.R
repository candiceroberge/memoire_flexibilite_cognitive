# ============================================================
# ANALYSE DE SENSIBILITE : PAM vs WARD
# ============================================================

# ------------------------------------------------------------
# 1. Fonction ARI sans package supplementaire
# ------------------------------------------------------------

adjusted_rand_index <- function(x, y) {
  
  tab <- table(x, y)
  
  comb2 <- function(z) choose(z, 2)
  
  sum_comb_cells <- sum(comb2(tab))
  sum_comb_rows <- sum(comb2(rowSums(tab)))
  sum_comb_cols <- sum(comb2(colSums(tab)))
  n <- sum(tab)
  total_comb <- comb2(n)
  
  expected_index <- (sum_comb_rows * sum_comb_cols) / total_comb
  max_index <- 0.5 * (sum_comb_rows + sum_comb_cols)
  
  if (max_index == expected_index) {
    return(NA_real_)
  }
  
  ari <- (sum_comb_cells - expected_index) / (max_index - expected_index)
  
  return(as.numeric(ari))
}

# ------------------------------------------------------------
# 2. PAM sur la meme matrice de distance que Ward
# ------------------------------------------------------------

set.seed(123)

pam_CR <- pam(
  x = dist_CR,
  k = K_CHOISI,
  diss = TRUE
)

clusters_pam_bruts <- pam_CR$clustering

# ------------------------------------------------------------
# 3. Reordonner les clusters PAM du profil le moins perseveratif
#    au profil le plus perseveratif
# ------------------------------------------------------------

ordre_clusters_pam <- table_patients_analyse %>%
  mutate(
    cluster_pam_brut = as.integer(clusters_pam_bruts)
  ) %>%
  group_by(cluster_pam_brut) %>%
  summarise(
    PE_moyen = mean(nb_PE_CR_total, na.rm = TRUE),
    CR_avec_PE_moyen = mean(nb_CR_avec_PE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(PE_moyen, CR_avec_PE_moyen) %>%
  mutate(
    cluster_pam = row_number()
  )

# ------------------------------------------------------------
# 4. Table de correspondance PAM patient par patient
# Important : jointure par patient pour eviter les decalages d'ordre
# ------------------------------------------------------------

attribution_pam <- table_patients_analyse %>%
  select(patient) %>%
  mutate(
    cluster_pam_brut = as.integer(clusters_pam_bruts)
  ) %>%
  left_join(
    ordre_clusters_pam %>%
      select(cluster_pam_brut, cluster_pam),
    by = "cluster_pam_brut"
  ) %>%
  select(
    patient,
    cluster_pam
  )

table_patients_sensibilite <- table_patients %>%
  rename(
    cluster_ward = cluster
  ) %>%
  left_join(
    attribution_pam,
    by = "patient"
  ) %>%
  select(
    patient,
    cluster_ward,
    cluster_pam,
    abandon,
    everything()
  ) %>%
  arrange(cluster_ward, cluster_pam, patient)

View(table_patients_sensibilite)

# ------------------------------------------------------------
# 5. ARI Ward vs PAM
# ------------------------------------------------------------

ARI_ward_pam <- adjusted_rand_index(
  table_patients_sensibilite$cluster_ward,
  table_patients_sensibilite$cluster_pam
)

table_ARI_ward_pam <- tibble(
  comparaison = "Ward.D2 vs PAM",
  k = K_CHOISI,
  ARI = round(ARI_ward_pam, 3),
  interpretation = case_when(
    is.na(ARI) ~ "non interpretable",
    ARI < 0.20 ~ "accord faible",
    ARI < 0.40 ~ "accord modere faible",
    ARI < 0.60 ~ "accord modere",
    ARI < 0.80 ~ "accord fort",
    TRUE ~ "accord tres fort"
  )
)

print(table_ARI_ward_pam)
View(table_ARI_ward_pam)

# ------------------------------------------------------------
# 6. Table croisee Ward x PAM
# ------------------------------------------------------------

table_croisee_ward_pam <- table_patients_sensibilite %>%
  count(cluster_ward, cluster_pam, name = "n") %>%
  pivot_wider(
    names_from = cluster_pam,
    values_from = n,
    values_fill = 0,
    names_prefix = "PAM_"
  ) %>%
  arrange(cluster_ward)

print(table_croisee_ward_pam)
View(table_croisee_ward_pam)

# ------------------------------------------------------------
# 7. Variables a decrire dans les clusters PAM
# Meme nomenclature que le tableau principal des clusters
# ------------------------------------------------------------

variables_clusters_lisibles_pam <- tibble(
  variable_code = c(
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
    "nb_CORR_CR_total"
  ),
  Variable = c(
    "Nombre de catégories complétées",
    "Nombre de cartes pour compléter 6 catégories",
    "Nombre total d’erreurs",
    "Nombre total de PE",
    "Temps de passation",
    "Nombre total d’essais",
    "Nombre de changements de règle",
    "Nombre d’erreurs simples après changement de règle",
    "Nombre d’erreurs de persévération après changement de règle",
    "Nombre de changements de règle avec erreur de persévération",
    "Nombre d’essais corrects après changement de règle"
  )
)

variables_manquantes_pam <- setdiff(
  variables_clusters_lisibles_pam$variable_code,
  names(table_patients_sensibilite)
)

if (length(variables_manquantes_pam) > 0) {
  stop(
    paste0(
      "Variables manquantes dans table_patients_sensibilite : ",
      paste(variables_manquantes_pam, collapse = ", "),
      ". Verifie que le bloc d'ajout des scores classiques est bien place apres la creation de table_patients."
    )
  )
}

# ------------------------------------------------------------
# 8. Ligne effectif des clusters PAM
# ------------------------------------------------------------

effectifs_clusters_pam <- table_patients_sensibilite %>%
  group_by(cluster_pam) %>%
  summarise(
    Valeur = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cluster_pam = paste0("PAM ", cluster_pam),
    Variable = "Effectif du cluster",
    Indicateur = "N patients"
  ) %>%
  select(
    Variable,
    Indicateur,
    cluster_pam,
    Valeur
  )

# ------------------------------------------------------------
# 9. Description des variables par cluster PAM
# ------------------------------------------------------------

description_clusters_pam_long <- table_patients_sensibilite %>%
  select(
    cluster_pam,
    patient,
    all_of(variables_clusters_lisibles_pam$variable_code)
  ) %>%
  pivot_longer(
    cols = all_of(variables_clusters_lisibles_pam$variable_code),
    names_to = "variable_code",
    values_to = "valeur"
  ) %>%
  left_join(
    variables_clusters_lisibles_pam,
    by = "variable_code"
  ) %>%
  group_by(cluster_pam, Variable) %>%
  summarise(
    Moyenne = round(mean(valeur, na.rm = TRUE), 2),
    Médiane = round(median(valeur, na.rm = TRUE), 2),
    `Écart-type` = round(sd0(valeur), 2),
    .groups = "drop"
  ) %>%
  mutate(
    cluster_pam = paste0("PAM ", cluster_pam),
    Variable = factor(
      Variable,
      levels = variables_clusters_lisibles_pam$Variable
    )
  ) %>%
  pivot_longer(
    cols = c(
      Moyenne,
      Médiane,
      `Écart-type`
    ),
    names_to = "Indicateur",
    values_to = "Valeur"
  ) %>%
  mutate(
    Indicateur = factor(
      Indicateur,
      levels = c("Moyenne", "Médiane", "Écart-type")
    )
  ) %>%
  arrange(Variable, Indicateur) %>%
  mutate(
    Variable = as.character(Variable),
    Indicateur = as.character(Indicateur)
  )

# ------------------------------------------------------------
# 10. Tableau final descriptif PAM
# ------------------------------------------------------------

table_clusters_pam <- bind_rows(
  effectifs_clusters_pam,
  description_clusters_pam_long
) %>%
  pivot_wider(
    names_from = cluster_pam,
    values_from = Valeur
  )

print(table_clusters_pam)
View(table_clusters_pam)

# ------------------------------------------------------------
# 11. Export Excel dedie a l'analyse de sensibilite
# ------------------------------------------------------------

write_xlsx(
  list(
    patients_ward_pam = table_patients_sensibilite,
    clusters_pam = table_clusters_pam,
    ari_ward_pam = table_ARI_ward_pam,
    table_croisee_ward_pam = table_croisee_ward_pam
  ),
  "sensibilite_PAM_Ward_ARI_table_croisee.xlsx"
)

