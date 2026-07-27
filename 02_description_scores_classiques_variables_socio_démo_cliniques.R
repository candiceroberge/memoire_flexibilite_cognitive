library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(ggplot2)
library(tidyr)
library(stringr)
library(flextable)
library(officer)

# ============================================================
# 1. EXTRACTION DES SCORES CLASSIQUES PATIENT PAR PATIENT
# ============================================================

fichiers <- list.files(
  pattern = "\\.xlsx$",
  full.names = TRUE
)

fichiers <- fichiers[
  !str_detect(
    basename(fichiers),
    "description_scores_classiques|scores_MADRS|~\\$"
  )
]

extraire_numero_patient <- function(fichier) {
  
  nom <- tools::file_path_sans_ext(basename(fichier))
  numero <- str_extract(nom, "\\d+")
  
  str_pad(numero, width = 3, side = "left", pad = "0")
}

extraire_scores <- function(fichier) {
  
  tab <- read_excel(
    fichier,
    col_names = FALSE
  )
  
  numero_patient <- extraire_numero_patient(fichier)
  
  lignes_scores_possibles <- which(
    rowSums(!is.na(tab[, 7:12])) > 0
  )
  
  if (length(lignes_scores_possibles) == 0) {
    
    return(
      tibble(
        patient = numero_patient,
        temps = NA_real_,
        categories = NA_real_,
        cartes_6_cat = NA_real_,
        erreurs_totales = NA_real_,
        perseverations = NA_real_,
        pct_perseverations = NA_real_
      )
    )
  }
  
  ligne_scores <- max(lignes_scores_possibles)
  
  tibble(
    patient = numero_patient,
    temps = as.numeric(tab[ligne_scores, 7][[1]]),
    categories = as.numeric(tab[ligne_scores, 8][[1]]),
    cartes_6_cat = as.numeric(tab[ligne_scores, 9][[1]]),
    erreurs_totales = as.numeric(tab[ligne_scores, 10][[1]]),
    perseverations = as.numeric(tab[ligne_scores, 11][[1]]),
    pct_perseverations = as.numeric(tab[ligne_scores, 12][[1]])
  )
}

tabl_scores_classiques <- map_dfr(
  fichiers,
  extraire_scores
)

View(tabl_scores_classiques)

# ============================================================
# 2. IMPORT DES DONNEES SOCIODEMOGRAPHIQUES ET CLINIQUES
# ============================================================

scores_MADRS <- read_excel("scores_MADRS.xlsx") %>%
  mutate(
    patient = str_extract(as.character(ID), "\\d+"),
    patient = str_pad(patient, width = 3, side = "left", pad = "0")
  ) %>%
  select(
    patient,
    age_M0,
    sexe_M0,
    Etudes,
    madrstot_M0,
    staiatot_M0
  ) %>%
  mutate(
    across(
      -patient,
      as.numeric
    )
  )

# ============================================================
# 3. FUSION
#    Ici aucun patient n'est supprimé.
# ============================================================

donnees_description <- tabl_scores_classiques %>%
  left_join(
    scores_MADRS,
    by = "patient"
  )

View(donnees_description)

# ============================================================
# 4. DESCRIPTION VARIABLE PAR VARIABLE
#    Tableau propre : variable / n / moyenne / mediane / ecart-type / normalite
# ============================================================

# Variables quantitatives uniquement
variables_quantitatives <- donnees_description %>%
  select(
    temps,
    categories,
    cartes_6_cat,
    erreurs_totales,
    perseverations,
    pct_perseverations,
    age_M0,
    Etudes,
    madrstot_M0,
    staiatot_M0
  )

# Noms lisibles pour le tableau final
noms_variables <- tibble(
  variable = c(
    "temps",
    "categories",
    "cartes_6_cat",
    "erreurs_totales",
    "perseverations",
    "pct_perseverations",
    "age_M0",
    "Etudes",
    "madrstot_M0",
    "staiatot_M0"
  ),
  Variable = c(
    "Temps de passation",
    "Nombre de catégories complétées",
    "Nombre de cartes pour 6 catégories",
    "Nombre total d’erreurs",
    "Nombre de persévérations",
    "Pourcentage de persévérations",
    "Âge à M0",
    "Nombre d’années d’études",
    "Score MADRS total à M0",
    "Score STAI-A total à M0"
  )
)

description_quantitative <- variables_quantitatives %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  group_by(variable) %>%
  summarise(
    `N patients` = sum(!is.na(valeur)),
    Moyenne = round(mean(valeur, na.rm = TRUE), 2),
    Médiane = round(median(valeur, na.rm = TRUE), 2),
    `Écart-type` = round(sd(valeur, na.rm = TRUE), 2),
    `p Shapiro-Wilk` = {
      x <- na.omit(valeur)
      if (length(x) >= 3 & length(unique(x)) > 1) {
        round(shapiro.test(x)$p.value, 4)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    Normalité = case_when(
      is.na(`p Shapiro-Wilk`) ~ "Non testable",
      `p Shapiro-Wilk` >= 0.05 ~ paste0("Compatible avec la normalité (p = ", `p Shapiro-Wilk`, ")"),
      `p Shapiro-Wilk` < 0.05 ~ paste0("Distribution non normale (p = ", `p Shapiro-Wilk`, ")")
    )
  ) %>%
  left_join(noms_variables, by = "variable") %>%
  select(
    Variable,
    `N patients`,
    Moyenne,
    Médiane,
    `Écart-type`,
    Normalité
  )

View(description_quantitative)

# ============================================================
# 4bis. DESCRIPTION DE LA VARIABLE SEXE
#    Variable qualitative : effectifs et pourcentages
# ============================================================

# ============================================================
# 4bis. DESCRIPTION DE LA VARIABLE SEXE
#    Variable qualitative : codes 1 et 2 uniquement
# ============================================================

description_sexe <- donnees_description %>%
  filter(!is.na(sexe_M0)) %>%
  mutate(
    Sexe = as.character(sexe_M0)
  ) %>%
  count(Sexe, name = "Effectif") %>%
  mutate(
    Pourcentage = round(100 * Effectif / sum(Effectif), 2)
  ) %>%
  select(
    Sexe,
    Effectif,
    Pourcentage
  )

View(description_sexe)
# ============================================================
# 5. EXPORT DES TABLEAUX
# ============================================================

# ------------------------------------------------------------
# 5.1 Export Excel complet
# ------------------------------------------------------------

write_xlsx(
  list(
    scores_patient_par_patient = donnees_description,
    description_quantitative = description_quantitative,
    description_sexe = description_sexe
  ),
  "description_scores_classiques_et_donnees_sociodemographiques.xlsx"
)

# ------------------------------------------------------------
# 5.2 Export Word avec beau tableau
# ------------------------------------------------------------

tableau_quantitatif_word <- description_quantitative %>%
  flextable() %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = "Variable", align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  set_caption(
    caption = "Tableau 1. Description des scores classiques de flexibilité cognitive et des variables quantitatives à M0"
  )

tableau_sexe_word <- description_sexe %>%
  flextable() %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = "Sexe", align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  set_caption(
    caption = "Tableau 2. Description de la répartition du sexe à M0"
  )

document_word <- read_docx() %>%
  body_add_par(
    "Description des scores classiques de flexibilité cognitive et des données socio-démographiques",
    style = "heading 1"
  ) %>%
  body_add_flextable(tableau_quantitatif_word) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(tableau_sexe_word)

print(
  document_word,
  target = "tableau_description_scores_classiques_et_donnees_sociodemographiques.docx"
)
# ============================================================
# 6. HISTOGRAMMES DES VARIABLES QUANTITATIVES
# ============================================================

histogrammes <- variables_quantitatives %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  left_join(noms_variables, by = "variable") %>%
  mutate(
    Variable = factor(Variable, levels = noms_variables$Variable)
  ) %>%
  drop_na(valeur) %>%
  ggplot(aes(x = valeur)) +
  geom_histogram(
    bins = 10,
    color = "black",
    fill = "grey80"
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free"
  ) +
  labs(
    title = "Distribution des scores classiques et variables quantitatives à M0",
    x = "Valeur",
    y = "Effectif"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 9),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10)
  )

print(histogrammes)

ggsave(
  filename = "histogrammes_scores_classiques_et_variables_quantitatives.png",
  plot = histogrammes,
  width = 12,
  height = 8,
  dpi = 300
)


# ============================================================
# 6bis. GRAPHIQUE DE REPARTITION DU SEXE
# ============================================================

graphique_sexe <- description_sexe %>%
  ggplot(aes(x = Sexe, y = Effectif)) +
  geom_col(
    color = "black",
    fill = "grey80"
  ) +
  labs(
    title = "Répartition du sexe",
    x = "Sexe",
    y = "Effectif"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

print(graphique_sexe)

ggsave(
  filename = "repartition_sexe_M0.png",
  plot = graphique_sexe,
  width = 6,
  height = 5,
  dpi = 300
)
