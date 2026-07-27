library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)
library(ggplot2)
library(flextable)
library(officer)

# ============================================================
# DESCRIPTION DES INDICATEURS DYNAMIQUES DU MCST - M0
#
# Un seul script :
# - import de la base d'essais C / F / N / A
# - reconstruction de COR / PE / ERR
# - identification des changements de règle
# - calcul des variables patient × CR
# - calcul des indicateurs dynamiques par patient
# - tableau descriptif unique
# - export Excel, Word et histogrammes
# ============================================================

# ============================================================
# 0. PARAMETRES
# ============================================================

FICHIER_ENTREE <- "essais__dynamiques_MCST_M0.xlsx"
FEUILLE_ENTREE <- "base_essais"

FICHIER_EXCEL_SORTIE <-
  "description_variables_dynamiques_MCST_M0.xlsx"

FICHIER_WORD_SORTIE <-
  "tableau_description_variables_dynamiques_MCST_M0.docx"

FICHIER_HISTOGRAMMES <-
  "histogrammes_variables_dynamiques_MCST_M0.png"

NB_COR_CATEGORIE_COMPLETEE <- 6L
SEUIL_RUPTURE_SET <- 3L

# ============================================================
# 1. FONCTIONS UTILES
# ============================================================

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

normaliser_reponse <- function(x) {
  
  x <- str_to_upper(
    str_trim(
      as.character(x)
    )
  )
  
  case_when(
    x %in% c("COR", "PE", "ERR") ~ x,
    TRUE ~ NA_character_
  )
}

moyenne_securisee <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  mean(x)
}

mediane_securisee <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  median(x)
}

maximum_securise <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(0)
  }
  
  max(x)
}

ecart_type_securise <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) <= 1) {
    return(NA_real_)
  }
  
  sd(x)
}

p_shapiro_securise <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (
    length(x) >= 3 &&
    length(x) <= 5000 &&
    length(unique(x)) > 1
  ) {
    return(
      shapiro.test(x)$p.value
    )
  }
  
  NA_real_
}

# ============================================================
# 2. IMPORT DE LA BASE D'ESSAIS
# ============================================================

base_brute <- read_excel(
  FICHIER_ENTREE,
  sheet = FEUILLE_ENTREE
)

colonnes_attendues <- c(
  "patient",
  "essai",
  "C",
  "F",
  "N",
  "A",
  "abandon"
)

colonnes_absentes <- setdiff(
  colonnes_attendues,
  names(base_brute)
)

if (length(colonnes_absentes) > 0) {
  
  stop(
    paste0(
      "Colonnes absentes : ",
      paste(
        colonnes_absentes,
        collapse = ", "
      ),
      "\n\nColonnes disponibles : ",
      paste(
        names(base_brute),
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 3. NETTOYAGE DE LA BASE
# ============================================================

base_brute <- base_brute %>%
  mutate(
    patient = normaliser_patient(patient),
    
    essai = as.integer(essai),
    
    across(
      c(C, F, N, A),
      normaliser_reponse
    ),
    
    abandon = suppressWarnings(
      as.numeric(abandon)
    ),
    
    abandon = if_else(
      !is.na(abandon) & abandon > 0,
      1L,
      0L
    )
  ) %>%
  filter(
    !is.na(patient),
    !is.na(essai)
  ) %>%
  arrange(
    patient,
    essai
  )

# ============================================================
# 4. PASSAGE AU FORMAT LONG
#
# Une ligne par patient × essai × catégorie renseignée
# ============================================================

base_longue <- base_brute %>%
  pivot_longer(
    cols = c(C, F, N, A),
    names_to = "categorie_choisie",
    values_to = "reponse",
    values_drop_na = TRUE
  )

# Vérification des essais avec plusieurs catégories renseignées

verification_cotations <- base_longue %>%
  count(
    patient,
    essai,
    name = "nb_cotations"
  )

essais_plusieurs_cotations <- verification_cotations %>%
  filter(
    nb_cotations > 1
  )

if (nrow(essais_plusieurs_cotations) > 0) {
  
  warning(
    paste0(
      nrow(essais_plusieurs_cotations),
      " essai(s) comportent plusieurs catégories renseignées. ",
      "La première cotation sera conservée."
    )
  )
}

# ============================================================
# 5. RECONSTRUCTION D'UNE LIGNE PAR ESSAI
# ============================================================

cotations_essais <- base_longue %>%
  arrange(
    patient,
    essai,
    categorie_choisie
  ) %>%
  distinct(
    patient,
    essai,
    .keep_all = TRUE
  ) %>%
  select(
    patient,
    essai,
    categorie_choisie,
    reponse
  )

base_essais <- base_brute %>%
  select(
    patient,
    essai,
    abandon
  ) %>%
  left_join(
    cotations_essais,
    by = c(
      "patient",
      "essai"
    )
  ) %>%
  mutate(
    COR = as.integer(
      reponse == "COR"
    ),
    
    PE = as.integer(
      reponse == "PE"
    ),
    
    ERR = as.integer(
      reponse == "ERR"
    ),
    
    COR = replace_na(
      COR,
      0L
    ),
    
    PE = replace_na(
      PE,
      0L
    ),
    
    ERR = replace_na(
      ERR,
      0L
    ),
    
    feedback = case_when(
      COR == 1 ~ "positif",
      PE == 1 | ERR == 1 ~ "negatif",
      TRUE ~ NA_character_
    ),
    
    essai_reel =
      COR == 1 |
      PE == 1 |
      ERR == 1 |
      abandon == 1
  ) %>%
  filter(
    essai_reel
  ) %>%
  arrange(
    patient,
    essai
  )

# ============================================================
# 6. IDENTIFICATION DES CATEGORIES COMPLETEES ET DES CR
# ============================================================

identifier_categories_patient <- function(df_patient) {
  
  df_patient <- df_patient %>%
    arrange(essai)
  
  n <- nrow(df_patient)
  
  compteur_COR <- 0L
  nb_categories_completees <- 0L
  
  categorie_completee <- integer(n)
  categories_avant_essai <- integer(n)
  serie_COR_avant_essai <- integer(n)
  
  for (i in seq_len(n)) {
    
    categories_avant_essai[i] <-
      nb_categories_completees
    
    serie_COR_avant_essai[i] <-
      compteur_COR
    
    if (df_patient$COR[i] == 1) {
      
      compteur_COR <- compteur_COR + 1L
      
      if (
        compteur_COR ==
        NB_COR_CATEGORIE_COMPLETEE
      ) {
        
        categorie_completee[i] <- 1L
        
        nb_categories_completees <-
          nb_categories_completees + 1L
        
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
      categorie_completee =
        categorie_completee,
      
      nb_categories_avant_essai =
        categories_avant_essai,
      
      serie_COR_avant_essai =
        serie_COR_avant_essai,
      
      CR = if_else(
        nb_categories_avant_essai >= 1,
        nb_categories_avant_essai,
        NA_integer_
      )
    )
}

base_essais <- base_essais %>%
  group_by(patient) %>%
  group_modify(
    ~ identifier_categories_patient(.x)
  ) %>%
  ungroup() %>%
  arrange(
    patient,
    essai
  )

# ============================================================
# 7. TABLE DYNAMIQUE PATIENT × CR
# ============================================================

table_CR_dynamique <- base_essais %>%
  filter(
    !is.na(CR)
  ) %>%
  group_by(
    patient,
    CR
  ) %>%
  summarise(
    nb_CORR_par_CR = sum(
      COR,
      na.rm = TRUE
    ),
    
    nb_ERR_SIMPLE_par_CR = sum(
      ERR,
      na.rm = TRUE
    ),
    
    nb_PE_par_CR = sum(
      PE,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    patient,
    CR
  )


View(table_CR_dynamique)

# ============================================================
# 8. TRANSITIONS APRES FEEDBACK
# ============================================================

base_transitions <- base_essais %>%
  group_by(patient) %>%
  arrange(
    essai,
    .by_group = TRUE
  ) %>%
  mutate(
    categorie_suivante =
      lead(categorie_choisie),
    
    essai_suivant =
      lead(essai),
    
    transition_comparable =
      !is.na(categorie_choisie) &
      !is.na(categorie_suivante) &
      !is.na(essai_suivant),
    
    changement_categorie = case_when(
      !transition_comparable ~ NA_integer_,
      
      categorie_suivante !=
        categorie_choisie ~ 1L,
      
      categorie_suivante ==
        categorie_choisie ~ 0L
    ),
    
    maintien_categorie = case_when(
      is.na(changement_categorie) ~
        NA_integer_,
      
      changement_categorie == 0L ~
        1L,
      
      changement_categorie == 1L ~
        0L
    ),
    
    # Le sixième COR est exclu :
    # il entraîne normalement un changement de règle.
    feedback_positif_comparable =
      feedback == "positif" &
      categorie_completee == 0L &
      transition_comparable,
    
    feedback_negatif_comparable =
      feedback == "negatif" &
      transition_comparable
  ) %>%
  ungroup()

# ============================================================
# 9. PROBABILITES APRES FEEDBACK
# ============================================================

variables_feedback <- base_transitions %>%
  group_by(patient) %>%
  summarise(
    nb_feedbacks_negatifs_comparables =
      sum(
        feedback_negatif_comparable,
        na.rm = TRUE
      ),
    
    nb_changements_apres_feedback_negatif =
      sum(
        feedback_negatif_comparable &
          changement_categorie == 1L,
        na.rm = TRUE
      ),
    
    probabilite_changement_apres_feedback_negatif =
      if_else(
        nb_feedbacks_negatifs_comparables > 0,
        
        nb_changements_apres_feedback_negatif /
          nb_feedbacks_negatifs_comparables,
        
        NA_real_
      ),
    
    nb_feedbacks_positifs_comparables =
      sum(
        feedback_positif_comparable,
        na.rm = TRUE
      ),
    
    nb_maintiens_apres_feedback_positif =
      sum(
        feedback_positif_comparable &
          maintien_categorie == 1L,
        na.rm = TRUE
      ),
    
    probabilite_maintien_apres_feedback_positif =
      if_else(
        nb_feedbacks_positifs_comparables > 0,
        
        nb_maintiens_apres_feedback_positif /
          nb_feedbacks_positifs_comparables,
        
        NA_real_
      ),
    
    .groups = "drop"
  )

# ============================================================
# 10. NOMBRE D'ESSAIS POUR COMPLETER UNE NOUVELLE CATEGORIE
#
# Le premier bloc avant le premier changement de règle
# est exclu.
# Les CR non terminés ne sont pas inclus dans la moyenne.
# ============================================================

essais_par_CR <- base_essais %>%
  filter(
    !is.na(CR)
  ) %>%
  group_by(
    patient,
    CR
  ) %>%
  summarise(
    nb_essais_pour_completer_categorie =
      n(),
    
    categorie_completee_dans_CR =
      as.integer(
        any(
          categorie_completee == 1,
          na.rm = TRUE
        )
      ),
    
    .groups = "drop"
  )

variables_essais_CR <- essais_par_CR %>%
  filter(
    categorie_completee_dans_CR == 1
  ) %>%
  group_by(patient) %>%
  summarise(
    nb_essais_nouvelle_categorie_moy =
      moyenne_securisee(
        nb_essais_pour_completer_categorie
      ),
    
    .groups = "drop"
  )

# ============================================================
# 11. LONGUEUR DES SEQUENCES PERSEVERATIVES
#
# Une séquence correspond à plusieurs PE consécutives.
# ============================================================

base_sequences_PE <- base_essais %>%
  group_by(patient) %>%
  arrange(
    essai,
    .by_group = TRUE
  ) %>%
  mutate(
    debut_sequence_PE =
      PE == 1 &
      (
        is.na(lag(PE)) |
          lag(PE) == 0
      ),
    
    identifiant_sequence_PE =
      cumsum(
        replace_na(
          debut_sequence_PE,
          FALSE
        )
      )
  ) %>%
  ungroup()

sequences_PE <- base_sequences_PE %>%
  filter(
    PE == 1
  ) %>%
  group_by(
    patient,
    identifiant_sequence_PE
  ) %>%
  summarise(
    longueur_sequence_PE = n(),
    .groups = "drop"
  )

variables_sequences_PE <- base_essais %>%
  distinct(patient) %>%
  left_join(
    sequences_PE %>%
      group_by(patient) %>%
      summarise(
        longueur_sequences_PE_moy =
          moyenne_securisee(
            longueur_sequence_PE
          ),
        
        longueur_sequences_PE_max =
          maximum_securise(
            longueur_sequence_PE
          ),
        
        .groups = "drop"
      ),
    
    by = "patient"
  ) %>%
  mutate(
    longueur_sequences_PE_moy =
      replace_na(
        longueur_sequences_PE_moy,
        0
      ),
    
    longueur_sequences_PE_max =
      replace_na(
        longueur_sequences_PE_max,
        0
      )
  )

# ============================================================
# 12. RUPTURES DE SET APRES AU MOINS 3 COR CONSECUTIFS
# ============================================================

base_ruptures <- base_essais %>%
  mutate(
    occasion_rupture_set =
      serie_COR_avant_essai >=
      SEUIL_RUPTURE_SET,
    
    rupture_set =
      occasion_rupture_set &
      (
        PE == 1 |
          ERR == 1
      )
  )

variables_ruptures <- base_ruptures %>%
  group_by(patient) %>%
  summarise(
    nb_occasions_rupture_set_apres_3_COR =
      sum(
        occasion_rupture_set,
        na.rm = TRUE
      ),
    
    nb_ruptures_set_apres_3_COR =
      sum(
        rupture_set,
        na.rm = TRUE
      ),
    
    probabilite_rupture_set_apres_3_COR =
      if_else(
        nb_occasions_rupture_set_apres_3_COR > 0,
        
        nb_ruptures_set_apres_3_COR /
          nb_occasions_rupture_set_apres_3_COR,
        
        NA_real_
      ),
    
    .groups = "drop"
  )

# ============================================================
# 13. PROPORTION D'EP PARMI LES ERREURS TOTALES
#
# Proportion = PE / (PE + erreurs simples)
# ============================================================

variables_proportion_PE <- base_essais %>%
  group_by(patient) %>%
  summarise(
    nb_PE_total = sum(
      PE,
      na.rm = TRUE
    ),
    
    nb_ERR_total = sum(
      ERR,
      na.rm = TRUE
    ),
    
    nb_erreurs_total =
      nb_PE_total + nb_ERR_total,
    
    proportion_PE_parmi_erreurs =
      if_else(
        nb_erreurs_total > 0,
        
        nb_PE_total /
          nb_erreurs_total,
        
        NA_real_
      ),
    
    .groups = "drop"
  )

# ============================================================
# 14. FUSION DES INDICATEURS DYNAMIQUES PAR PATIENT
# ============================================================

indicateurs_dynamiques_patient <- base_essais %>%
  distinct(patient) %>%
  
  left_join(
    variables_feedback,
    by = "patient"
  ) %>%
  
  left_join(
    variables_essais_CR,
    by = "patient"
  ) %>%
  
  left_join(
    variables_sequences_PE,
    by = "patient"
  ) %>%
  
  left_join(
    variables_ruptures,
    by = "patient"
  ) %>%
  
  left_join(
    variables_proportion_PE,
    by = "patient"
  ) %>%
  
  select(
    patient,
    
    probabilite_changement_apres_feedback_negatif,
    
    probabilite_maintien_apres_feedback_positif,
    
    nb_essais_nouvelle_categorie_moy,
    
    longueur_sequences_PE_moy,
    
    longueur_sequences_PE_max,
    
    nb_ruptures_set_apres_3_COR,
    
    probabilite_rupture_set_apres_3_COR,
    
    proportion_PE_parmi_erreurs
  ) %>%
  
  arrange(patient)

View(indicateurs_dynamiques_patient)

# ============================================================
# 15. NOMS LISIBLES DES VARIABLES
# ============================================================

noms_variables_CR <- tibble(
  variable = c(
    "nb_CORR_par_CR",
    "nb_ERR_SIMPLE_par_CR",
    "nb_PE_par_CR"
  ),
  
  Variable = c(
    "Nombre d’essais corrects par CR",
    "Nombre d’erreurs simples par CR",
    "Nombre d’EP par CR"
  )
)

noms_indicateurs_patient <- tibble(
  variable = c(
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
    "Probabilité de changer de règle après un feedback négatif",
    "Probabilité de maintenir la règle après un feedback positif",
    "Nombre moyen d’essais nécessaires pour compléter une nouvelle catégorie",
    "Longueur moyenne des séquences persévératives",
    "Longueur maximale des séquences persévératives",
    "Nombre de ruptures de set après au moins 3 réponses correctes",
    "Probabilité de rupture de set après au moins 3 réponses correctes",
    "Proportion d’EP parmi les erreurs totales"
  )
)

variables_CR <-
  noms_variables_CR$variable

variables_patient <-
  noms_indicateurs_patient$variable

# ============================================================
# 16. FORMAT LONG DES VARIABLES PATIENT × CR
# ============================================================

variables_CR_long <- table_CR_dynamique %>%
  select(
    patient,
    CR,
    all_of(variables_CR)
  ) %>%
  pivot_longer(
    cols = all_of(variables_CR),
    names_to = "variable",
    values_to = "valeur"
  )

# ============================================================
# 17. FORMAT LONG DES INDICATEURS PAR PATIENT
# ============================================================

variables_patient_long <-
  indicateurs_dynamiques_patient %>%
  
  select(
    patient,
    all_of(variables_patient)
  ) %>%
  
  pivot_longer(
    cols = all_of(variables_patient),
    names_to = "variable",
    values_to = "valeur"
  )

# ============================================================
# 18. FONCTION DE DESCRIPTION
# ============================================================

decrire_variable <- function(donnees_longues) {
  
  donnees_longues %>%
    group_by(variable) %>%
    
    summarise(
      `N patients` =
        n_distinct(
          patient[!is.na(valeur)]
        ),
      
      Moyenne =
        if (
          all(is.na(valeur))
        ) {
          NA_real_
        } else {
          round(
            mean(
              valeur,
              na.rm = TRUE
            ),
            2
          )
        },
      
      Médiane =
        if (
          all(is.na(valeur))
        ) {
          NA_real_
        } else {
          round(
            median(
              valeur,
              na.rm = TRUE
            ),
            2
          )
        },
      
      `Écart-type` =
        round(
          ecart_type_securise(
            valeur
          ),
          2
        ),
      
      `p Shapiro-Wilk` =
        round(
          p_shapiro_securise(
            valeur
          ),
          4
        ),
      
      .groups = "drop"
    )
}

# ============================================================
# 19. DESCRIPTIONS SEPAREES AVANT FUSION
# ============================================================

description_variables_CR <-
  decrire_variable(
    variables_CR_long
  ) %>%
  left_join(
    noms_variables_CR,
    by = "variable"
  )

description_indicateurs_patient <-
  decrire_variable(
    variables_patient_long
  ) %>%
  left_join(
    noms_indicateurs_patient,
    by = "variable"
  )

# ============================================================
# 20. TABLEAU DESCRIPTIF UNIQUE
# ============================================================

ordre_variables_dynamiques <- c(
  noms_variables_CR$variable,
  noms_indicateurs_patient$variable
)

description_variables_dynamiques <- bind_rows(
  description_variables_CR,
  description_indicateurs_patient
) %>%
  mutate(
    Normalité = case_when(
      is.na(`p Shapiro-Wilk`) ~
        "Non testable",
      
      `p Shapiro-Wilk` >= 0.05 ~
        paste0(
          "Compatible avec la normalité (p = ",
          format(
            `p Shapiro-Wilk`,
            decimal.mark = ",",
            nsmall = 4
          ),
          ")"
        ),
      
      TRUE ~
        paste0(
          "Distribution non normale (p = ",
          format(
            `p Shapiro-Wilk`,
            decimal.mark = ",",
            nsmall = 4
          ),
          ")"
        )
    ),
    
    variable = factor(
      variable,
      levels =
        ordre_variables_dynamiques
    )
  ) %>%
  arrange(variable) %>%
  select(
    Variable,
    `N patients`,
    Moyenne,
    Médiane,
    `Écart-type`,
    Normalité
  )

View(description_variables_dynamiques)
print(description_variables_dynamiques)

# ============================================================
# 21. PATIENTS SANS CHANGEMENT DE REGLE EXPLOITABLE
# ============================================================

patients_base <- base_essais %>%
  distinct(patient)

patients_avec_CR <- table_CR_dynamique %>%
  distinct(patient)

patients_sans_CR <- patients_base %>%
  anti_join(
    patients_avec_CR,
    by = "patient"
  )

View(patients_sans_CR)

# ============================================================
# 22. EXPORT EXCEL
# ============================================================

write_xlsx(
  list(
    donnees_patient_par_CR =
      table_CR_dynamique,
    
    indicateurs_dynamiques_patient =
      indicateurs_dynamiques_patient,
    
    description_variables_dynamiques =
      description_variables_dynamiques,
    
    patients_sans_CR =
      patients_sans_CR
  ),
  
  FICHIER_EXCEL_SORTIE
)

cat(
  "\nFichier Excel créé : ",
  FICHIER_EXCEL_SORTIE,
  "\n",
  sep = ""
)

# ============================================================
# 23. EXPORT WORD
# ============================================================

tableau_dynamiques_word <-
  description_variables_dynamiques %>%
  
  flextable() %>%
  
  autofit() %>%
  
  theme_booktabs() %>%
  
  align(
    align = "center",
    part = "all"
  ) %>%
  
  align(
    j = "Variable",
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
      "Tableau. Description des indicateurs ",
      "dynamiques de flexibilité cognitive"
    )
  )

document_word_dynamiques <- read_docx() %>%
  
  body_add_par(
    "Description des indicateurs dynamiques de flexibilité cognitive",
    style = "heading 1"
  ) %>%
  
  body_add_flextable(
    tableau_dynamiques_word
  )

print(
  document_word_dynamiques,
  target = FICHIER_WORD_SORTIE
)

cat(
  "\nFichier Word créé : ",
  FICHIER_WORD_SORTIE,
  "\n",
  sep = ""
)

# ============================================================
# 24. PREPARATION DES HISTOGRAMMES
# ============================================================

donnees_histogrammes <- bind_rows(
  variables_CR_long %>%
    select(
      patient,
      variable,
      valeur
    ),
  
  variables_patient_long %>%
    select(
      patient,
      variable,
      valeur
    )
) %>%
  
  left_join(
    bind_rows(
      noms_variables_CR,
      noms_indicateurs_patient
    ),
    by = "variable"
  ) %>%
  
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        noms_variables_CR$Variable,
        noms_indicateurs_patient$Variable
      )
    )
  ) %>%
  
  filter(
    !is.na(valeur)
  )

# ============================================================
# 25. HISTOGRAMMES
# ============================================================

histogrammes_variables_dynamiques <- ggplot(
  donnees_histogrammes,
  aes(
    x = valeur
  )
) +
  
  geom_histogram(
    bins = 10,
    color = "black",
    fill = "grey80"
  ) +
  
  facet_wrap(
    ~ Variable,
    scales = "free",
    ncol = 3
  ) +
  
  theme_minimal() +
  
  labs(
    title =
      "Distribution des variables dynamiques du MCST",
    
    x = "Valeur",
    
    y = "Effectif"
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 8
    ),
    
    axis.text = element_text(
      size = 8
    ),
    
    axis.title = element_text(
      size = 10
    ),
    
    panel.grid.minor =
      element_blank()
  )

print(
  histogrammes_variables_dynamiques
)

ggsave(
  filename =
    FICHIER_HISTOGRAMMES,
  
  plot =
    histogrammes_variables_dynamiques,
  
  width = 15,
  height = 12,
  dpi = 300
)

cat(
  "\nHistogrammes créés : ",
  FICHIER_HISTOGRAMMES,
  "\n",
  sep = ""
)

cat(
  "\nAnalyse terminée.\n"
)
