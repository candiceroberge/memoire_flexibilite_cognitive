library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(stringr)
library(tibble)

# ============================================================
# CREATION DE LA BASE ESSAI PAR ESSAI DU MCST M0
#
# Tableau final :
# patient | essai | C | F | N | A | abandon
#
# Dans les colonnes C, F, N et A :
# - P                 -> PE
# - E                 -> ERR
# - 1, 2, 3, 4, 5, 6 -> COR
# - +                 -> COR
# - absence de réponse -> cellule vide
#
# Dans la colonne abandon :
# - essai auquel le patient abandonne -> 1
# - tous les autres essais            -> cellule vide
# ============================================================

# ------------------------------------------------------------
# 1. Liste des fichiers Excel
# ------------------------------------------------------------

fichiers <- list.files(
  path = ".",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Exclusion des fichiers temporaires Excel
fichiers <- fichiers[
  !str_detect(
    basename(fichiers),
    "^~\\$"
  )
]

# Exclusion du fichier final s'il existe déjà
fichiers <- fichiers[
  !str_detect(
    basename(fichiers),
    "^essais_MCST_M0"
  )
]

cat(
  "Nombre de fichiers patients détectés : ",
  length(fichiers),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# 2. Fonctions utiles
# ------------------------------------------------------------

normaliser_patient <- function(x) {
  
  str_pad(
    as.character(x),
    width = 3,
    side = "left",
    pad = "0"
  )
}

convertir_cotation <- function(x) {
  
  x <- str_to_upper(
    str_trim(
      as.character(x)
    )
  )
  
  case_when(
    x == "P" ~ "PE",
    x == "E" ~ "ERR",
    x %in% c(
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "+"
    ) ~ "COR",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------
# 3. Fonction d'extraction d'un fichier patient
# ------------------------------------------------------------

extraire_essais <- function(fichier) {
  
  tab <- read_excel(
    fichier,
    col_names = FALSE
  )
  
  # ----------------------------------------------------------
  # Recherche de la ligne d'en-tête contenant "Numéro"
  # ----------------------------------------------------------
  
  ligne_entete <- which(
    apply(
      tab,
      1,
      function(ligne) {
        
        valeurs <- str_to_lower(
          str_trim(
            as.character(ligne)
          )
        )
        
        any(
          valeurs %in% c(
            "numéro",
            "numero",
            "num"
          ),
          na.rm = TRUE
        )
      }
    )
  )[1]
  
  if (is.na(ligne_entete)) {
    
    warning(
      paste(
        "Aucune ligne d'en-tête trouvée dans :",
        basename(fichier)
      )
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------
  # Numéro du patient à partir du nom du fichier
  # Exemple : su_lgd_004DI.xlsx -> 004
  # ----------------------------------------------------------
  
  nom_patient <- tools::file_path_sans_ext(
    basename(fichier)
  )
  
  numero_patient <- str_match(
    str_to_lower(nom_patient),
    "lgd_?(\\d+)"
  )[, 2]
  
  if (is.na(numero_patient)) {
    
    warning(
      paste(
        "Numéro de patient non identifié dans :",
        basename(fichier)
      )
    )
    
    numero_patient <- nom_patient
    
  } else {
    
    numero_patient <- normaliser_patient(
      numero_patient
    )
  }
  
  # ----------------------------------------------------------
  # Lecture des intitulés de colonnes
  # ----------------------------------------------------------
  
  noms_entete <- tab %>%
    slice(ligne_entete) %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    str_trim()
  
  noms_entete_minuscules <- str_to_lower(
    noms_entete
  )
  
  noms_entete_majuscules <- str_to_upper(
    noms_entete
  )
  
  # Position de la colonne du numéro d'essai
  position_numero <- which(
    noms_entete_minuscules %in% c(
      "numéro",
      "numero",
      "num"
    )
  )[1]
  
  if (is.na(position_numero)) {
    position_numero <- 1
  }
  
  # Positions des quatre catégories
  position_C <- which(
    noms_entete_majuscules == "C"
  )[1]
  
  position_F <- which(
    noms_entete_majuscules == "F"
  )[1]
  
  position_N <- which(
    noms_entete_majuscules == "N"
  )[1]
  
  position_A <- which(
    noms_entete_majuscules == "A"
  )[1]
  
  positions_categories <- c(
    C = position_C,
    F = position_F,
    N = position_N,
    A = position_A
  )
  
  if (any(is.na(positions_categories))) {
    
    warning(
      paste0(
        "Les quatre colonnes C, F, N et A n'ont pas été trouvées dans : ",
        basename(fichier),
        "\nEn-têtes détectés : ",
        paste(noms_entete, collapse = ", ")
      )
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------
  # Extraction des lignes situées après l'en-tête
  # ----------------------------------------------------------
  
  if (ligne_entete >= nrow(tab)) {
    
    warning(
      paste(
        "Aucune ligne après l'en-tête dans :",
        basename(fichier)
      )
    )
    
    return(NULL)
  }
  
  essais_bruts <- tab %>%
    slice(
      (ligne_entete + 1):nrow(tab)
    )
  
  # ----------------------------------------------------------
  # Identification des lignes correspondant à des essais
  # ----------------------------------------------------------
  
  numero_essai_brut <- essais_bruts[[position_numero]]
  
  numero_essai <- suppressWarnings(
    as.integer(
      as.character(numero_essai_brut)
    )
  )
  
  lignes_essais <- !is.na(numero_essai)
  
  essais_bruts <- essais_bruts[
    lignes_essais,
    ,
    drop = FALSE
  ]
  
  numero_essai <- numero_essai[
    lignes_essais
  ]
  
  if (nrow(essais_bruts) == 0) {
    
    warning(
      paste(
        "Aucun essai numérique trouvé dans :",
        basename(fichier)
      )
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------
  # Extraction et conversion des cotations C, F, N et A
  # ----------------------------------------------------------
  
  colonne_C <- convertir_cotation(
    essais_bruts[[position_C]]
  )
  
  colonne_F <- convertir_cotation(
    essais_bruts[[position_F]]
  )
  
  colonne_N <- convertir_cotation(
    essais_bruts[[position_N]]
  )
  
  colonne_A <- convertir_cotation(
    essais_bruts[[position_A]]
  )
  
  # ----------------------------------------------------------
  # Détection de l'abandon essai par essai
  #
  # 1  = abandon indiqué sur la ligne
  # NA = absence d'abandon, donc cellule vide dans Excel
  # ----------------------------------------------------------
  
  abandon <- apply(
    essais_bruts,
    1,
    function(ligne) {
      
      texte_ligne <- str_to_lower(
        as.character(ligne)
      )
      
      abandon_detecte <- any(
        str_detect(
          texte_ligne,
          "abandon"
        ),
        na.rm = TRUE
      )
      
      if (abandon_detecte) {
        return(1)
      } else {
        return(NA_real_)
      }
    }
  )
  
  # ----------------------------------------------------------
  # Construction de la table finale du patient
  # ----------------------------------------------------------
  
  resultat_patient <- tibble(
    patient = numero_patient,
    essai = numero_essai,
    C = colonne_C,
    F = colonne_F,
    N = colonne_N,
    A = colonne_A,
    abandon = abandon
  ) %>%
    arrange(essai)
  
  return(resultat_patient)
}

# ------------------------------------------------------------
# 4. Création de la base complète
# ------------------------------------------------------------

base_essais <- map_dfr(
  fichiers,
  extraire_essais
) %>%
  arrange(
    patient,
    essai
  )

# ------------------------------------------------------------
# 5. Vérification du nombre de cotations par essai
# ------------------------------------------------------------

verification_cotations <- base_essais %>%
  rowwise() %>%
  mutate(
    nb_cotations_ligne = sum(
      !is.na(
        c_across(
          c(C, F, N, A)
        )
      )
    )
  ) %>%
  ungroup()

# Essais sans cotation, hors abandon
essais_sans_cotation <- verification_cotations %>%
  filter(
    nb_cotations_ligne == 0,
    is.na(abandon)
  )

# Essais avec plusieurs cotations
essais_plusieurs_cotations <- verification_cotations %>%
  filter(
    nb_cotations_ligne > 1
  )

# ------------------------------------------------------------
# 6. Résumé général
# ------------------------------------------------------------

resume_base <- base_essais %>%
  summarise(
    n_patients = n_distinct(patient),
    n_essais = n(),
    
    total_COR = sum(
      C == "COR",
      F == "COR",
      N == "COR",
      A == "COR",
      na.rm = TRUE
    ),
    
    total_PE = sum(
      C == "PE",
      F == "PE",
      N == "PE",
      A == "PE",
      na.rm = TRUE
    ),
    
    total_ERR = sum(
      C == "ERR",
      F == "ERR",
      N == "ERR",
      A == "ERR",
      na.rm = TRUE
    ),
    
    total_abandon = sum(
      abandon == 1,
      na.rm = TRUE
    )
  )

cat("\nRésumé de la base créée :\n")
print(resume_base)

cat(
  "\nNombre d'essais sans cotation : ",
  nrow(essais_sans_cotation),
  "\n",
  sep = ""
)

cat(
  "Nombre d'essais avec plusieurs cotations : ",
  nrow(essais_plusieurs_cotations),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# 7. Affichage des tables
# ------------------------------------------------------------

View(base_essais)
View(essais_sans_cotation)
View(essais_plusieurs_cotations)

# ------------------------------------------------------------
# 8. Export Excel
# ------------------------------------------------------------

write_xlsx(
  list(
    base_essais = base_essais
  ),
  "essais__dynamiques_MCST_M0.xlsx"
)

cat(
  "\nFichier créé : essais_MCST_M0.xlsx\n"
)
