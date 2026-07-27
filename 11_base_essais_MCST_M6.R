library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(stringr)

# ------------------------------------------------------------
# 1. Liste des fichiers Excel
# ------------------------------------------------------------

fichiers <- list.files(
  path = ".",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# On exclut le fichier exporté s'il existe déjà
fichiers <- fichiers[!str_detect(basename(fichiers), "essais_MCST_M0")]

length(fichiers)

# ------------------------------------------------------------
# 2. Fonction d'extraction simple
# ------------------------------------------------------------

extraire_essais <- function(fichier) {
  
  tab <- read_excel(fichier, col_names = FALSE)
  
  # Ligne d'en-tête contenant "Numéro"
  ligne_entete <- which(
    apply(tab, 1, function(x) {
      any(
        str_detect(
          str_to_lower(as.character(x)),
          "numéro|numero|num"
        ),
        na.rm = TRUE
      )
    })
  )[1]
  
  if (is.na(ligne_entete)) {
    warning(paste("Pas de ligne Numéro trouvée dans :", basename(fichier)))
    return(NULL)
  }
  
  # Numéro patient depuis le nom du fichier
  nom_patient <- tools::file_path_sans_ext(basename(fichier))
  
  numero_patient <- str_match(
    nom_patient,
    "lgd_?(\\d+)"
  )[, 2]
  
  if (is.na(numero_patient)) {
    numero_patient <- nom_patient
  }
  
  # Lignes après l'en-tête
  essais_bruts <- tab %>%
    slice((ligne_entete + 1):n()) %>%
    filter(grepl("^[0-9]+$", as.character(.data[["...1"]])))
  
  # Colonnes de cotation : colonnes 3 à 6
  cols_cotation <- names(essais_bruts)[3:min(6, ncol(essais_bruts))]
  
  essais <- essais_bruts %>%
    mutate(
      patient = as.character(numero_patient),
      essai = as.numeric(.data[["...1"]]),
      
      ERR = if_else(
        if_any(
          all_of(cols_cotation),
          ~ str_to_upper(str_trim(as.character(.x))) == "E"
        ),
        1, 0
      ),
      
      PE = if_else(
        if_any(
          all_of(cols_cotation),
          ~ str_to_upper(str_trim(as.character(.x))) == "P"
        ),
        1, 0
      ),
      
      COR = if_else(
        if_any(
          all_of(cols_cotation),
          ~ str_to_upper(str_trim(as.character(.x))) %in% c("1", "2", "3", "4", "5", "6", "+")
        ),
        1, 0
      ),
      
      abandon = if_else(
        if_any(
          everything(),
          ~ str_detect(
            str_to_lower(as.character(.x)),
            "abandon"
          )
        ),
        1, 0
      )
    ) %>%
    select(
      patient,
      essai,
      COR,
      PE,
      ERR,
      abandon
    )
  
  return(essais)
}

# ------------------------------------------------------------
# 3. Création de base_essais
# ------------------------------------------------------------

base_essais <- map_dfr(fichiers, extraire_essais)

# ------------------------------------------------------------
# 4. Vérifications simples
# ------------------------------------------------------------

View(base_essais)

base_essais %>%
  summarise(
    n_patients = n_distinct(patient),
    n_essais = n(),
    total_COR = sum(COR, na.rm = TRUE),
    total_ERR = sum(ERR, na.rm = TRUE),
    total_PE = sum(PE, na.rm = TRUE),
    total_abandon = sum(abandon, na.rm = TRUE)
  )

# ------------------------------------------------------------
# 6. Export Excel
# ------------------------------------------------------------

write_xlsx(
  list(
    base_essais = base_essais
  ),
  "essais_MCST_M6.xlsx"
)
