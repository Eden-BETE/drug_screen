# data_prep.R — Génération de data/drug_screen_clean.csv + constantes globales
#
# Si data/drug_screen_clean.csv existe déjà, on le lit directement (rapide).
# Sinon, on repart des CSV bruts, on filtre, joint, passe en format long,
# nettoie les noms de drogues, et on sauvegarde le fichier propre.
#
# Objet produit : df  (Cell.lines | cluster | drug | drug_label | AUC)

library(tidyverse)
library(stringr)

# ── Constantes globales ───────────────────────────────────────────────────────

DEFAULT_AUC_THRESHOLD <- 0.3  # Valeur pas défaut, configurable dans UI
MIN_CELLS     <- 2

CLUSTER_COLORS <- c(
  "MES"  = "#E63946",
  "PN"   = "#457B9D",
  "CL-A" = "#E9C46A",
  "CL-B" = "#2A9D8F",
  "CL-C" = "#9B5DE5",
  "NL"   = "#F4A261",
  "PN-L" = "#6A0572"
)

CLEAN_PATH <- "data/drug_screen_clean.csv"

# ── Lecture ou construction ───────────────────────────────────────────────────

if (file.exists(CLEAN_PATH)) {
  
  df <- read.csv(CLEAN_PATH, stringsAsFactors = FALSE) %>%
    mutate(
      cluster    = factor(cluster, levels = names(CLUSTER_COLORS))
      # responsive removed - will be calculated reactively
    )
  
} else {
  
  message("[data_prep] Fichier propre absent — construction depuis les sources…")
  
  # 1. Chargement brut
  prisma_raw     <- read.csv("data/PRISM Repurposing Secondary Subsetted.csv",
                             stringsAsFactors = FALSE)
  classification <- read.csv("data/classification_uploadedData.csv",
                             stringsAsFactors = FALSE)
  
  # 3. Filtrer sur les lignées sein classifiées
  cell_lines    <- classification$Cell.lines
  id_col        <- names(prisma_raw)[1]   # "X"
  prisma_breast <- prisma_raw[prisma_raw[[id_col]] %in% cell_lines, , drop = FALSE]
  
  # 4. Jointure wide + cluster
  data_wide <- prisma_breast %>%
    rename(Cell.lines = all_of(id_col)) %>%
    inner_join(classification[, c("Cell.lines", "cluster")], by = "Cell.lines")
  
  # 5. Format long
  data_long <- data_wide %>%
    pivot_longer(
      cols      = -c(Cell.lines, cluster),
      names_to  = "drug",
      values_to = "AUC"
    )
  
  # 6. Nettoyage : exclure NA, construire drug_label
  df <- data_long %>%
    filter(!is.na(AUC)) %>%
    mutate(
      drug_label = drug %>%
        str_replace("^X[\\d\\.]+", "") %>%
        str_replace("^X\\.+",       "") %>%
        str_replace_all("\\.",       " ") %>%
        str_trim() %>%
        str_to_title(),
      cluster    = factor(cluster, levels = names(CLUSTER_COLORS))
      # responsive removed
    )
  
  # 7. Sauvegarde (sans responsive)
  df %>%
    select(Cell.lines, cluster, drug, drug_label, AUC) %>%
    write.csv(CLEAN_PATH, row.names = FALSE)
  
  message(sprintf("[data_prep] Fichier sauvegardé : %s", CLEAN_PATH))
}

