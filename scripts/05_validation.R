# -------------------------------------------------------------------------
# Masterarbeit: Unüberwachte Baumartenklassifizierung
# Skript 05: Validierung (ARI & Konfusionsmatrizen)
# Autor: Christian Salzmann (2026)
#
# Beschreibung:
# Dieses Skript vergleicht die Clustering-Ergebnisse (K-Means & Mclust)
# mit den terrestrischen Referenzdaten (Validierungsdaten).
# Es berechnet den Adjusted Rand Index (ARI) und erstellt Konfusionsmatrizen.
# -------------------------------------------------------------------------

# 1. Bibliotheken laden
# install.packages("readxl")
# install.packages("aricode")
# install.packages("dplyr")

library(readxl)
library(aricode)
library(dplyr)

# --- KONFIGURATION & PFADE ---
# Hinweis: Die Validierungsdatei muss manuell erstellt/bereitgestellt werden.
# Sie enthält die 'Wahre Baumart' (aus Inventur) und die Cluster-IDs.

input_validation_file <- "data/Validierung_kmeans_mclust.xlsx" 
# Alternativ als CSV, falls Excel nicht vorhanden:
# input_validation_file <- "data/Validierung_kmeans_mclust.csv"

# -------------------------------------------------------------------------
# 2. Daten laden
# -------------------------------------------------------------------------

if(!file.exists(input_validation_file)) {
  stop(paste("Fehler: Validierungsdatei nicht gefunden:", input_validation_file))
}

print("Lade Validierungsdaten...")

# Unterscheidung nach Dateityp (Excel oder CSV)
if(grepl("\\.xlsx$", input_validation_file)) {
  df <- read_excel(input_validation_file, sheet = 1) 
} else {
  df <- read.csv(input_validation_file)
}

# 'Bemerkung'-Spalte bereinigen
if ("Bemerkung" %in% colnames(df)) {
  if (is.factor(df$Bemerkung)) {
    df$Bemerkung <- as.character(df$Bemerkung)
  }
} else {
  warning("Spalte 'Bemerkung' nicht gefunden. Filterung ggf. nicht möglich.")
  df$Bemerkung <- NA # Dummy-Werte setzen
}

# -------------------------------------------------------------------------
# 3. Datenfilterung
# -------------------------------------------------------------------------

# Filter 1: NUR "sichere" Bäume (Bemerkung ist leer/NA)
df_sicher <- subset(df, is.na(Bemerkung))

# Filter 2: "Zuweisbare" Bäume (sicher + unsicher; "nicht zuweisbar" wird gefiltert)
df_zuweisbar <- subset(df, is.na(Bemerkung) | Bemerkung != "nicht zuweisbar")

print(paste("Anzahl Bäume (Total):", nrow(df)))
print(paste("Anzahl Bäume (Sicher):", nrow(df_sicher)))
print(paste("Anzahl Bäume (Zuweisbar):", nrow(df_zuweisbar)))

# -------------------------------------------------------------------------
# 4. BWI-Kategorien erstellen (Mapping)
# -------------------------------------------------------------------------

# Hilfsfunktion für das Mapping der Baumarten zu BWI-Gruppen
add_bwi_categories <- function(data) {
  if(!"Echte Baumart" %in% colnames(data)) {
    stop("Spalte 'Echte Baumart' fehlt im Datensatz!")
  }
  
  data %>%
    mutate(BWI_Kategorie = case_when(
      `Echte Baumart` == "Buche" ~ "Buche",
      `Echte Baumart` == "Eiche" ~ "Eiche",
      `Echte Baumart` == "Fichte" ~ "Fichte",
      `Echte Baumart` == "Kiefer" ~ "Kiefer",
      `Echte Baumart` == "Lärche" ~ "Lärche",
      `Echte Baumart` == "Tanne" ~ "Fichte", # Tanne zu Fichte gruppieren
      
      # Sonstige Laubhölzer (SLH)
      `Echte Baumart` %in% c("Birke", "Erle", "Pappel") ~ "Sonstige Laubhölzer",
      
      # Edellaubhölzer (ELH)
      `Echte Baumart` %in% c("Ahorn", "Esche") ~ "Edellaubhölzer",
      
      # Platzhalter für alles andere
      TRUE ~ "Andere" 
    ))
}

# Mapping anwenden
df_sicher_bwi <- add_bwi_categories(df_sicher)
df_zuweisbar_bwi <- add_bwi_categories(df_zuweisbar)

# -------------------------------------------------------------------------
# 5. Validierung: Adjusted Rand Index (ARI)
# -------------------------------------------------------------------------

print("--- ARI ERGEBNISSE ---")

# Sicher
ari_kmeans_sicher <- ARI(df_sicher_bwi$BWI_Kategorie, df_sicher_bwi$`C-ID_kmeans`)
ari_mclust_sicher <- ARI(df_sicher_bwi$BWI_Kategorie, df_sicher_bwi$`C-ID_mclust`)

# Zuweisbar (Alle)
ari_kmeans_all <- ARI(df_zuweisbar_bwi$BWI_Kategorie, df_zuweisbar_bwi$`C-ID_kmeans`)
ari_mclust_all <- ARI(df_zuweisbar_bwi$BWI_Kategorie, df_zuweisbar_bwi$`C-ID_mclust`)

print(paste("ARI K-Means (nur sichere Bäume):", round(ari_kmeans_sicher, 3)))
print(paste("ARI Mclust  (nur sichere Bäume):", round(ari_mclust_sicher, 3)))
print("---------------------------------------------------")
print(paste("ARI K-Means (alle zuweisbaren): ", round(ari_kmeans_all, 3)))
print(paste("ARI Mclust  (alle zuweisbaren): ", round(ari_mclust_all, 3)))


# -------------------------------------------------------------------------
# 6. Validierung: Konfusionsmatrizen
# -------------------------------------------------------------------------

print("--- KONFUSIONSMATRIZEN (Absolute Zahlen) ---")

# Wir schauen uns exemplarisch die "sicheren" Bäume an
print("K-Means (Sichere Bäume):")
tab_kmeans <- table(Referenz = df_sicher_bwi$BWI_Kategorie, Cluster = df_sicher_bwi$`C-ID_kmeans`)
print(tab_kmeans)

print("Mclust (Sichere Bäume):")
tab_mclust <- table(Referenz = df_sicher_bwi$BWI_Kategorie, Cluster = df_sicher_bwi$`C-ID_mclust`)
print(tab_mclust)


# -------------------------------------------------------------------------
# 7. Validierung: Relative Verteilung (in %)
# -------------------------------------------------------------------------

print("--- RELATIVE VERTEILUNG (Zeilenprozente) ---")
# Zeigt an, wie viel Prozent einer Baumart in welchem Cluster gelandet sind

print("K-Means (%):")
print(round(prop.table(tab_kmeans, 1) * 100, 1))

print("Mclust (%):")
print(round(prop.table(tab_mclust, 1) * 100, 1))

print("--- VALIDIERUNG ABGESCHLOSSEN ---")
