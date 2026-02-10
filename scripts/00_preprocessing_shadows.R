# -------------------------------------------------------------------------
# Masterarbeit: Unüberwachte Baumartenklassifizierung
# Skript 00: Preprocessing (Schattenmaskierung)
# Autor: Christian Salzmann (2026)
#
# Beschreibung:
# Dieses Skript entfernt Schattenbereiche aus dem Raster.
# Basierend auf einer vorherigen Klassifizierung (z.B. initiales K-Means),
# werden Pixel, die als Schatten identifiziert wurden (hier: Wert 1),
# auf NA (NoData) gesetzt.
# -------------------------------------------------------------------------

# 1. Bibliotheken laden
library(terra)

# --- KONFIGURATION & PFADE ---
# Eingabe: Das Raster, das die Schattenklassen enthält
# (z.B. ein initiales K-Means-Ergebnis, in dem Cluster 1 = Schatten ist)
input_raster_path <- "data/kmeans_monotemporal_cluster.tif"

# Ausgabe: Die Maske (oder das maskierte Raster)
output_raster_path <- "data/Maske_ohne_Schatten.tif"

# Ordner erstellen, falls nicht vorhanden
if(!dir.exists("data")) dir.create("data")

# -------------------------------------------------------------------------
# 2. Daten laden
# -------------------------------------------------------------------------

if(!file.exists(input_raster_path)) {
  stop("Fehler: Eingabedatei nicht gefunden. Bitte Pfad prüfen.")
}

print("Lade Raster für Schattenmaskierung...")
r <- rast(input_raster_path)

# Optional: Plotten des Originals
# plot(r, main = "Original (mit Schatten)")

# -------------------------------------------------------------------------
# 3. Schatten entfernen
# -------------------------------------------------------------------------

# Erstelle eine Kopie, um das Original nicht zu verändern
r_modified <- r

# Definiere den Pixelwert, der Schatten repräsentiert
# In diesem Fall: Wert 1 = Schatten
shadow_value <- 1

print(paste("Entferne Pixel mit Wert:", shadow_value))

# Setze alle Pixel mit diesem Wert auf NA (NoData)
r_modified[r_modified == shadow_value] <- NA

# -------------------------------------------------------------------------
# 4. Speichern und Visualisieren
# -------------------------------------------------------------------------

print("Speichere maskiertes Raster...")
writeRaster(r_modified, output_raster_path, overwrite=TRUE)

print(paste("Datei gespeichert unter:", output_raster_path))

# Visualisierung des Ergebnisses
plot(r_modified, main="Raster nach Schattenentfernung (NA)", colNA="red")
print("Hinweis: Rot dargestellte Bereiche sind nun NA (maskiert).")
