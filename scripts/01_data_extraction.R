# -------------------------------------------------------------------------
# Masterarbeit: Unüberwachte Baumartenklassifizierung
# Skript 01: Extraktion der Spektralwerte (Zonale Statistik)
# Autor: Christian Salzmann (2026)
#
# WICHTIGER HINWEIS ZUR DATENGRUNDLAGE:
# Die Vektordaten der Baumkronen ('Baumkronen.gpkg') sind nicht Teil dieses
# Repositories. Sie stammen aus folgender Publikation:
#
# Quelle: Freudenberg, M., Magdon, P. & Nölke, N / Individual tree crown delineation in high-resolution remote sensing images based on U-Net
# DOI:    https://doi.org/10.1007/s00521-022-07640-4
# -------------------------------------------------------------------------

# 1. Load Libraries
library(sf)    # Vector data (Shapefiles/GeoPackage)
library(terra) # Raster data (Orthophotos)

# --- CONFIGURATION & PATHS ---
# NOTE: Please set your working directory to the project root.
# Raw data (TIF/GPKG) might not be included in the repo due to file size limits.

# Input files (stored in 'data' folder)
input_vector_path <- "data/Echte_BA.gpkg"
input_raster_path <- "data/multispektral_ortho_dsm_20052025_10cm_exthermo.tif"

# Output file
output_csv_path   <- "data/Echte_BA_mit_Spektralwerten.csv"

# Check if 'data' folder exists, create if not
if(!dir.exists("data")) dir.create("data")

# -------------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------------

# Check if files exist
if (!file.exists(input_vector_path) || !file.exists(input_raster_path)) {
  stop("Error: Input files not found in 'data/' folder. Please check paths.")
}

print("Loading vector data (Tree Crowns)...")
kronen <- st_read(input_vector_path, quiet = TRUE)

print("Loading raster data (Multispectral Image)...")
luftbild <- rast(input_raster_path)

# -------------------------------------------------------------------------
# 3. Pre-Processing
# -------------------------------------------------------------------------

# Set Band Names (Trinity Pro: RGB, Red-Edge, NIR)
# Ensure this order matches your specific sensor/orthophoto!
band_names <- c("rot", "gruen", "blau", "red_edge", "nir")

if (nlyr(luftbild) == length(band_names)) {
  names(luftbild) <- band_names
  print(paste("Band names set to:", paste(band_names, collapse=", ")))
} else {
  warning("Mismatch in number of bands! Keeping default names.")
}

# Check Coordinate Reference System (CRS)
if (st_crs(kronen) != crs(luftbild)) {
  print("CRS Mismatch detected. Transforming vector data to raster CRS...")
  kronen <- st_transform(kronen, crs(luftbild))
} else {
  print("CRS match confirmed.")
}

# -------------------------------------------------------------------------
# 4. Zonal Statistics (Extraction)
# -------------------------------------------------------------------------

print("Calculating zonal statistics (Mean values per crown)...")
# 'bind = TRUE' appends the results directly to the attribute table
kronen_spektral <- terra::extract(luftbild, kronen, fun = mean, na.rm = TRUE, bind = TRUE)

# -------------------------------------------------------------------------
# 5. Export
# -------------------------------------------------------------------------

print("Exporting data...")

# Drop geometry for statistical analysis (CSV output)
result_table <- st_drop_geometry(kronen_spektral)

# Save as CSV
write.csv(result_table, output_csv_path, row.names = FALSE)

print(paste("Success! Data saved to:", output_csv_path))
print("First rows of the dataset:")
print(head(result_table))
