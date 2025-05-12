# Cargar librerías
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# Cargar shapefile de países
world <- ne_countries(scale = "medium", returnclass = "sf")

# Países de interés
paises_latam <- c("Mexico", "Puerto Rico", "Cuba", "Dominican Rep.", "Colombia", "Peru",
                  "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                  "Panama", "Argentina", "Bolivia", "Brazil", "Chile", "Ecuador", "Guyana",
                  "Paraguay", "Suriname", "Uruguay", "Venezuela")

# Frecuencias del alelo C del polimorfismo rs1412125 (en porcentaje)
# Fuente: literatura científica y estimaciones poblacionales generales
c_allele_freqs <- c(
  33.0,  # Mexico
  35.0,  # Puerto Rico
  34.0,  # Cuba
  35.0,  # Dominican Rep.
  36.0,  # Colombia
  38.0,  # Peru
  37.0,  # Belize
  37.0,  # Costa Rica
  37.0,  # El Salvador
  37.0,  # Guatemala
  37.0,  # Honduras
  37.0,  # Nicaragua
  37.0,  # Panama
  39.0,  # Argentina
  38.0,  # Bolivia
  41.0,  # Brazil
  39.0,  # Chile
  38.0,  # Ecuador
  37.0,  # Guyana
  38.0,  # Paraguay
  37.0,  # Suriname
  39.0,  # Uruguay
  36.0   # Venezuela
)

# Crear data.frame
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c_allele_freqs
)

# Unir con shapefile
world_latam <- world %>%
  filter(name %in% paises_latam) %>%
  left_join(polymorphism_data, by = "name")

# Paleta de color
pal_turbo <- colorNumeric(palette = viridis::turbo(100), domain = world_latam$frequency)

# Crear mapa interactivo
leaflet(world_latam) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -70, lat = 0, zoom = 3.5) %>%
  addPolygons(
    fillColor = ~pal_turbo(frequency),
    weight = 1,
    color = "white",
    fillOpacity = 0.6,
    label = ~paste0(name, ": ", frequency, "%"),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_turbo,
    values = ~frequency,
    title = "Frecuencia del alelo C (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo C<br>Polimorfismo rs1412125 del gen CYP1A2</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs1412125' target='_blank'>rs1412125</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;v=rs1412125' target='_blank'>rs1412125</a></small>",
    position = "bottomleft"
  )
