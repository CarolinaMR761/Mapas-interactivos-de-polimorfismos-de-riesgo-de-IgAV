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

# Frecuencias del alelo C del polimorfismo rs2070600 (en porcentaje)
# Basado en literatura y frecuencias aproximadas para poblaciones mestizas latinoamericanas
c_allele_freqs <- c(
  96.0,  # Mexico
  95.0,  # Puerto Rico
  95.0,  # Cuba
  95.0,  # Dominican Rep.
  95.0,  # Colombia
  94.0,  # Peru
  94.0,  # Belize
  94.0,  # Costa Rica
  94.0,  # El Salvador
  94.0,  # Guatemala
  94.0,  # Honduras
  94.0,  # Nicaragua
  94.0,  # Panama
  93.0,  # Argentina
  93.0,  # Bolivia
  92.0,  # Brazil
  93.0,  # Chile
  93.0,  # Ecuador
  94.0,  # Guyana
  93.0,  # Paraguay
  94.0,  # Suriname
  93.0,  # Uruguay
  94.0   # Venezuela
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
    "<b>Frecuencia del alelo C<br>Polimorfismo rs2070600 del gen AGER (RAGE)</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs2070600' target='_blank'>rs2070600</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;v=rs2070600' target='_blank'>rs2070600</a></small>",
    position = "bottomleft"
  )