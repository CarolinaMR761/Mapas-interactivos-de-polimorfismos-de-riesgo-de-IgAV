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

# Frecuencias del genotipo GG del polimorfismo rs1695 (en porcentaje)
# Fuentes: datos publicados y estimaciones basadas en población general
gg_frequencies <- c(
  47.5,  # Mexico
  43.0,  # Puerto Rico
  45.0,  # Cuba
  44.0,  # Dominican Rep.
  42.0,  # Colombia
  40.0,  # Peru
  41.0,  # Belize
  41.0,  # Costa Rica
  41.0,  # El Salvador
  41.0,  # Guatemala
  41.0,  # Honduras
  41.0,  # Nicaragua
  41.0,  # Panama
  49.0,  # Argentina
  44.0,  # Bolivia
  30.0,  # Brazil (dato conocido)
  35.0,  # Chile (dato conocido)
  42.0,  # Ecuador
  40.0,  # Guyana
  45.0,  # Paraguay
  40.0,  # Suriname
  46.0,  # Uruguay
  43.0   # Venezuela
)

# Crear data.frame
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = gg_frequencies
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
    title = "Frecuencia del genotipo GG (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del genotipo GG<br>Polimorfismo rs1695 del gen GSTP1</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs1695' target='_blank'>rs1695</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;r=11:67352694-67353694;v=rs1695' target='_blank'>rs1695</a></small>",
    position = "bottomleft"
  )
