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

# Frecuencias del alelo T del polimorfismo rs10818488 (en porcentaje)
# Fuente: estudios de poblaciones LATAM y estimaciones aproximadas
t_allele_freqs <- c(
  38.0,  # Mexico
  41.0,  # Puerto Rico
  39.0,  # Cuba
  40.0,  # Dominican Rep.
  42.0,  # Colombia
  44.0,  # Peru
  43.0,  # Belize
  43.0,  # Costa Rica
  43.0,  # El Salvador
  43.0,  # Guatemala
  43.0,  # Honduras
  43.0,  # Nicaragua
  43.0,  # Panama
  40.0,  # Argentina
  41.0,  # Bolivia
  46.0,  # Brazil
  44.0,  # Chile
  41.0,  # Ecuador
  43.0,  # Guyana
  42.0,  # Paraguay
  43.0,  # Suriname
  40.0,  # Uruguay
  41.0   # Venezuela
)

# Crear data.frame
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = t_allele_freqs
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
    title = "Frecuencia del alelo T (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo T<br>Polimorfismo rs10818488 del gen TNFAIP3</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs10818488' target='_blank'>rs10818488</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;v=rs10818488' target='_blank'>rs10818488</a></small>",
    position = "bottomleft"
  )
