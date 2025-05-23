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
paises_latam <- c("Mexico", "Puerto Rico", "Colombia", "Peru")

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(8.6, 8.7, 7.4, 6.5)
)

# Unir con el shapefile
world_latam <- world %>%
  filter(name %in% paises_latam) %>%
  left_join(polymorphism_data, by = "name")

# Crear paleta tipo 'turbo' como en tu mapa estático
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
    title = "Frecuencia del alelo G (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (G)<br>Polimorfismo rs2236416 del gen MMP9</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  Ensembl – <a href='https://grch37.ensembl.org/Homo_sapiens/Variation/Population?db=core;r=20:44640075-44641075;v=rs2236416;vdb=variation;vf=1011414565' target='_blank'>rs2236416</a><br/>",
    position = "bottomleft"
  )
