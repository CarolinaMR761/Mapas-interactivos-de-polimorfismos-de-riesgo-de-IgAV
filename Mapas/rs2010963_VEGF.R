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
paises_latam <- c("Colombia", "Mexico", "Peru", "Puerto Rico")

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(31.9, 33.6, 36.5, 38.9)
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
    title = "Frecuencia del alelo C (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (C)<br>Polimorfismo rs2010963 del gen VEGF</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  Ensembl – <a href= 'https://www.ensembl.org/Homo_sapiens/Variation/Population?r=6:43770113-43771113;v=rs2010963;vdb=variation;vf=406254814' target='_blank'>rs2010963</a></small>",
    position = "bottomleft"
  )
