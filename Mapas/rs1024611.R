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

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(49.82, 32.10, 29.19, 27.04, 41.5, 66.5, 47.18, 47.18, 47.18, 47.18,
                47.18, 47.18, 47.18, 49.14, 49.14, 49.14, 49.14, 49.14, 49.14, 49.14,
                49.14, 49.14, 49.14)
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
    "<b>Frecuencia del alelo de riesgo (G)<br>Polimorfismo rs1024611 del gen MCP1/CCL2</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs1024611#frequency_tab' target='_blank'>rs1024611</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;r=17:34252269-34253269;v=rs1024611;vdb=variation;vf=959559590' target='_blank'>rs1024611</a><br/>
  gnomAD – <a href='https://gnomad.broadinstitute.org/variant/17-34252769-A-G?dataset=gnomad_r4' target='_blank'>rs1024611</a></small>",
    position = "bottomleft"
  )
