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
  frequency = c(41.384, 51.67, 55.65, 50, 44.7, 30.6, 38.94, 38.94, 38.94, 38.94,
                38.94, 38.94, 38.94, 38.9, 38.9, 38.9, 38.9, 38.9, 38.9, 38.9,
                38.9, 38.9, 38.9),
  sd = c(2.239, 1.033, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Solo para México y Puerto Rico
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
    label = ~ifelse(
      !is.na(sd),
      paste0(name, ": ", frequency, "% ± ", sd, "%"),
      paste0(name, ": ", frequency, "%")
    ),
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
    "<b>Frecuencia del alelo de riesgo (T)<br>Polimorfismo rs4359 del gen ACE</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs4359'>rs4359</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;r=17:63494482-63495482;v=rs4359;vdb=variation;vf=959537474' target='_blank'>rs4359</a><br/>",
    position = "bottomleft"
  )
