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
  frequency = c(25.49, 28.38, 35.91, 31.45, 23.61, 17.91, 25.67, 25.67, 25.67, 25.67,
                25.67, 25.67, 25.67, 21.72, 21.72, 21.72, 21.72, 21.72, 21.72, 21.72,
                21.72, 21.72, 21.72),
  sd = c(1.57, 4.77, NA, NA, 2.67, 5.39, NA, NA, NA, NA, NA, NA, NA,
         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)  # Solo para México, Puerto Rico, Colombia y Perú
)
# Unir con el shapefile
world_latam <- world %>%
  filter(name %in% paises_latam) %>%
  left_join(polymorphism_data, by = "name")

# Crear paleta tipo 'turbo'
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
    title = "Frecuencia del alelo G (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (G)<br>Polimorfismo rs17576 del gen MMP9</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs17576' target='_blank'>rs17576</a><br/>
  Ensembl – <a href='https://grch37.ensembl.org/Homo_sapiens/Variation/Population?db=core;r=20:44639725-44640725;v=rs17576;vdb=variation;vf=1011379016' target='_blank'>rs17576</a><br/>",
    position = "bottomleft"
  )
