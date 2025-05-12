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
                  "Chile", "Brazil","Belize", "Guatemala", "Panama", "Costa Rica", "Honduras", 
                  "El Salvador", "Nicaragua", "Argentina", "Bolivia", "Ecuador", "Guyana", "Paraguay", "Suriam",
                  "Uruguay", "Venezuela")

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(12.78, 23.40, 20.07, 17.16, 17.00, 6.47, 9.70, 5.00, 12.37, 12.37, 12.37, 12.37, 12.37, 12.37, 12.37, 15.09, 15.09, 15.09, 15.09, 15.09, 15.09, 15.09, 15.09)
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
    title = "Frecuencia del alelo A (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (A)<br>Polimorfismo rs2297518 del gen iNOS</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs2297518#frequency_tab' target='_blank'>rs2297518</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;r=17:27769071-27770071;v=rs2297518;vdb=variation;vf=959583172#population_freq_AFR' target='_blank'>rs2297518</a><br/>
  gnomAD – <a href='https://gnomad.broadinstitute.org/variant/17-27769571-G-A?dataset=gnomad_r4' target='_blank'>rs2297518</a></small>",
    position = "bottomleft"
  )
