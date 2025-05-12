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
paises_latam <- c("Brazil", "Chile", "Colombia", "Cuba", "Nicaragua", "Costa Rica", "Panama", "Mexico")
                

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(3.03, 0.77, 1.20, 1.90, 0.95, 1.10, 1.15, 0.67)
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
    title = "Frecuencia del alelo B*35 (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (B*35)<br> HLA-B*35(*3503) </b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  Allele Frequency Net Database – <a href='https://www.allelefrequencies.net/hla6006a.asp?hla_locus_type=Classical&hla_locus=&hla_allele1=B*35%3A03&hla_allele2=B*35%3A03&hla_selection=&hla_pop_selection=&hla_population=&hla_country=&hla_dataset=&hla_region=South+and+Central+America&hla_ethnic=&hla_study=&hla_order=order_1&hla_sample_size_pattern=equal&hla_sample_size=&hla_sample_year_pattern=equal&hla_sample_year=&hla_level_pattern=equal&hla_level=&standard=a&hla_show='>HLA-B*35(*3503)</a><br/>
  </small>",
    position = "bottomleft"
  )
