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
paises_latam <- c("Argentina", "Brazil", "Chile", "Colombia", "Cuba", "Nicaragua", "Costa Rica", "Guatemala", "Panama", "Ecuador", "Mexico", "Peru", "Venezuela", "Paraguay", "Bolivia")
                

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(1.35, 2.24, 3.86, 7.99, 2.25, 2.5, 8.04, 4.33, 3.31, 1.89, 4.26, 3.8, 11.4, 2.94, 4.76)
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
    title = "Frecuencia del alelo B*15 (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (B*15)<br> HLA-B*15(*1501) </b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  Allele Frequency Net Database – <a href='https://www.allelefrequencies.net/hla6006a.asp?hla_locus_type=Classical&hla_locus=&hla_allele1=B*15%3A01&hla_allele2=B*15%3A01&hla_selection=&hla_pop_selection=&hla_population=&hla_country=&hla_dataset=&hla_region=South+and+Central+America&hla_ethnic=&hla_study=&hla_order=order_1&hla_sample_size_pattern=equal&hla_sample_size=&hla_sample_year_pattern=equal&hla_sample_year=&hla_level_pattern=equal&hla_level=&standard=a&hla_show='>HLA-B*15(*1501)</a><br/>
  </small>",
    position = "bottomleft"
  )
