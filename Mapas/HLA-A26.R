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
paises_latam <- c("Mexico", "Cuba", "Colombia", "Peru", "Guatemala",
                  "Argentina", "Bolivia", "Brazil", "Chile", "Ecuador",
                  "Venezuela")

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(2.278, 3.3, 2.725, 1.0, 0.745,
                2.425, 2.9, 2.096, 2.66, 1.302, 4.21),
  sd = c(1.627, 0.264, 1.237, NA, 0.063, 1.880, NA, 2.134, NA, 0.870,
         3.896)  # Todo aquel país que tenga más de una región con ese alelo específico y con la frecuencia alélica reportada
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
    title = "Frecuencia del alelo A*26 (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (A*26)<br>Polimorfismo HLA-A*26 del gen HLA-A</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  The Allele Frequency Net Data – <a href='http://www.allelefrequencies.net/hla6006a.asp?hla_locus_type=Classical&hla_locus=A&hla_allele1=A*26&hla_allele2=A*26&hla_selection=&hla_pop_selection=&hla_population=&hla_country=Venezuela&hla_dataset=&hla_region=South+and+Central+America&hla_ethnic=&hla_study=&hla_order=order_1&hla_sample_size_pattern=equal&hla_sample_size=&hla_sample_year_pattern=equal&hla_sample_year=&hla_level_pattern=equal&hla_level=&standard=a&hla_show=' target='_blank'>HLA-A*26</a></small>",
    position = "bottomleft"
  )
