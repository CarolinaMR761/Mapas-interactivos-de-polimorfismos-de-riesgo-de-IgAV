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
                  "Argentina", "Brazil", "Chile", "Ecuador",
                  "Venezuela") 

# Dataset de frecuencias
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = c(3.105, 0.75, 1.4, 0.95, 1.57, 1.328, 2.293, 1.9, 2.495, 3.457),
  sd = c(1.878, 0.070, 0.282, 0.353, NA,
         0.819, 2.185, NA, 1.366, 1.273)  # Todo aquel país que tenga más de una región con ese alelo específico y con la frecuencia alélica reportada
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
    title = "Frecuencia del alelo B*52 (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo de riesgo (B*52)<br>Polimorfismo HLA-B*52 del gen HLA-B</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
 The Allele Frequency Net Data – <a href='http://www.allelefrequencies.net/hla6006a.asp?hla_locus_type=Classical&hla_locus=B&hla_allele1=B*52&hla_allele2=B*52&hla_selection=&hla_pop_selection=&hla_population=&hla_country=Venezuela&hla_dataset=&hla_region=South+and+Central+America&hla_ethnic=&hla_study=&hla_order=order_1&hla_sample_size_pattern=equal&hla_sample_size=&hla_sample_year_pattern=equal&hla_sample_year=&hla_level_pattern=equal&hla_level=&standard=a&hla_show=' target='_blank'>HLA-B*52</a></small>",
    position = "bottomleft"
  )
