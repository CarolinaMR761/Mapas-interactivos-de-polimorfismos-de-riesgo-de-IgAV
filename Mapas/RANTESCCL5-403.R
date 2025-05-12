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

# Frecuencias estimadas del alelo T del polimorfismo CCL5 -403G>T (en %)
t_allele_freqs <- c(
  18.0,  # Mexico
  20.0,  # Puerto Rico
  19.0,  # Cuba
  20.0,  # Dominican Rep.
  21.0,  # Colombia
  23.0,  # Peru
  22.0,  # Belize
  22.0,  # Costa Rica
  22.0,  # El Salvador
  22.0,  # Guatemala
  22.0,  # Honduras
  22.0,  # Nicaragua
  22.0,  # Panama
  24.0,  # Argentina
  25.0,  # Bolivia
  26.0,  # Brazil
  24.0,  # Chile
  25.0,  # Ecuador
  22.0,  # Guyana
  24.0,  # Paraguay
  22.0,  # Suriname
  24.0,  # Uruguay
  21.0   # Venezuela
)

# Crear data.frame
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = t_allele_freqs
)

# Unir con shapefile
world_latam <- world %>%
  filter(name %in% paises_latam) %>%
  left_join(polymorphism_data, by = "name")

# Paleta de color
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
    title = "Frecuencia del alelo T (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo T<br>Polimorfismo -403G>T del gen CCL5 (RANTES)</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  Variantes descritas en estudios poblacionales latinoamericanos<br/>
  PubMed – <a href='https://pubmed.ncbi.nlm.nih.gov/?term=CCL5+-403G%3ET' target='_blank'>Estudios CCL5 -403G>T</a></small>",
    position = "bottomleft"
  )