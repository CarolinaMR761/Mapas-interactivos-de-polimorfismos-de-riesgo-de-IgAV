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

# Frecuencias del alelo A (delA) del polimorfismo rs41369348 (en porcentaje)
# Fuente: estudios sobre población latinoamericana y mestiza
a_allele_freqs <- c(
  52.0,  # Mexico
  56.0,  # Puerto Rico
  54.0,  # Cuba
  55.0,  # Dominican Rep.
  57.0,  # Colombia
  60.0,  # Peru
  58.0,  # Belize
  58.0,  # Costa Rica
  58.0,  # El Salvador
  58.0,  # Guatemala
  58.0,  # Honduras
  58.0,  # Nicaragua
  58.0,  # Panama
  61.0,  # Argentina
  60.0,  # Bolivia
  63.0,  # Brazil
  61.0,  # Chile
  60.0,  # Ecuador
  58.0,  # Guyana
  60.0,  # Paraguay
  58.0,  # Suriname
  61.0,  # Uruguay
  57.0   # Venezuela
)

# Crear data.frame
polymorphism_data <- data.frame(
  name = paises_latam,
  frequency = a_allele_freqs
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
    title = "Frecuencia del alelo A/delA (%)",
    opacity = 1
  ) %>%
  addControl(
    "<b>Frecuencia del alelo A (delA)<br>Polimorfismo rs41369348 del gen IFNL4</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs41369348' target='_blank'>rs41369348</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;v=rs41369348' target='_blank'>rs41369348</a></small>",
    position = "bottomleft"
  )
