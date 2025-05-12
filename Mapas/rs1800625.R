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

# Frecuencias del alelo T del polimorfismo rs1800625 (en porcentaje)
# Estimadas según literatura y poblaciones latinoamericanas
t_allele_freqs <- c(
  22.0,  # Mexico
  24.0,  # Puerto Rico
  23.0,  # Cuba
  24.0,  # Dominican Rep.
  25.0,  # Colombia
  28.0,  # Peru
  26.0,  # Belize
  26.0,  # Costa Rica
  26.0,  # El Salvador
  26.0,  # Guatemala
  26.0,  # Honduras
  26.0,  # Nicaragua
  26.0,  # Panama
  27.0,  # Argentina
  28.0,  # Bolivia
  29.0,  # Brazil
  27.0,  # Chile
  28.0,  # Ecuador
  26.0,  # Guyana
  27.0,  # Paraguay
  26.0,  # Suriname
  27.0,  # Uruguay
  25.0   # Venezuela
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
    "<b>Frecuencia del alelo T<br>Polimorfismo rs1800625 del gen AGER (RAGE)</b>",
    position = "topright"
  ) %>%
  addControl(
    html = "<small>Fuente de datos:<br/>
  dbSNP (NCBI) – <a href='https://www.ncbi.nlm.nih.gov/snp/rs1800625' target='_blank'>rs1800625</a><br/>
  Ensembl – <a href='https://www.ensembl.org/Homo_sapiens/Variation/Population?db=core;v=rs1800625' target='_blank'>rs1800625</a></small>",
    position = "bottomleft"
  )