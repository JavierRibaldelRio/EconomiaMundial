library(eurostat)
library(tidyverse)
library(sf)
library(giscoR)
library(stringi)
library(here)


sf::sf_use_s2(FALSE) # evita cargar s2

eu_countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)


df <- get_eurostat("aact_eaa01", time_format = "num", unit = "T") |>
  filter(
    geo %in% eu_countries, # Selección países EU
    TIME_PERIOD == 2023, # Año 2023
    unit == "MIO_EUR", # Unidades en Millones de €
    indic_ag == "PROD_BP", # VA_pb
    itm_newa == 10000 # Selección de la serie de cultivos (no confundir con agricultural ya que este incluye la ganadería)
  ) |>
  mutate(
    pais = geo,
    value = values,
    .keep = "none"
  )


# Crea la base del mapa de la EU
map_eu <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = 0,
  year = 2021
) |>
  filter(id %in% eu_countries) |>
  sf::st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON")



# Suprime los territorios de ultramar para visualizar mejor

coords <- sf::st_coordinates(sf::st_point_on_surface(map_eu$geom))
map_eu$lon <- coords[, 1]
map_eu$lat <- coords[, 2]

map_eu |>
  dplyr::filter(lon > -25, lon < 45, lat > 34, lat < 72) |>
  dplyr::group_by(id) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  # Unión con el DF original
  
  left_join(df, by = c("id" = "pais")) |>
  st_transform(3035) |> # Transformación a formato ggplot
  ggplot() +
  geom_sf(aes(fill = value), color = "white", size = 0.2) +
  scale_fill_gradient(
    low  = "#cfe1ff", # tinte claro
    high = "#003399", # azul UE
    name = "Millones de Euros", # <-- etiqueta de unidad
    label = scales::label_number(big.mark = ".", decimal.mark = ",")
    
  ) +
  labs(
    title    = "Producción agrícola vegetal en la UE-27",
    subtitle = "Valores Añadidos a precios básicos corrientes (millones de euros, 2023)",
    caption  = "Elaboración propia con R.  Eurostat (aact_eaa01, P11 – Output of crop production)."
  ) +
  theme(
    plot.title = element_text(size=11),
    plot.subtitle = element_text(size=9),
    plot.caption = element_text(hjust = 0.3, size = 8)
  )

ggsave(here("R", "img/valor_anyadido.png"))