library(eurostat)
library(tidyverse)
library(sf)
library(giscoR)



sf::sf_use_s2(FALSE) # evita cargar s2

eu_countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)

# TODO revisar
# Descargar datos (solo una vez) y selección de países de la UE, año 2023,
# y cultivos deseados (todos los sumables :))
df <- get_eurostat("apro_cpsh1", time_format = "num", unit = "T") |>
  filter(
    geo %in% eu_countries,
    TIME_PERIOD == 2024,
    crops %in% c(
      "C0000", "P0000", "R0000",
      "I0000", "V0000", "F0000", "H0000"
    )
  ) |>
  mutate(values = values, pais = geo, time = TIME_PERIOD, .keep = "none") |>
  group_by(pais) |>
  summarise(total_prod = sum(values))


# Obtiene el mapa
map_eu <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "60",
  nuts_level = 0,
  year = 2021
) |>
  filter(id %in% eu_countries)



map_eu_ll <- sf::st_transform(map_eu, 4326)

# 2) Explota multipolígonos a polígonos
map_eu_parts <- map_eu_ll |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON")

# 3) Calcula un punto representativo por polígono y saca lon/lat
centers <- sf::st_point_on_surface(map_eu_parts$geom) # o map_eu_parts$geometry
coords <- sf::st_coordinates(centers)
map_eu_parts$lon <- coords[, 1]
map_eu_parts$lat <- coords[, 2]

# 4) Filtro “Europa” (ajusta si quieres incluir/excluir Azores/Canarias)
#    -25 < lon < 45 mantiene Canarias (~ -18), pero elimina Guayana Francesa (~ -53)
#    Si quieres conservar Azores (~ -31), baja el umbral a -35.
map_eu_clean <- map_eu_parts |>
  dplyr::filter(lon > -25, lon < 45, lat > 34, lat < 72) |>
  dplyr::group_by(id) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")


# Une el mapa con los datos
map_df <- map_eu_clean |>
  left_join(df, by = c("id" = "pais"))


map_df_3035 <- st_transform(map_df, 3035)


ggplot(map_df_3035) +
  geom_sf(aes(fill = total_prod), color = "white", size = 0.2) +
  scale_fill_gradient(
    low  = "#cfe1ff", # tinte claro
    high = "#003399", # azul UE
    name = "Miles de Toneladas" # <-- etiqueta de unidad
  ) +
  labs(
    title    = "Producción agrícola vegetal",
    subtitle = paste0("UE-27 (", 2023, "). Tabla Eurostat: apro_cpsh1"),
    caption  = "Fuente: Eurostat (tabla apro_cpsh1). Elaboración propia. Formato ChatGPT"
  )
