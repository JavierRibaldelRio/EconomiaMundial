library(eurostat)
library(tidyverse)
library(sf)
library(giscoR)
library(stringi)


sf::sf_use_s2(FALSE) # evita cargar s2

eu_countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", 
  "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)

# 
df1 <- get_eurostat("aact_eaa01", time_format = "num", unit = "T") |>
  filter(
    geo %in% eu_countries, # Selección países EU
    TIME_PERIOD == 2023, # Año 2023
    unit == "MIO_EUR", # Unidades en Millones de €
    indic_ag == "PROD_BP", # VA_pb
    itm_newa == 10000 # Selección de la serie de cultivos
  ) |>
  mutate(
    pais = geo,
    value = values,
    .keep = "none"
  )

df2 <- get_eurostat("nama_10_gdp", time_format = "num", unit = "T") |> 
  filter(
    geo %in% eu_countries,
    TIME_PERIOD == 2023,
    unit == "CP_MEUR",
    na_item == "B1GQ"
  ) |> 
  mutate(
    pais = geo,
    gdp = values,
    .keep = "none"
  )

df <- df1 |> 
  left_join(df2, join_by(pais)) |> 
  mutate(
    pais = pais,
    agr_gdp = (value / gdp) * 100
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
  dplyr::summarise(
    geometry = sf::st_union(geometry), 
    .groups = "drop"
  ) |>
  
  # Unión con el DF original
  left_join(df, by = c("id" = "pais")) |>
  st_transform(3035) |> # Transformación a formato ggplot
  ggplot() +
  geom_sf(aes(fill = agr_gdp), color = "white", size = 0.2) +
  scale_fill_gradient(
    low  = "#cfe1ff", # tinte claro
    high = "#003399", # azul UE
    name = "Porcentaje del PIB", # <-- etiqueta de unidad
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title    = "Participación de la agricultura en el PIB de la UE-27",
    subtitle = "Valor Añadido Bruto respecto al total nacional (2023)",
    caption  = paste0("Elaboración propia con R. ",
      "Eurostat (nama_10_gdp, Gross domestic product (GDP) ",
      "and main components (output, expenditure and income))."
    )
  ) +
  theme(
    plot.title = element_text(size=11),
    plot.subtitle = element_text(size=9),
    plot.caption = element_text(hjust = 0.3, size = 8)
 )
  