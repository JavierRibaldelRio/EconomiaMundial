library(eurostat)
library(tidyverse)
library(sf)
library(giscoR)
library(stringi)


sf::sf_use_s2(FALSE) # evita cargar s2

eu_countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
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

df |> 
    ggplot(aes(x = pais, y = value)) + 
        geom_bar()

ggsave("barras.png")

print(df)