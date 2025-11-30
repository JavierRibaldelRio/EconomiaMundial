library(eurostat)
library(tidyverse)
library(scales)

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

df |>
  slice_max(order_by = value, n = 10) |>
  ggplot(aes(x = fct_reorder(pais, value, .desc = TRUE), y = value)) +
  geom_col(width = 0.7, fill = "#003399") +
  geom_text(aes(label = number(value, scale = 0.001, accuracy = 1)), vjust = -0.4) +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    x = NULL,
    y = "Miles de millones de €",
    title = "Los 10 países de la UE-27 con mayor producción agrícola vegetal",
    subtitle = "Valores Añadidos a precios básicos corrientes (millones de euros, 2023)",
    caption = "Elaboración propia con R.  Eurostat (aact_eaa01, P11 – Output of crop production)."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(hjust = 0.3, size = 8)
  )
