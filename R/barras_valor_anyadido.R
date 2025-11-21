library(eurostat)
library(tidyverse)

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
    names = fct_recode(pais,
      "Francia" = "FR",
      "Italia" = "IT",
      "España" = "ES",
      "Alemania" = "DE",
      "Países Bajos" = "NL",
      "Polonia" = "PL",
      "Rumanía" = "RO",
      "Portugal" = "PT",
      "Hungría" = "HU",
      "Bélgica" = "BE"
    ),
    .keep = "none"
  )

df |>
  slice_max(order_by = value, n = 10) |>
  ggplot(aes(x = fct_reorder(names, value, .desc = TRUE), y = value)) +
  geom_col(width = 0.7, fill = "#003399") +
  ylim(0, 60000) +
  geom_text(aes(label = format(value, big.mark = ".", decimal.mark = ",", scientific = FALSE)), vjust = -0.4) +
  labs(
    x = NULL,
    y = "Millones de Euros",
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
ggsave("lio.png")
