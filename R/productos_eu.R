library(eurostat)
library(tidyverse)

eu_countries <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)


df <- get_eurostat("apro_cpsh1", time_format = "num", unit = "T") |>
  filter(
    geo %in% eu_countries,
    TIME_PERIOD %in% c(2022, 2023, 2024),
    strucpro == "PR_HU_EU",
    crops %in% c(
      "C0000", "P0000", "R0000", "I0000", "V0000",
      "F0000", "T0000", "W1000", "S0000", "O1000", "U1000", "G0000"
    )
  ) |>
  complete(
    geo,
    crops,
    TIME_PERIOD,
    fill = list(value = NA)
  ) |>
  mutate(
    pais = geo,
    value = values,
    names = fct_recode(
      pais,
      "Alemania (DE)" = "DE",
      "Austria (AT)" = "AT",
      "Bélgica (BE)" = "BE",
      "Bulgaria (BG)" = "BG",
      "Chipre (CY)" = "CY",
      "Croacia (HR)" = "HR",
      "Dinamarca (DK)" = "DK",
      "Eslovaquia (SK)" = "SK",
      "Eslovenia (SI)" = "SI",
      "España (ES)" = "ES",
      "Estonia (EE)" = "EE",
      "Finlandia (FI)" = "FI",
      "Francia (FR)" = "FR",
      "Grecia (EL)" = "EL",
      "Hungría (HU)" = "HU",
      "Irlanda (IE)" = "IE",
      "Italia (IT)" = "IT",
      "Letonia (LV)" = "LV",
      "Lituania (LT)" = "LT",
      "Luxemburgo (LU)" = "LU",
      "Malta (MT)" = "MT",
      "Países Bajos (NL)" = "NL",
      "Polonia (PL)" = "PL",
      "Portugal (PT)" = "PT",
      "Rumanía (RO)" = "RO",
      "Suecia (SE)" = "SE",
      "Chequia (CZ)" = "CZ"
    ),
    crop = fct_recode(crops,
      "Cereales para la producción de grano" = "C0000",
      "Legumbres secas" = "P0000",
      "Cultivos de raíz" = "R0000",
      "Cultivos industriales" = "I0000",
      "Hortalizas frescas (incluidos los melones)" = "V0000",
      "Frutas, bayas y frutos secos" = "F0000",
      "Citricos" = "T0000",
      "Uva" = "W1000",
      "Fresas" = "S0000",
      "Olivas" = "O1000",
      "Setas cultivadas" = "U1000",
      "Forrajes de tierras arables" = "G0000"
    ),
    .keep = "none"
  ) |>
  group_by(pais, crop, names) |>
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

df |>
  ggplot(aes(y = mean_value, x = pais, fill = names)) +
  geom_col() +
  facet_wrap(~crop, nrow = 6, ncol = 2, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(ncol = 1, title = "Países")) +
  labs(
    x = NULL,
    y = "Miles de toneladas",
    title = "Panorama de la Producción Vegetal en la Unión Europea (2022  -2024)",
    caption = "Elaboración propia con R. Eurostat (apro_cpsh1, Crop production in EU standard humidity)."
  ) +
  theme_bw() +
  theme(
    panel.spacing.y = unit(1.5, "lines"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(size = 6)
  )
