library(dplyr)
library(ggplot2)
library(tibble)
library(scales)

# Cálculo base
result <- tibble(
  tipo = c("Importaciones", "Exportaciones"),
  total_euro_value = c(
    sum(imports$euroValue, na.rm = TRUE),
    sum(exports$euroValue, na.rm = TRUE)
  )
)

# Calcular saldo
result_total |>
  ggplot(aes(x = total_euro_value, y = tipo, fill = tipo)) +
  geom_col() +
  geom_text(
    aes(label = paste0(
      scales::comma(total_euro_value, big.mark = ".", decimal.mark = ","), " €"
    )),
    hjust = 1.05,
    color = "white",
    size = 4
  ) +
  geom_vline(xintercept = saldo, linetype = "dashed", linewidth = 1) +

  # *** TEXTO EN LA FILA "Saldo balanza" ***
  annotate(
    "label",
    x = 24000000000,
    y = "Saldo balanza",
    label = "Déficit de la balanza comercial:\n−6.374.145.239 € ",
    size = 5,
    fontface = "bold",
    label.size = 0,
    fill = "white",
  ) +
  scale_fill_manual(values = colores) +
  labs(
    x = "",
    y = "",
    fill = "Tipo",
    title = "Balanza Comercial"
  ) +
  theme_bw()
