library(dplyr)
library(ggplot2)
library(tibble)
library(scales)
library(here)


imports <- readRDS(here("R", "data/dataset_agrifood_imports.rds")) |>
  filter(marketingYear == "2021/2022")

exports <- readRDS(here("R", "data/dataset_agrifood_exports.rds")) |>
  filter(marketingYear == "2021/2022")

# Cálculo base
result <- tibble(
  tipo = c("Importaciones", "Exportaciones"),
  total_euro_value = c(
    sum(imports$euroValue, na.rm = TRUE),
    sum(exports$euroValue, na.rm = TRUE)
  )
)

# Calcular saldo
saldo <- result$total_euro_value[result$tipo == "Exportaciones"] -
         result$total_euro_value[result$tipo == "Importaciones"]

# Tibble adicional con el saldo
saldo_tbl <- tibble(
  tipo = "Saldo balanza",
  total_euro_value = saldo
)

# Unir todo
result_total <- bind_rows(result, saldo_tbl)

# Colores (exportaciones azul, importaciones rojo, saldo verde/rojo según signo)
colores <- c(
  "Importaciones" = "#E41A1C",
  "Exportaciones" = "#003399",
  "Saldo balanza" = ifelse(saldo >= 0, "#228B22", "#8B0000")
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
    title = "Balanza Comercial",
    caption = "Elaboración propia con R. Fuente: API Agri-food Data Portal (Taxud)"
  ) +
  theme_bw() + 
  theme(
    panel.spacing.y = unit(1.5, "lines"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(face = "bold", size = 8)
  )

ggsave(here("R", "img/balanza_comercial.png"))