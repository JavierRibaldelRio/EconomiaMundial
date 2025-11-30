library(tidyverse)
library(scales)

imports <- readRDS("./data/dataset_agrifood_imports.rds") |> 
  mutate(
    sector = fct_recode(sector,
                        "Cereales" = "Cereals",
                        "Vino" = "Wine",
                        "Aceite de oliva" = "Olive Oil",
                        "Azúcar" = "Sugar",
                        "Cítricos" = "Citrus Fruit",
                        "Semillas oleaginosas" = "Oilseeds"
    ),
    year = marketingYear,
    memberStateCode = memberStateCode,
    memberName = str_c(memberStateName, " (", memberStateCode, ")"),
    partnerCode = partnerCode,
    partnerName = str_c(partner, " (", partnerCode, ")"),
    euros = euroValue,
    peso = kg,
    .keep = "none"
  ) |> 
  group_by(sector, year) |> 
  summarise(
    total_kg = sum(peso),
    .groups = "drop"
  ) |> 
  group_by(sector) |> 
  summarise(
    mean_kg = mean(total_kg),
    .groups = "drop"
  )

exports <- readRDS("./data/dataset_agrifood_exports.rds") |> 
  mutate(
    sector = fct_recode(sector,
                        "Cereales" = "Cereals",
                        "Vino" = "Wine",
                        "Aceite de oliva" = "Olive Oil",
                        "Azúcar" = "Sugar",
                        "Cítricos" = "Citrus Fruit",
                        "Semillas oleaginosas" = "Oilseeds"
    ),
    year = marketingYear,
    memberStateCode = memberStateCode,
    memberName = str_c(memberStateName, " (", memberStateCode, ")"),
    partnerCode = partnerCode,
    partnerName = str_c(partner, " (", partnerCode, ")"),
    euros = euroValue,
    peso = kg,
    .keep = "none"
  ) |> 
  group_by(sector, year) |> 
  summarise(
    total_kg = sum(peso),
    .groups = "drop"
  ) |> 
  group_by(sector) |> 
  summarise(
    mean_kg = mean(total_kg),
    .groups = "drop"
  )

net_exports <- left_join(
  exports,
  imports,
  join_by(sector)
  ) |> 
  mutate(
    sector = sector,
    total_imports_neg = -mean_kg.y,
    total_exports = mean_kg.x,
    
    .keep = "none"
  )


# Plot --------------------------------------------------------------------

net_exports |> 
  
  pivot_longer(
    cols = c(total_exports, total_imports_neg),
    names_to = "flujo",
    values_to = "valor"
  ) |>
  mutate(flujo = case_when(
    flujo == "total_imports_neg" ~ "Importaciones",
    flujo == "total_exports" ~ "Exportaciones"
  )) |> 
  
  # Reorder ordena los productos según el valor de exportación para dar orden visual
  ggplot(aes(x = reorder(sector, abs(valor)), y = valor, fill = flujo)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_blank(data = limites_simetricos, aes(x = NULL, y = valor, fill = NULL)) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) {
      number(
        abs(x),           
        scale = 1e-6,     
        suffix = " M",    
        accuracy = 1      
      )
    },
    n.breaks = 8
  ) +
  scale_fill_manual(values = c(
    "Importaciones" = "#E41A1C",
    "Exportaciones" = "#003399"
  )) +
  theme_minimal() + # Tema limpio
  labs(
    title = "Volumen de Comercio Exterior Agrícola de la UE (2022-2024)",
    subtitle = "Comparativa de la media de Importaciones (Izquierda) frente a la media de Exportaciones (Derecha)",
    x = NULL, 
    y = "Quilogramos (kg)",
    caption = "Fuente: API Agri-food Data Portal (Taxud)"
  ) +
  theme(
    legend.position = "bottom",         
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.8)

