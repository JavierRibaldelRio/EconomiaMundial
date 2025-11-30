library(tidyverse)
library(scales)
library(here)

raw_imports <- readRDS(here("R", "./data/dataset_agrifood_imports.rds"))
raw_exports <- readRDS(here("R", "./data/dataset_agrifood_exports.rds"))

# Tidy data ---------------------------------------------------------------

imports_by_sector <- raw_imports |> 
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
  )

total_imports_by_sector <- imports_by_sector |> 
  group_by(sector, memberStateCode, year) |> 
  summarise(
    total_kg = sum(peso),
    .groups = "drop"
  ) |> 
  group_by(sector, memberStateCode) |> 
  summarise(
    mean_kg = mean(total_kg),
    .groups = "drop"
  )

exports_by_sector <- raw_exports |> 
  mutate(
    sector = fct_recode(sector,
                        "Cereales" = "Cereals",
                        "Vino" = "Wine",
                        "Aceite de oliva" = "Olive Oil",
                        "Azúcar" = "Sugar",
                        "Cítricos" = "Citrus Fruit",
                        "Semillas oleaginosas" = "Oilseeds"
                        ),                      
    memberStateCode = memberStateCode,
    memberName = str_c(memberStateName, " (", memberStateCode, ")"),
    partnerCode = partnerCode,
    partnerName = str_c(partner, " (", partnerCode, ")"),
    euros = euroValue,
    peso = kg,
    year = marketingYear,
    .keep = "none"
  )

total_exports_by_sector <- exports_by_sector |> 
  group_by(sector, memberStateCode, year) |> 
  summarise(
    total_kg = sum(peso),
    .groups = "drop"
  ) |> 
  group_by(sector, memberStateCode) |> 
  summarise(
    mean_kg = mean(total_kg),
    .groups = "drop"
  )

datos_comercio <- left_join(
  total_exports_by_sector, 
  total_imports_by_sector, 
  join_by(sector, memberStateCode)
  ) |> 
  mutate(
    sector = sector,
    memberStateCode = memberStateCode,
    total_imports_neg = -mean_kg.y,
    total_exports = mean_kg.x,
    
    .keep = "none"
  )


# Simetry dataset ---------------------------------------------------------

limites_simetricos <- datos_comercio |> 
  group_by(sector) |> 
  summarise(
    max_export = max(total_exports, na.rm = TRUE),
    max_import = max(abs(total_imports_neg), na.rm = TRUE)
  ) |> 
  # Comparar las dos y quedarse con la mayor. 
  # Multiplicamos por 1.1 para dar un 10% de aire extra a los lados.
  mutate(limite_maximo = pmax(max_export, max_import) * 1.1) |> 
  
  # Esto "cruza" los datos para tener dos filas por sector
  cross_join(tibble(signo = c(1, -1))) |> 
  
  # Calculamos el valor final: un positivo y un negativo IGUALES
  mutate(valor = limite_maximo * signo) |> 
  
  # Añadimos columnas "falsas" de relleno para que ggplot no se queje
  mutate(
    memberName = NA,
    flujo = NA       
  )

# Plot --------------------------------------------------------------------

datos_comercio |> 
  
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
  ggplot(aes(x = reorder(memberStateCode, abs(valor)), y = valor, fill = flujo)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_blank(data = limites_simetricos, aes(x = NULL, y = valor, fill = NULL)) +
  facet_wrap(~sector, ncol = 2, scales = "free") +
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
    subtitle = "Comparativa de la media de Exportaciones (Derecha) vs media de Importaciones (Izquierda)",
    x = NULL, 
    y = "Quilogramos (kg)",
    fill = "Tipo de Flujo",
    caption = "Fuente: API Agri-food Data Portal (Taxud)"
  ) +
  theme(
    legend.position = "bottom",         
    plot.title = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 7, face = "bold"),
    axis.text.x = element_text(size = 7),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.8)
