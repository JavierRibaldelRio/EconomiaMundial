library(tidyverse)
library(networkD3)
library(htmltools)
library(jsonlite)

# --------------------------------------------------------
# 1. Cargar datos y filtrar por marketing year
# --------------------------------------------------------

imports_raw <- readRDS("./data/dataset_agrifood_imports.rds")

# Definir niveles válidos
niveles_validos <- c(
  "Cereales", "Vino", "Aceite de oliva",
  "Azúcar", "Cítricos", "Semillas oleaginosas"
)

# Limpiar, recodificar, y asegurar categorías
imports_clean <- imports_raw |> 
  filter(marketingYear == "2021/2022") |> 
  mutate(
    producto = fct_recode(sector,
      "Cereales" = "Cereals",
      "Vino" = "Wine",
      "Aceite de oliva" = "Olive Oil",
      "Azúcar" = "Sugar",
      "Cítricos" = "Citrus Fruit",
      "Semillas oleaginosas" = "Oilseeds"
    ),
    producto = factor(producto, levels = niveles_validos),
    origen = str_c(partner, " (", partnerCode, ")"),
    destino = "UE",
    euros = euroValue
  ) |> 
  filter(partnerCode != "EU") |> 
  drop_na(producto)  # ✅ Elimina productos no válidos

# --------------------------------------------------------
# 2. Top 10 países extracomunitarios por valor total
# --------------------------------------------------------

top_origen <- imports_clean |> 
  group_by(origen) |> 
  summarise(total = sum(euros, na.rm = TRUE)) |> 
  slice_max(total, n = 10) |> 
  pull(origen)

imports_top <- imports_clean |> 
  filter(origen %in% top_origen)

# --------------------------------------------------------
# 3. Agregar flujos por país-producto-UE (evita duplicados)
# --------------------------------------------------------

df <- imports_top |> 
  group_by(origen, destino, producto) |> 
  summarise(valor = sum(euros, na.rm = TRUE), .groups = "drop")

# --------------------------------------------------------
# 4. Crear nodos y enlaces (CORREGIDO)
# --------------------------------------------------------

# Crear nodos únicos - asegurar que todos los nombres sean distintos
nodos <- data.frame(
  name = unique(c(unique(df$origen), "UE")),  # Incluir "UE" explícitamente solo una vez
  stringsAsFactors = FALSE
)

df_links <- df |> 
  mutate(
    source = match(origen, nodos$name) - 1,
    target = match(destino, nodos$name) - 1,
    group = as.character(producto)
  ) |> 
  select(source, target, value = valor, group)

# Verificar que no hay valores NA en source/target
if(any(is.na(df_links$source)) | any(is.na(df_links$target))) {
  stop("Algunos nodos no encontrados en la lista de nodos")
}

# --------------------------------------------------------
# 5. Colores fijos por producto
# --------------------------------------------------------

colores_custom <- c(
  "Cereales" = "#E41A1C",            # rojo
  "Vino" = "#FFD92F",                # amarillo
  "Aceite de oliva" = "#4DAF4A",     # verde
  "Azúcar" = "#377EB8",              # azul
  "Cítricos" = "#FB9A99",            # rosa
  "Semillas oleaginosas" = "#984EA3" # morado
)

color_scale <- JS(sprintf("d3.scaleOrdinal().domain(%s).range(%s)",
                          toJSON(names(colores_custom)),
                          toJSON(unname(colores_custom))))

# --------------------------------------------------------
# 6. Crear el Sankey diagram
# --------------------------------------------------------

sankey <- sankeyNetwork(
  Links = df_links,
  Nodes = nodos,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  LinkGroup = "group",
  fontSize = 11,
  nodeWidth = 15,
  sinksRight = TRUE,
  width = 900,
  height = 500,
  colourScale = color_scale
)

# --------------------------------------------------------
# 7. Leyenda
# --------------------------------------------------------

leyenda <- tags$div(
  style = "margin-top: 10px; font-family: sans-serif;",
  tags$strong("Producto:"),
  tags$ul(
    lapply(names(colores_custom), function(grupo) {
      tags$li(style = "list-style: none; margin-bottom: 4px;",
              tags$span(style = paste0("display:inline-block;width:14px;height:14px;background-color:",
                                       colores_custom[grupo], ";margin-right:6px;border-radius:3px;"),
                        ""),
              grupo)
    })
  )
)

# --------------------------------------------------------
# 8. Mostrar resultado con leyenda
# --------------------------------------------------------

browsable(
  tagList(
    sankey,
    leyenda
  )
)