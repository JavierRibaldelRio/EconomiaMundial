#############################################
#############################################
######## NO EJECUTAR ESTE ARCHIVO ###########
#############################################
#############################################

library(jsonlite)
library(dplyr)

options(timeout = 600)

# Imports -----------------------------------------------------------------

nombres_archivos_import <- c(
  "import_oilseeds.json",
  "import_cereals.json",
  "import_olive_oil.json",
  "import_wine.json",
  "import_sugar.json",
  "import_citrus_fruit.json"
)

nombres_url_import <- c(
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=Oilseeds&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=Cereals&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=Olive%20Oil&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=Wine&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=Sugar&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=Citrus%20Fruit&marketingYears=2023/2024"
)


for(i in 1:length(nombres_url)) {
  cat("Descargando archivo", i, ":", nombres_archivos[i], "...\n")
  try({
    download.file(url = nombres_url[i], 
                  destfile = nombres_archivos[i], 
                  mode = "wb", 
                  quiet = TRUE) # quiet=TRUE reduce el ruido en la consola
  })
}

# Exports -----------------------------------------------------------------

nombres_archivos_export <- c(
  "export_oilseeds.json",
  "export_cereals.json",
  "export_olive_oil.json",
  "export_wine.json",
  "export_sugar.json",
  "export_citrus_fruit.json"
)

nombres_url_export <- c(
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=Oilseeds&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=Cereals&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=Olive%20Oil&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=Wine&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=Sugar&marketingYears=2023/2024",
  "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=Citrus%20Fruit&marketingYears=2023/2024"
)

for(i in 1:length(nombres_url_export)) {
  cat("Descargando archivo", i, ":", nombres_archivos_export[i], "...\n")
  try({
    download.file(url = nombres_url_export[i], 
                  destfile = nombres_archivos_export[i], 
                  mode = "wb", 
                  quiet = TRUE) # quiet=TRUE reduce el ruido en la consola
  })
}


# Compact the data --------------------------------------------------------

## Imports ----------------------------------------------------------------



dataset_imports <- lapply(nombres_archivos_import, function(archivo) {
  datos <- fromJSON(archivo, flatten = TRUE)
  
  # Añadimos columna de origen
  datos$origen <- archivo 
  
  return(datos)
}) |> 
  bind_rows()

cat("Dimensiones finales:", nrow(dataset_imports), "filas y", ncol(dataset_imports), "columnas.\n")

# CSV 
write.csv(dataset_imports, "dataset_agrifood_imports.csv", row.names = FALSE)

# Formato R
saveRDS(dataset_imports, "dataset_agrifood_imports.rds")


## Exports ----------------------------------------------------------------

dataset_exports <- lapply(nombres_archivos_export, function(archivo) {
  datos <- fromJSON(archivo, flatten = TRUE)
  
  # Añadimos columna de origen
  datos$origen <- archivo 
  
  return(datos)
}) |> 
  bind_rows()

cat("Dimensiones finales:", nrow(dataset_exports), "filas y", ncol(dataset_exports), "columnas.\n")

# CSV 
write.csv(dataset_exports, "dataset_agrifood_exports.csv", row.names = FALSE)

# Formato R
saveRDS(dataset_exports, "dataset_agrifood_exports.rds")
