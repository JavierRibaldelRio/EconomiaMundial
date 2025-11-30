#############################################
#############################################
######## NO EJECUTAR ESTE ARCHIVO ###########
#############################################
#############################################

library(jsonlite)
library(dplyr)
library(tools)

options(timeout = 600)

# Ciclos fiscales deseados
ciclos_fiscales <- c("2021/2022", "2022/2023", "2023/2024")

# Sectores (formato URL correcto)
sectores <- c("Oilseeds", "Cereals", "Olive%20Oil", "Wine", "Sugar", "Citrus%20Fruit")

#############################################
# IMPORTS
#############################################

# Inicializar vectores
nombres_archivos_import <- c()
nombres_url_import <- c()

# Construir URLs y nombres de archivo
for (sector in sectores) {
  for (ciclo in ciclos_fiscales) {
    nombre_archivo <- paste0(
      "import_",
      gsub("%20", "_", tolower(sector)),
      "_",
      gsub("/", "-", ciclo),
      ".json"
    )

    url <- paste0(
      "https://ec.europa.eu/agrifood/api/taxud/weeklyData/import?importCategories=&memberStateCodes=&partnerCodes=&products=&sectors=",
      sector,
      "&marketingYears=",
      ciclo
    )

    nombres_archivos_import <- c(nombres_archivos_import, nombre_archivo)
    nombres_url_import <- c(nombres_url_import, url)
  }
}

# Descargar archivos
for (i in seq_along(nombres_url_import)) {
  cat("Descargando archivo", i, ":", nombres_archivos_import[i], "...\n")
  try({
    download.file(
      url = nombres_url_import[i],
      destfile = nombres_archivos_import[i],
      mode = "wb",
      quiet = TRUE
    )
  })
}

# Leer imports
dataset_imports <- lapply(nombres_archivos_import, function(archivo) {
  datos <- fromJSON(archivo, flatten = TRUE)
  datos$origen <- archivo

  partes <- strsplit(archivo, "_")[[1]]
  producto_raw <- partes[2]
  ciclo_raw <- gsub(".json", "", partes[3])

  producto <- toTitleCase(gsub("_", " ", producto_raw))
  ciclo <- gsub("-", "/", ciclo_raw)

  hora <- format(Sys.time(), "%H:%M:%S")

  cat(sprintf(
    "[%s] Read %s %s %d imports,\n",
    hora, producto, ciclo, nrow(datos)
  ))

  return(datos)
}) |> bind_rows()

cat("Dimensiones finales (imports):", nrow(dataset_imports), "filas y", ncol(dataset_imports), "columnas.\n")

dir.create("data", showWarnings = FALSE)
saveRDS(dataset_imports, "data/dataset_agrifood_imports.rds")


#############################################
# EXPORTS (idéntico a imports)
#############################################

nombres_archivos_export <- c()
nombres_url_export <- c()

# Construir URLs y nombres de archivo para EXPORTS
for (sector in sectores) {
  for (ciclo in ciclos_fiscales) {
    nombre_archivo <- paste0(
      "export_",
      gsub("%20", "_", tolower(sector)),
      "_",
      gsub("/", "-", ciclo),
      ".json"
    )

    url <- paste0(
      "https://ec.europa.eu/agrifood/api/taxud/weeklyData/export?memberStateCodes=&partnerCodes=&products=&sectors=",
      sector,
      "&marketingYears=",
      ciclo
    )

    nombres_archivos_export <- c(nombres_archivos_export, nombre_archivo)
    nombres_url_export <- c(nombres_url_export, url)
  }
}

# Descargar archivos export
for (i in seq_along(nombres_url_export)) {
  cat("Descargando archivo", i, ":", nombres_archivos_export[i], "...\n")
  try({
    download.file(
      url = nombres_url_export[i],
      destfile = nombres_archivos_export[i],
      mode = "wb",
      quiet = TRUE
    )
  })
}

# Leer exports
dataset_exports <- lapply(nombres_archivos_export, function(archivo) {
  datos <- fromJSON(archivo, flatten = TRUE)
  datos$origen <- archivo

  partes <- strsplit(archivo, "_")[[1]]
  producto_raw <- partes[2]
  ciclo_raw <- gsub(".json", "", partes[3])

  producto <- toTitleCase(gsub("_", " ", producto_raw))
  ciclo <- gsub("-", "/", ciclo_raw)

  hora <- format(Sys.time(), "%H:%M:%S")

  cat(sprintf(
    "[%s] Read %s %s %d exports,\n",
    hora, producto, ciclo, nrow(datos)
  ))

  return(datos)
}) |> bind_rows()

cat("Dimensiones finales (exports):", nrow(dataset_exports), "filas y", ncol(dataset_exports), "columnas.\n")

saveRDS(dataset_exports, "data/dataset_agrifood_exports.rds")
