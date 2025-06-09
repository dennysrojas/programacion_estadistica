#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("maps")


#Paquetes necesarios
library(tidyverse)
library(skimr)
library(maps)

# 0. Cargar los datos
# Función para cargar los datos desde un archivo CSV
cargar_datos <- function(ruta_archivo) {
  if (!file.exists(ruta_archivo)) {
    stop(paste("El archivo no se encontró en la ruta:", ruta_archivo))
  }
  meteoritos_df <- read_csv(ruta_archivo)
  message(paste("Datos cargados desde:", ruta_archivo))
  return(meteoritos_df)
}

# Función para renombrar columnas
renombrar_columnas <- function(df, nuevos_nombres) {
  if (length(nuevos_nombres) != ncol(df)) {
    stop("El número de nuevos nombres no coincide con el número de columnas.")
  }
  colnames(df) <- nuevos_nombres
  message("Nombres de columnas cambiados.")
  return(df)
}

# 1. Vistazo Inicial y Limpieza Preliminar
# Función para realizar un vistazo inicial y seleccionar columnas
vistazo_y_seleccion_inicial <- function(df) {
  message("\n--- Vistazo Inicial (glimpse) ---")
  glimpse(df)
  
  columnas_a_mantener <- c("nombre", "clase_meteorito", "masa_g", "evento", "anio", "latitud", "longitud")
  meteoritos_proc <- df %>%
    select(all_of(columnas_a_mantener)) # all_of() para seguridad con select
  
  message("\n--- Nueva estructura simplificada (glimpse) ---")
  glimpse(meteoritos_proc)
  return(meteoritos_proc)
}

# 2. Descripción y Resumen General
# Función para obtener un resumen estadístico completo con skimr
obtener_resumen_skim <- function(df) {
  message("\n--- Resumen estadístico completo con skimr ---")
  resumen <- skim(df)
  return(resumen)
}

# Función para calcular rangos (amplitud) de columnas numéricas
calcular_rangos_amplitud <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) {
      return(max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
    } else {
      return(NA)
    }
  })
}

# Función para calcular rangos (intervalo) de columnas numéricas
calcular_rangos_intervalo <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) {
      r <- range(col, na.rm = TRUE)
      return(paste0("[", r[1], ", ", r[2], "]"))
    } else {
      return(NA)
    }
  })
}

# 3. Limpieza Profunda
# Función para realizar la limpieza profunda del dataset
limpieza_profunda <- function(df, anio_min = 860, masa_min_g = 1) {
  
  # Asegurar tipos de datos correctos
  df_limpio <- df %>%
    mutate(
      masa_g = as.numeric(masa_g),
      anio = as.numeric(anio),
      latitud = as.numeric(latitud),
      longitud = as.numeric(longitud)
    )
  
  # Obtener el año actual dinámicamente
  anio_actual <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Aplicar filtros y eliminar NAs
  df_limpio <- df_limpio %>%
    filter(
      anio >= anio_min & anio <= anio_actual, # Filtrar años absurdos
      !(latitud == 0 & longitud == 0),      # Filtrar coordenadas (0,0)
      masa_g > masa_min_g                    # Meteoritos de más de 1 gramo
    ) %>%
    drop_na(masa_g, latitud, longitud, anio) # Eliminar filas con NA en columnas clave
  
  message(paste("\n--- Limpieza Profunda Aplicada ---"))
  message(paste("Dimensiones del dataset limpio:", nrow(df_limpio), "filas,", ncol(df_limpio), "columnas"))
  return(df_limpio)
}

# 4. Análisis de Outliers (Masa de Meteoritos)
# Función para visualizar la distribución de masa_g con boxplot y escala logarítmica
plot_distribucion_masa <- function(df, columna_masa = "masa_g", titulo = "Distribución de la Masa de los Meteoritos (Escala Log)") {
  ggplot(df, aes(y = .data[[columna_masa]])) +
    geom_boxplot(fill = "orange") +
    scale_y_log10(labels = scales::comma) +
    labs(
      title = titulo,
      y = "Masa (gramos)",
      caption = "La escala logarítmica ayuda a visualizar distribuciones muy asimétricas"
    ) +
    theme_minimal()
}

# Función para identificar los N meteoritos más masivos/pequeños
identificar_top_meteoritos <- function(df, columna_masa = "masa_g", n = 5, masivos = TRUE) {
  if (masivos) {
    df_ordenado <- df %>% arrange(desc(.data[[columna_masa]]))
    mensaje <- paste("Los", n, "meteoritos más masivos:")
  } else {
    df_ordenado <- df %>% arrange(.data[[columna_masa]])
    mensaje <- paste("Los", n, "meteoritos más pequeños en masa:")
  }
  
  resultado <- df_ordenado %>%
    select(nombre, .data[[columna_masa]], anio, clase_meteorito) %>%
    head(n)
  
  message(paste("\n---", mensaje, "---"))
  return(resultado)
}

# Función para eliminar outliers usando el método IQR
eliminar_outliers_iqr <- function(df, columna) {
  if (!is.numeric(df[[columna]])) {
    stop(paste("La columna '", columna, "' no es numérica."))
  }
  Q1 <- quantile(df[[columna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[columna]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR_val
  lim_sup <- Q3 + 1.5 * IQR_val
  df_filtrado <- subset(df, df[[columna]] >= lim_inf & df[[columna]] <= lim_sup)
  message(paste("Outliers eliminados de la columna '", columna, "'. Filas restantes:", nrow(df_filtrado)))
  return(df_filtrado)
}

# 5. Análisis Temporal (Año del Evento)
# Función para visualizar el número de hallazgos por año
plot_hallazgos_por_anio <- function(df, columna_anio = "anio") {
  df %>%
    count(.data[[columna_anio]]) %>%
    ggplot(aes(x = .data[[columna_anio]], y = n)) +
    geom_line(color = "skyblue", size = 1) +
    labs(
      title = "Número de Hallazgos de Meteoritos Registrados por Año",
      x = "Año",
      y = "Número de Hallazgos Registrados"
    ) +
    theme_minimal()
}

# 6. Análisis Geográfico (Latitud y Longitud)
# Función para mapear la distribución geográfica de los meteoritos
mapa_meteoritos <- function(df, lat = "latitud", lon = "longitud") {
  df_filtrado <- df[!is.na(df[[lat]]) & !is.na(df[[lon]]), ]
  mapa_mundo <- map_data("world")
  
  ggplot() +
    geom_polygon(data = mapa_mundo,
                 aes(x = long, y = lat, group = group),
                 fill = "gray90", color = "white") +
    geom_point(data = df_filtrado,
               aes_string(x = lon, y = lat), # aes_string para variables pasadas como string
               color = "skyblue", alpha = 0.3, size = 0.8) +
    coord_map("mercator") +
    labs(title = "Distribución Geográfica de los Hallazgos de Meteoritos") +
    theme_minimal()
}

# 7. Análisis Categórico (Clase de Meteorito)
# Función para visualizar las Top N clases de meteoritos más comunes
plot_top_clases_comunes <- function(df, columna_clase = "clase_meteorito", n_top = 10) {
  ggplot(df, aes(y = fct_lump(fct_infreq(.data[[columna_clase]]), n = n_top))) +
    geom_bar(fill = "skyblue") +
    labs(
      title = paste("Top", n_top, "Clases de Meteoritos Más Comunes"),
      y = "Clase de Meteorito",
      x = "Conteo"
    ) +
    theme_minimal()
}

# Función para visualizar la distribución de masa por clase de meteorito (Top N)
plot_masa_por_clase <- function(df, columna_clase = "clase_meteorito", columna_masa = "masa_g", n_top = 10) {
  df %>%
    mutate(clase_agrupada = fct_lump(.data[[columna_clase]], n = n_top)) %>%
    filter(clase_agrupada != "Other") %>%
    ggplot(aes(x = fct_reorder(clase_agrupada, .data[[columna_masa]], .fun = median, na.rm = TRUE), y = .data[[columna_masa]])) +
    geom_boxplot(aes(fill = clase_agrupada)) +
    scale_y_log10(labels = scales::comma) +
    coord_flip() +
    guides(fill = "none") +
    labs(
      title = paste("Distribución de Masa por Clase de Meteorito (Top", n_top, ")"),
      x = "Clase de Meteorito",
      y = "Masa (gramos) en Escala Log"
    ) +
    theme_minimal()
}

################################################################################
# FLujo Principal del Análisis
################################################################################

ruta_datos <- "C:/Users/denny/Documents/Programación/programacion_estadistica/R/Meteorite_Landings.csv" 
 
# Función principal que orquesta todo el análisis
ejecutar_analisis_meteoritos <- function(ruta_archivo_csv) {
  
  # Definir nuevos nombres de columnas
  nuevos_nombres_columnas <- c(
    "nombre", "id", "tipo_nombre", "clase_meteorito", "masa_g",
    "evento", "anio", "latitud", "longitud", "geolocalizacion"
  )
  
  # 0. Cargar los datos y renombrar columnas
  meteoritos_df <- cargar_datos(ruta_archivo_csv)
  meteoritos_df <- renombrar_columnas(meteoritos_df, nuevos_nombres_columnas)
  
  # 1. Vistazo Inicial y Limpieza Preliminar (selección de columnas)
  meteoritos_proc <- vistazo_y_seleccion_inicial(meteoritos_df)
  # Si quieres ver el dataframe resultante de esta etapa: View(meteoritos_proc)
  
  # 2. Descripción y Resumen General
  resumen_inicial <- obtener_resumen_skim(meteoritos_proc)
  message("\n--- Resumen inicial con skimr (completo) ---")
  print(resumen_inicial)
  
  message("\n--- Rangos (amplitud) de columnas numéricas ---")
  print(calcular_rangos_amplitud(meteoritos_proc))
  
  message("\n--- Rangos (intervalo) de columnas numéricas ---")
  print(calcular_rangos_intervalo(meteoritos_proc))
  
  message("\n--- Resumen detallado de columnas numéricas con skimr ---")
  print(resumen_inicial[resumen_inicial$skim_type == "numeric", ])
  
  # 3. Limpieza Profunda
  meteoritos_limpio <- limpieza_profunda(meteoritos_proc)
  message("\n--- Vistazo del dataset limpio (glimpse) ---")
  glimpse(meteoritos_limpio)
  
  resumen_limpio <- obtener_resumen_skim(meteoritos_limpio)
  message("\n--- Resumen del dataset limpio con skimr (completo) ---")
  print(resumen_limpio)
  
  message("\n--- Resumen detallado de columnas numéricas en el dataset limpio ---")
  print(resumen_limpio[resumen_limpio$skim_type == "numeric", ])
  # Si quieres ver el dataframe limpio: View(meteoritos_limpio)
  
  # 4. Análisis de Outliers (Masa de Meteoritos)
  message("\n--- Análisis de Outliers (Masa) ---")
  plot(plot_distribucion_masa(meteoritos_limpio)) # Muestra el gráfico
  
  print(identificar_top_meteoritos(meteoritos_limpio, n = 5, masivos = TRUE))
  print(identificar_top_meteoritos(meteoritos_limpio, n = 5, masivos = FALSE))
  
  # 5. Análisis Temporal (Año del Evento)
  message("\n--- Análisis Temporal ---")
  plot(plot_hallazgos_por_anio(meteoritos_limpio))
  
  # 6. Análisis Geográfico (Latitud y Longitud)
  message("\n--- Análisis Geográfico ---")
  plot(mapa_meteoritos(meteoritos_limpio))
  
  # 7. Análisis Categórico (Clase de Meteorito)
  message("\n--- Análisis Categórico ---")
  plot(plot_top_clases_comunes(meteoritos_limpio))
  plot(plot_masa_por_clase(meteoritos_limpio))
  
  message("\n--- Análisis completado ---")
  
  # Puedes devolver los dataframes finales si los necesitas para análisis posteriores
  return(list(
    datos_originales = meteoritos_df,
    datos_procesados_inicial = meteoritos_proc,
    datos_limpios = meteoritos_limpio
  ))
}

# Ejecutar el análisis completo
resultados_analisis <- ejecutar_analisis_meteoritos(ruta_datos)


datos_limpios_final <- resultados_analisis$datos_limpios
glimpse(datos_limpios_final)