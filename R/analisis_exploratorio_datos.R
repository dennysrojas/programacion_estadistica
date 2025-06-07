#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("maps")


#Paquetes necesarios
library(tidyverse)
library(skimr)
library(maps)

#0. Cargar los datos

#Adquisición de los datos desde el archivo 
Meteoritos_df <- read.csv("/Meteorite_Landings.csv")
View(Meteoritos_df)

#Segundo método
meteoritos <- read_csv("C:/Users/denny/Documents/Programación/programacion_estadistica/R/Meteorite_Landings.csv")
View(meteoritos)

#Extrar los nombres de las columnas
names(meteoritos)

#Cambiar los nombres de las columnas
colnames(meteoritos) <- c("nombre", "id", "tipo_nombre", "clase_meteorito","masa_g", "evento", "anio", "latitud", "longitud", "geolocalizacion")
View(meteoritos)

#1. Vistazo Inicial y Limpieza Preliminar

#Primero, demos un vistazo para confirmar los tipos de datos
glimpse(meteoritos)

# La columna 'geolocalizacion' es a menudo compleja o redundante si ya tenemos latitud y longitud.
# Vamos a quitarla junto con 'id' y 'tipo_nombre' para simplificar el análisis.
meteoritos_proc <- meteoritos %>%
  select(nombre, clase_meteorito, masa_g, evento, anio, latitud, longitud)

# Vemos la nueva estructura simplificada
glimpse(meteoritos_proc)


#2. Descripción y Resumen General
# Utilizamos skimr para obtener un resumen estadístico completo
skim(meteoritos_proc)

#Función para crear rangos como amplitud de las columnas numéricas
calcular_rangos <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) {
      return(max(col, na.rm = TRUE) - min(col, na.rm = TRUE))
    } else {
      return(NA)
    }
  })
}

#Usar la función
rangos <- calcular_rangos(meteoritos_proc)
rangos

#Función para crear rangos como intervalo de las columnas numéricas
rangos_intervalo <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) {
      r <- range(col, na.rm = TRUE)
      return(paste0("[", r[1], ", ", r[2], "]"))
    } else {
      return(NA)
    }
  })
}
#Usar la función
View(rangos_intervalo(meteoritos_proc))

#Revisar el resumen más en detalle por tipo de columnas
resumen <- skim(meteoritos_proc)
View(resumen[resumen$skim_type == "numeric", ])
#rango enorme entre el mínimo y el máximo, y una media muy diferente a la mediana, lo que confirma la presencia de outliers.

# Filtrar valores válidos de año
meteoritos_proc <- subset(meteoritos_proc, anio >= 860 & anio <= 2024)
View(rangos_intervalo(meteoritos_proc))

#3. Limpieza Profunda
# Creamos nuestro dataset final y limpio para el análisis
# Aseguramos que las variables numéricas clave sean del tipo correcto
# Filtramos años absurdos (ej: antes de Cristo o en el futuro)
# Filtramos coordenadas (0,0) que suelen ser placeholders para datos faltantes
# Eliminamos filas donde las columnas importantes no tengan datos
# Nos quedamos con meteoritos de más de 1 gramo
meteoritos_limpio <- meteoritos_proc %>% mutate(masa_g = as.numeric(masa_g),anio = as.numeric(anio),latitud = as.numeric(latitud),
                                                longitud = as.numeric(longitud)
) %>% filter(anio >= 860 & anio <= as.numeric(format(Sys.Date(), "%Y"))) %>% filter(!(latitud == 0 & longitud == 0)) %>%
  drop_na(masa_g, latitud, longitud, anio) %>%
  filter(masa_g > 1)

# Verifiquemos el resultado final de la limpieza
cat("Dimensiones del dataset limpio:", dim(meteoritos_limpio), "\n")
View(skim(meteoritos_limpio))
resumen <- skim(meteoritos_limpio)
View(resumen[resumen$skim_type == "numeric", ])

#4. Análisis de Outliers (Masa de Meteoritos)
#Pregunta: ¿Cómo se distribuye la masa_g de los meteoritos?
# Visualización de la distribución de masa_g usando una escala logarítmica
ggplot(meteoritos_limpio, aes(y = masa_g)) +
  geom_boxplot(fill = "orange") +
  scale_y_log10(labels = scales::comma) + # Escala logarítmica con etiquetas legibles
  labs(
    title = "Distribución de la Masa de los Meteoritos (Escala Log)",
    y = "Masa (gramos)",
    caption = "La escala logarítmica ayuda a visualizar distribuciones muy asimétricas"
  ) + theme_minimal()

# Identifiquemos los 5 meteoritos más masivos
View(meteoritos_limpio %>% arrange(desc(masa_g)) %>%
       select(nombre, masa_g, anio, clase_meteorito) %>%
       head(5))

# Identifiquemos los 5 meteoritos más pequeños en masa
View(meteoritos_limpio %>% arrange(masa_g) %>%
       select(nombre, masa_g, anio, clase_meteorito) %>%
       head(5))

#En caso de querer filtrar "eliminar" los outliers (función)
eliminar_outliers <- function(df, columna) {
  # Calcular cuartiles y IQR
  Q1 <- quantile(df[[columna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[columna]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  # Definir límites inferior y superior
  lim_inf <- Q1 - 1.5 * IQR_val
  lim_sup <- Q3 + 1.5 * IQR_val
  # Filtrar y devolver el nuevo dataframe
  df_filtrado <- subset(df, df[[columna]] >= lim_inf & df[[columna]] <= lim_sup)
  return(df_filtrado)
}

#Uso de la función que filtra los outliers
meteoritos_sin_outliers <- eliminar_outliers(meteoritos_limpio, "masa_g")

#Gráfico sin outliers
ggplot(meteoritos_sin_outliers, aes(y = masa_g)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_log10(labels = scales::comma) + # Escala logarítmica con etiquetas legibles
  labs(
    title = "Distribución de la Masa de los Meteoritos (Escala Log)",
    y = "Masa (gramos)",
    caption = "La escala logarítmica ayuda a visualizar distribuciones muy asimétricas"
  ) + theme_minimal()

#5. Análisis Temporal (Año del Evento)
#Pregunta: ¿Ha cambiado el número de hallazgos a lo largo del anio?
meteoritos_limpio %>% count(anio) %>% # Contamos hallazgos por año
  ggplot(aes(x = anio, y = n)) +
  geom_line(color = "skyblue", size = 1) +
  labs(
    title = "Número de Hallazgos de Meteoritos Registrados por Año",
    x = "Año",
    y = "Número de Hallazgos Registrados"
  ) +  theme_minimal()

#6. Análisis Geográfico (Latitud y Longitud)
#Pregunta: ¿Dónde se encuentran la mayoría de los meteoritos?

mapa_meteoritos <- function(df, lat = "latitud", lon = "longitud") {
  # Filtrar coordenadas válidas
  df_filtrado <- df[!is.na(df[[lat]]) & !is.na(df[[lon]]), ]
  # Cargar datos del mapa mundial
  mapa_mundo <- map_data("world")
  # Crear el gráfico
  ggplot() +
    geom_polygon(data = mapa_mundo,
                 aes(x = long, y = lat, group = group),
                 fill = "gray90", color = "white") +
    geom_point(data = df_filtrado,
               aes_string(x = lon, y = lat),
               color = "skyblue", alpha = 0.3) +
    coord_map("mercator") +
    labs(title = "Distribución Geográfica de los Hallazgos de Meteoritos") +
    theme_minimal()
}
#Usar la función
mapa_meteoritos(meteoritos_limpio)


#7. Análisis Categórico (Clase de Meteorito)
#Pregunta: ¿Qué clase_meteorito es más común y cuál tiende a ser más masiva?

# Pregunta 7a: ¿Cuáles son las 10 clases más comunes?
# Usamos fct_lump() para agrupar las menos comunes en "Other"
ggplot(meteoritos_limpio, aes(y = fct_lump(fct_infreq(clase_meteorito), n = 10))) +
  geom_bar(fill = "skyblue") +
  labs(
    title = "Top 10 Clases de Meteoritos Más Comunes",
    y = "Clase de Meteorito",
    x = "Conteo"
  )

# Pregunta 7b: ¿Cómo se relaciona la clase con la masa?
meteoritos_limpio %>%
  mutate(clase_agrupada = fct_lump(clase_meteorito, n = 10)) %>%
  filter(clase_agrupada != "Other") %>%
  ggplot(aes(x = fct_reorder(clase_agrupada, masa_g, .fun = median), y = masa_g)) + # Ordenamos por mediana
  geom_boxplot(aes(fill = clase_agrupada)) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  guides(fill = "none") +
  labs(
    title = "Distribución de Masa por Clase de Meteorito (Top 10)",
    x = "Clase de Meteorito",
    y = "Masa (gramos) en Escala Log"
  )