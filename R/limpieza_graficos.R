#Instalar paquetes

#install.packages("readxl")

#install.packages("writexl")

#install.packages("ggplot2")

#install.packages("dplyr")

# Cargar librerías

library(readxl)

library(writexl)

library(ggplot2)

library(dplyr)

# 1. Cargar el archivo Excel

ruta_archivo <- "C:/Users/denny/Documents/Programación/programacion_estadistica/R/archivo_practica.xlsx"  # <-- CAMBIA ESTA RUTA

df <- read_excel(ruta_archivo, sheet = 1)
View(df)


#Inspeccionar el dataframe:

# Ver las primeras 6 filas del dataframe

View(head(df))

# Ver las últimas 6 filas del dataframe

tail(df)

# Ver la estructura del dataframe: nombres de columnas, tipos de datos, primeros valores

str(df)

# Ver el resumen estadístico de todas las columnas numéricas

summary(df)

# Ver los nombres de las columnas

names(df)

# Ver el número de filas y columnas

dim(df)          # filas y columnas

nrow(df)         # solo número de filas

ncol(df)         # solo número de columnas

# Ver si hay valores nulos por columna

colSums(is.na(df))


#inspección visual del dataframe antes de la limpieza

na_matrix <- is.na(df) * 1

# Dibujar mapa de calor básico

heatmap(
  
  na_matrix,
  
  Rowv = NA,
  
  Colv = NA,
  
  col = c("gray90", "red"),
  
  scale = "none",
  
  margins = c(6, 8),
  
  main = "Mapa de calor de valores NA en Titanic"
  
)

# 2. Crear copia del DataFrame

df_clean <- df

# 3. Eliminar columna 'Cabin' (demasiados NA)

df_clean <- df_clean %>% select(-Cabin) #%>%: es el operador pipe (tubería) que significa "y luego". Se usa para encadenar operaciones de forma más legible.

# 4. Imputar medias en columnas numéricas

df_clean$Age[is.na(df_clean$Age)] <- mean(df_clean$Age, na.rm = TRUE)

df_clean$Fare[is.na(df_clean$Fare)] <- mean(df_clean$Fare, na.rm = TRUE)

# 5. Eliminar filas con valores nulos en 'Embarked'

df_clean <- df_clean %>% filter(!is.na(Embarked))


#inspección visual del dataframe después de la limpieza

na_matrix <- is.na(df_clean) * 1

# Dibujar mapa de calor básico

heatmap(
  
  na_matrix,
  
  Rowv = NA,
  
  Colv = NA,
  
  col = c("gray90", "red"),
  
  scale = "none",
  
  margins = c(6, 8),
  
  main = "Mapa de calor de valores NA en Titanic"
  
)

# 6. Gráficos exploratorios

# Gráfico de sobrevivientes

ggplot(df_clean, aes(x = factor(Survived))) +
  
  geom_bar(fill = "steelblue") +
  
  labs(title = "Distribución de Sobrevivientes", x = "Sobrevivió", y = "Cantidad")

# Histograma de edades

ggplot(df_clean, aes(x = Age)) +
  
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia")

# 7. Exportar DataFrame limpio a nuevo Excel

write_xlsx(df_clean, "C:/Users/denny/Documents/Programación/programacion_estadistica/R//titanic_limpio.xlsx")  # Esto guardará el archivo en tu carpeta de trabajo

