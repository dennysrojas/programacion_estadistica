#PREPARACIÓN DEL ENTORNO

# Instalar paquetes si no los tienes (descomenta la línea si es necesario)
#install.packages(c("ggplot2", "dplyr", "caTools", "MASS", "caret"))

# Cargar las librerías necesarias para la práctica
library(ggplot2) # Contiene el dataset 'diamonds'
library(dplyr)   # Para manipulación de datos como renombrar columnas
library(caTools) # Para dividir los datos en entrenamiento y prueba
library(MASS)    # Para ejecutar la regresión logística ordinal (función polr)
library(caret)   # Para calcular la matriz de confusión y métricas de evaluación
library(randomForest)


# CARGA Y PREPARACIÓN DE LOS DATOS

# Cargar el dataset 'diamonds'
datos_originales <- ggplot2::diamonds
datos_originales
# Renombrar las columnas al español como especificaste
datos_diamantes <- datos_originales %>%
  rename(
    quilates = carat,
    corte = cut,
    color = color,
    claridad = clarity,
    profundidad = depth,
    tabla = table,
    precio = price,
    longitud = x,
    ancho = y,
    alto = z
  )

# La regresión ordinal necesita que la variable objetivo sea un factor ordenado.
# Le indicamos a R el orden jerárquico correcto del corte, de peor a mejor.
datos_diamantes$corte <- factor(datos_diamantes$corte,
                                levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
                                ordered = TRUE)

# Fijar una semilla de aleatoriedad para que la división de datos sea siempre la misma (reproducibilidad)
set.seed(123)


# DIVISIÓN DE DATOS EN ENTRENAMIENTO Y PRUEBA
# Crear la división de datos: 80% para entrenamiento, 20% para prueba
# La función sample.split se asegura de que la proporción de cada categoría de 'corte' sea similar en ambos conjuntos
division <- sample.split(datos_diamantes$corte, SplitRatio = 0.8)

# Crear el conjunto de entrenamiento con el 80% de los datos
entrenamiento <- subset(datos_diamantes, division == TRUE)

# Crear el conjunto de prueba con el 20% restante
prueba <- subset(datos_diamantes, division == FALSE)


# ENTRENAMIENTO DEL MODELO DE REGRESIÓN LOGÍSTICA ORDINAL
# Entrenar el modelo usando la función polr() del paquete MASS
# Vamos a predecir 'corte' usando las otras variables categóricas y los quilates
modelo_logistico <- polr(corte ~ color + claridad + quilates, data = entrenamiento)

# Opcional: ver un resumen del modelo entrenado (coeficientes, etc.)
View(summary(modelo_logistico))


#PREDICCIÓN Y EVALUACIÓN DEL MODELO

# Realizar predicciones sobre el conjunto de prueba, que el modelo nunca ha visto
predicciones <- predict(modelo_logistico, newdata = prueba)

# Generar la matriz de confusión y todas las métricas de evaluación
# Comparamos las 'predicciones' con los valores reales ('prueba$corte')
matriz_confusion <- confusionMatrix(data = predicciones, reference = prueba$corte)

# Imprimir los resultados de la evaluación
matriz_confusion

# VISUALIZACIÓN DE MÉTRICAS DE EVALUACIÓN

library(stringr)
library(reshape2)

# Convierte la parte "byClass" de la matriz de confusión en un data frame.
# Esta parte contiene las métricas por cada clase del modelo de clasificación (Precision, Recall, F1, etc.)
metricas_por_clase <- as.data.frame(matriz_confusion$byClass)

# Agrega una nueva columna llamada 'Clase' con los nombres de las filas (por ejemplo: "Class: Ideal", "Class: Premium", etc.)
metricas_por_clase$Clase <- rownames(metricas_por_clase)

# Limpia los nombres de clase eliminando el prefijo "Class: " para que quede solo el nombre de la clase (por ejemplo: "Ideal")
metricas_por_clase$Clase <- str_replace(metricas_por_clase$Clase, "Class: ", "")

# Selecciona únicamente las columnas necesarias para graficar: Clase, Precisión, Recall y F1-score
metricas_a_graficar <- metricas_por_clase[, c("Clase", "Precision", "Recall", "F1")]

# Convierte el data frame de formato ancho a largo para facilitar la visualización con ggplot2.
# 'Clase' se mantiene fija, y las columnas de métricas se reorganizan como pares clave-valor: Metrica (nombre) y Valor (número)
metricas_long <- melt(metricas_a_graficar, id.vars = "Clase", variable.name = "Metrica", value.name = "Valor")


# Crear el gráfico base
grafico_separado <- ggplot(metricas_long, 
                           # En el eje X ahora va la Métrica, en el Y el Valor
                           aes(x = Metrica, y = Valor, fill = Metrica)) +
  # 1. Gráfico de barras simple, sin valores encima
  geom_bar(stat = "identity") +
  # 2. La función clave: crea una "faceta" o sub-gráfico para cada 'Clase'
  # ncol=3 los arreglará en una cuadrícula de 3 columnas
  facet_wrap(~ Clase, ncol = 3) +
  # 3. Títulos y etiquetas simples
  labs(
    title = "Métricas de Evaluación por Clase",
    subtitle = "Cada panel muestra el rendimiento para una calidad de corte",
    x = "Métrica",
    y = "Puntuación (de 0 a 1)"
  ) +
  # 4. Un tema limpio y clásico, sin personalización extra
  theme_bw() 

  # Imprimir el nuevo gráfico
  print(grafico_separado)

print(matriz_confusion$table)

#Prueba con datos reales
#OBTENER LOS NIVELES DE LAS VARIABLES CATEGÓRICAS
# Extrae los niveles (categorías únicas) de la variable 'color' del data frame 'datos_diamantes'.
# Esta variable es un factor que representa el color del diamante (por ejemplo: D, E, F, ..., J).
niveles_color <- levels(datos_diamantes$color)

# Extrae los niveles (categorías únicas) de la variable 'claridad' del data frame 'datos_diamantes'.
# Esta variable es un factor que representa la claridad del diamante (por ejemplo: IF, VVS1, VVS2, SI1, etc.).
niveles_claridad <- levels(datos_diamantes$claridad)


# CREAR LOS DATOS DE LOS 3 NUEVOS DIAMANTES
diamantes_nuevos <- data.frame(
  # Columna 'quilates' con los tres valores
  quilates = c(0.4, 1.8, 1.0),
  # Columna 'color' con los tres valores
  color = factor(c("E", "J", "G"), levels = niveles_color),
  # Columna 'claridad' con los tres valores
  claridad = factor(c("VVS1", "SI2", "VS1"), levels = niveles_claridad)
)


# REALIZAR LA PREDICCIÓN 
prediccion_final <- predict(modelo_logistico, newdata = diamantes_nuevos)


# MOSTRAR LOS RESULTADOS DE FORMA CLARA 
resultado_final <- cbind(diamantes_nuevos, corte_predicho = prediccion_final)

# Imprimir la tabla de resultados corregida
print(resultado_final)


#---------------#
# Random Forest #
#---------------#

modelo_rf <- randomForest(corte ~ .,
                          data = entrenamiento,
                          ntree = 200,
                          sampling = "smote" ,
                          importance = TRUE) # Guardar métricas de importancia de variables

print(modelo_rf)

predicciones_rf <- predict(modelo_rf, newdata = prueba)

matriz_confusion_rf <- confusionMatrix(data = predicciones_rf, reference = prueba$corte)

print(matriz_confusion_rf)

print(matriz_confusion$table)
print(matriz_confusion_rf$table)


metricas_por_clase_rf <- as.data.frame(matriz_confusion_rf$byClass)
metricas_por_clase_rf$Clase <- str_replace(rownames(metricas_por_clase_rf), "Class: ", "")
metricas_a_graficar_rf <- metricas_por_clase_rf[, c("Clase", "Precision", "Recall", "F1")]
metricas_long_rf <- melt(metricas_a_graficar_rf, id.vars = "Clase", variable.name = "Metrica", value.name = "Valor")

grafico_rf <- ggplot(metricas_long_rf, aes(x = Metrica, y = Valor, fill = Metrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Clase, ncol = 3) +
  labs(
    title = "Métricas de Evaluación del Modelo Random Forest por Clase",
    subtitle = "Rendimiento para cada calidad de corte",
    x = "Métrica de Evaluación",
    y = "Puntuación (de 0 a 1)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Mejorar legibilidad de etiquetas

print(grafico_rf)