library(ggplot2)
library(dplyr)
library(caret)      # Para entrenamiento y evaluación
library(e1071)      # Necesario para algunos modelos en caret

#Crear un nuevo dataframe a partir del dataset a diamons 
df_diamantes <- diamonds
View(df_diamantes)

# Nombres de las columnas
names(df_diamantes)
# cambiar nombres
names(df_diamantes) <- c(
  "quilates",      # carat
  "corte",         # cut
  "color",         # color
  "claridad",      # clarity
  "profundidad",   # depth
  "tabla",         # table
  "precio",        # price
  "longitud",      # x
  "ancho",         # y
  "alto"           # z
)
#Mostrar nombres en español
names(df_diamantes)

#Explorar datos iniciales.
str(df_diamantes)         # Estructura
summary(df_diamantes)     # Estadísticas resumen
head(df_diamantes)        # Primeras filas

#Aplicación de modelos de regresión:
set.seed(123)  # Asegura reproducibilidad

# Crear índices para separar 80% entrenamiento
indices_entrenamiento <- createDataPartition(df_diamantes$precio, p = 0.8, list = FALSE)
dim(indices_entrenamiento)
View(indices_entrenamiento)
# Crear los conjuntos
datos_entrenamiento <- df_diamantes[indices_entrenamiento, ]
datos_prueba <- df_diamantes[-indices_entrenamiento, ]

# Entrenar solo con el conjunto de entrenamiento
modelo_reg <- train(
  precio ~ quilates + profundidad + tabla + longitud + ancho + alto,
  data = datos_entrenamiento,
  method = "lm"
)

# Ver resumen del modelo
x <-summary(modelo_reg$finalModel)
View(x)

# Coeficientes: estimaciones para cada variable (pendientes y el intercepto).
# Errores estándar de los coeficientes.
# Estadísticos t y valores p para evaluar la significancia de cada coeficiente.
# R-cuadrado (R²): qué tanto explica el modelo la variabilidad de los datos.
# R-cuadrado ajustado: similar al anterior, pero ajustado por el número de variables.
# Error residual estándar: qué tan lejos están, en promedio, las predicciones de los valores reales.
# Estadístico F y su p-valor para evaluar si el modelo en conjunto es significativo

# Usar el modelo para predecir en el conjunto de prueba
predicciones <- predict(modelo_reg, newdata = datos_prueba)

# Evaluar el modelo en el conjunto de prueba
resultado_lm <- postResample(pred = predicciones, obs = datos_prueba$precio)
print(resultado_lm)

#Interpretación de métricas
#RMSE
# Es el error cuadrático medio raíz, y mide el promedio de cuánto se equivoca tu modelo, pero penalizando más los errores grandes.
# En este caso: tu modelo se equivoca en promedio unos $1506 al predecir el precio de un diamante.
# Cuanto más bajo, mejor.
# Ejemplo: Si el precio real de un diamante es $10,000, tu modelo podría estar prediciendo $8,500 o $11,300.

#Rsquared
# Es el coeficiente de determinación.
# Mide qué tan bien tu modelo explica la variabilidad de los precios reales.
# Su valor está entre 0 y 1.
# 0 = el modelo no explica nada.
# 1 = el modelo explica todo perfectamente.
# En tu caso: el modelo explica el 85.5% de la variación del precio de los diamantes.
# ¡Es un resultado bastante bueno para un modelo lineal simple!

# MAE
# Es el error absoluto medio, es decir, en promedio, tu modelo se equivoca $890 al predecir el precio.
# A diferencia del RMSE, no penaliza tanto los errores grandes, por eso suele ser menor.
# También, cuanto más bajo, mejor.

#Análisis gráfico
# Creamos un dataframe con reales y predichos
df_resultados <- data.frame(
  Real = datos_prueba$precio,
  Predicho = predicciones
)

# Gráfico
ggplot(df_resultados, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = "blue") +  # Puntos
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Línea ideal
  labs(
    title = "Valores Reales vs. Valores Predichos",
    x = "Precio real ($)",
    y = "Precio predicho ($)"
  ) +
  theme_minimal()

#Interpretación:
# Los puntos deberían estar cerca de la línea roja (ideal), que representa cuando el modelo acierta perfectamente.
# Si hay muchos puntos lejos de esa línea, hay errores grandes.

#predicción con Random Forest
# Grid de hiperparámetros (número de predictores por división)
grid_rf <- expand.grid(mtry = c(2, 3))

# Entrenamiento con validación cruzada
modelo_rf <- train(  # Entrena un modelo con la función train() del paquete caret y guarda el resultado en 'modelo_rf'
  precio ~ quilates + profundidad + tabla + longitud + ancho + alto,  # Fórmula: variable dependiente 'precio' según las variables independientes
  data = datos_entrenamiento,  # Conjunto de datos que se utilizará para entrenar el modelo
  method = "rf",  # Especifica el método: 'rf' significa Random Forest
  tuneGrid = grid_rf,  # Conjunto de valores de 'mtry' que se probarán para encontrar el mejor
  trControl = trainControl(method = "cv", number = 5),  # Configura la validación cruzada: 5 particiones (5-fold CV)
  ntree = 50  # Número de árboles que se construirán en el bosque (menos de los 500 por defecto para acelerar el proceso)
)

#Ver mejor combinación de hiperparámetros
modelo_rf$bestTune

#Predecir con el conjunto de prueba
predicciones_rf <- predict(modelo_rf, newdata = datos_prueba)

#Evaluar métricas (igual que antes)
resultado_rf <- postResample(pred = predicciones_rf, obs = datos_prueba$precio)
print(resultado_rf)

#Graficar
df_rf <- data.frame(
  Real = datos_prueba$precio,
  Predicho = predicciones_rf
)

ggplot(df_rf, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = "darkorange") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "Random Forest: Precio Real vs. Predicho",
    x = "Precio real ($)",
    y = "Precio predicho ($)"
  ) +
  theme_minimal()

#Comparar métricas con regresión lineal
comparacion <- rbind(
  Lineal = resultado_lm,
  RandomForest = resultado_rf
)
View(comparacion)

#Probar con datos reales
nuevo_diamante <- data.frame(
  quilates = 0.29,
  profundidad = 62.4,
  tabla = 58,
  longitud = 4.2,
  ancho = 4.23,
  alto = 2.63
)

#Con Random Forest
precio_estimado_rf <- predict(modelo_rf, newdata = nuevo_diamante)
precio_estimado_rf

#Con Regresión lineal
precio_estimado_lm <- predict(modelo_reg, newdata = nuevo_diamante)
precio_estimado_lm