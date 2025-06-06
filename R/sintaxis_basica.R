#------------------------------------------------------------------#
# Parte 1: R como Calculadora y Primeras Variables
#------------------------------------------------------------------#

# R puede usarse como una calculadora potente.
# Probemos algunas operaciones aritméticas básicas:

5 + 3
10 - 4
6 * 8
100 / 5
2^3  # Exponenciación (2 elevado a 3)
(5 + 3) * 2 # Uso de paréntesis

# Asignación de valores a variables
# Usamos '<-' para asignar.

nota_parcial1 <- 7.5
nota_parcial2 <- 8.0

# Ver el valor de una variable (simplemente escribiendo su nombre)
nota_parcial1
nota_parcial2

# Calcular el promedio de las notas
promedio_notas <- (nota_parcial1 + nota_parcial2) / 2
promedio_notas

# Listar los objetos (variables) que hemos creado en nuestro entorno
ls()


#------------------------------------------------------------------#
# Parte 2: Vectores – Nuestros Primeros Conjuntos de Datos
#------------------------------------------------------------------#

# Un vector es una secuencia de elementos del mismo tipo.
# Creemos un vector con los nombres de algunos estudiantes (tipo caracter)
nombres_estudiantes <- c("Ana", "Luis", "Eva", "Juan", "Sofia")
nombres_estudiantes

# Creemos un vector con sus edades (tipo numérico)
edades_estudiantes <- c(22, 24, 21, 23, 22)
edades_estudiantes

# Creemos un vector con sus notas finales (tipo numérico)
notas_finales <- c(8.5, 7.0, 9.2, 6.5, 8.8)
notas_finales

# ¿Cuántos elementos tiene cada vector? Usamos la función length()
length(nombres_estudiantes)
length(notas_finales)

# Operaciones básicas con vectores numéricos
# Supongamos que hubo un bono de 0.5 puntos para todos
notas_ajustadas <- notas_finales + 0.5
notas_ajustadas

# Acceder a elementos de un vector (Indexación)
# R usa índices que comienzan en 1.

nombres_estudiantes[1]       # Primer estudiante
notas_finales[3]           # Nota del tercer estudiante
nombres_estudiantes[c(2, 4)] # Estudiantes en la posición 2 y 4

# Indexación lógica: ¿Qué estudiantes aprobaron con más de 8?
# Primero, creamos un vector lógico
aprobaron_con_mas_de_8 <- notas_finales > 8.0
aprobaron_con_mas_de_8

# Ahora usamos este vector lógico para filtrar los nombres
nombres_estudiantes[aprobaron_con_mas_de_8]
notas_finales[aprobaron_con_mas_de_8]

#------------------------------------------------------------------#
# Parte 3: Data Frames – Organizando Datos Tabulares
#------------------------------------------------------------------#

# Un data frame es como una tabla u hoja de cálculo.
# Podemos crearlo a partir de nuestros vectores.
# ¡Importante! Todos los vectores deben tener la misma longitud.

datos_clase <- data.frame(
  Nombre = nombres_estudiantes,
  Edad = edades_estudiantes,
  NotaFinal = notas_finales
)

# Ver nuestro data frame
datos_clase

# Para una mejor visualización en RStudio, podemos usar View() (con V mayúscula)
View(datos_clase)

# Explorar la estructura del data frame
head(datos_clase)      # Muestra las primeras 6 filas
str(datos_clase)       # Muestra la estructura (tipos de datos de cada columna)
summary(datos_clase)   # Proporciona un resumen estadístico de cada columna
dim(datos_clase)       # Dimensiones (filas, columnas)
nrow(datos_clase)      # Número de filas
ncol(datos_clase)      # Número de columnas
names(datos_clase)     # Nombres de las columnas

# Acceder a columnas del data frame
# Usando el signo '$'
datos_clase$Nombre
datos_clase$NotaFinal

# Usando corchetes '[]' (como una matriz: [filas, columnas])
datos_clase[, "Edad"]       # Todas las filas, columna "Edad"
datos_clase[, c("Nombre", "NotaFinal")] # Todas las filas, columnas "Nombre" y "NotaFinal"

# Acceder a filas específicas
datos_clase[1, ]             # Primera fila, todas las columnas
datos_clase[c(1, 3, 5), ]    # Filas 1, 3 y 5

# Acceder a un valor específico
datos_clase[2, "Edad"]       # Edad del segundo estudiante

# Filtrar filas basado en una condición
# Estudiantes que tienen más de 22 años
estudiantes_mayores_22 <- datos_clase[datos_clase$Edad > 22, ]
estudiantes_mayores_22

# Estudiantes con NotaFinal mayor o igual a 8.0
estudiantes_buena_nota <- datos_clase[datos_clase$NotaFinal >= 8.0, ]
estudiantes_buena_nota

# Añadir una nueva columna al data frame
# Por ejemplo, si el estudiante aprobó (NotaFinal >= 7.0)
datos_clase$Aprobado <- datos_clase$NotaFinal >= 7.0
datos_clase # Ver el data frame con la nueva columna
str(datos_clase) # Notar que la nueva columna es de tipo 'logical'


#------------------------------------------------------------------#
# Parte 4: Visualización Sencilla con Base R
#------------------------------------------------------------------#

# R es excelente para gráficos. Comencemos con algo muy simple.
# Un diagrama de dispersión de Edades vs. Notas Finales

plot(
  x = datos_clase$Edad,
  y = datos_clase$NotaFinal,
  main = "Diagrama de Dispersión: Edad vs. Nota Final", # Título del gráfico
  xlab = "Edad (años)",                               # Etiqueta del eje X
  ylab = "Nota Final",                                # Etiqueta del eje Y
  pch = 16,                                           # Tipo de símbolo (círculo relleno)
  col = "blue"                                        # Color de los puntos
)

# Un histograma de las Notas Finales para ver su distribución
hist(
  datos_clase$NotaFinal,
  main = "Histograma de Notas Finales",
  xlab = "Nota Final",
  ylab = "Frecuencia",
  col = "lightblue",
  border = "black"
)

# Un gráfico de barras simple de las edades (primero necesitamos contar frecuencias)
# Si tenemos muchas edades repetidas, un gráfico de barras es útil.
# Para este ejemplo, como hay pocas y casi únicas, puede no ser el mejor,
# pero sirve para ilustrar.
# Primero, contamos cuántos estudiantes hay por cada edad:
conteo_edades <- table(datos_clase$Edad)
conteo_edades # Muestra la tabla de frecuencias

barplot(
  conteo_edades,
  main = "Distribución de Edades",
  xlab = "Edad",
  ylab = "Número de Estudiantes",
  col = c("orange", "lightgreen", "skyblue") # Podemos dar varios colores
)


#------------------------------------------------------------------#
# Parte 5: Usando Funciones Predefinidas y Creando una Simple
#------------------------------------------------------------------#

# R tiene muchas funciones útiles ya incorporadas.
# Ya hemos usado algunas: c(), length(), data.frame(), head(), str(), summary(), plot(), mean(), etc.

# Calcular la media de las notas finales
media_notas <- mean(datos_clase$NotaFinal)
media_notas

# Calcular la suma de las edades (quizás no muy útil, pero ilustrativo)
suma_edades <- sum(datos_clase$Edad)
suma_edades

# Obtener la nota mínima y máxima
nota_minima <- min(datos_clase$NotaFinal)
nota_maxima <- max(datos_clase$NotaFinal)
nota_minima
nota_maxima

# ¿Cuántos estudiantes tenemos? (ya lo vimos con nrow)
numero_estudiantes <- nrow(datos_clase)
numero_estudiantes

# --- Creando nuestra propia función sencilla ---
# Supongamos que queremos una función que salude a un estudiante

saludar_estudiante <- function(nombre) {
  mensaje <- paste("¡Hola,", nombre, "! Bienvenido/a al curso de R.")
  return(mensaje)
}

# Probemos nuestra función:
saludo_ana <- saludar_estudiante("Ana")
saludo_ana

saludo_luis <- saludar_estudiante(datos_clase$Nombre[2]) # Usando un nombre del data frame
saludo_luis

# Una función que calcule el rango de un vector numérico (max - min)
calcular_rango <- function(numeros) {
  valor_minimo <- min(numeros)
  valor_maximo <- max(numeros)
  rango_calculado <- valor_maximo - valor_minimo
  return(rango_calculado)
}

# Probemos con las notas finales
rango_notas <- calcular_rango(datos_clase$NotaFinal)
rango_notas
# También existe la función base range() que devuelve el min y el max
range(datos_clase$NotaFinal)

