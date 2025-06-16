# 
# El dataset mtcars (Motor Trend Car Road Tests) es una base de datos 
# incorporada en R que contiene datos de rendimiento de 32 modelos de 
# automóviles de 1974. Cada fila representa un coche y cada columna 
# describe una característica, por ejemplo:
# 
#   mpg: Millas por galón (rendimiento del combustible).
#   cyl: Número de cilindros.
#   disp: Cilindrada del motor.
#   hp: Caballos de fuerza.
#   drat: Relación del eje trasero.
#   wt: Peso (en miles de libras).
#   qsec: Tiempo en recorrer 1/4 de milla.
#   vs: Motor en línea (0 = V, 1 = en línea).
#   am: Transmisión (0 = automática, 1 = manual).
#   gear: Número de marchas.
#   carb: Número de carburadores.


# Cargar las librerías necesarias para construir la app
library(shiny)           # Librería base para aplicaciones web en R
library(shinydashboard)  # Extensión para crear dashboards con Shiny
library(ggplot2)         # Librería para generar gráficos

# --------------------------
# Interfaz de Usuario (UI)
# --------------------------
ui <- dashboardPage(  # Define la estructura general del dashboard
  dashboardHeader(title = "Análisis interactivo de mtcars"),  # Encabezado con título
  dashboardSidebar(  # Barra lateral con menú y controles
    sidebarMenu(  # Menú de navegación
      menuItem("Exploración", tabName = "exploracion", icon = icon("chart-line")),  # Opción 1: pestaña de gráficos
      menuItem("Resumen", tabName = "resumen", icon = icon("table"))  # Opción 2: pestaña de resumen de datos
    ),
    # Selector de variable para el eje X del gráfico
    selectInput("xvar", "Variable en eje X", choices = names(mtcars), selected = "wt"),
    # Selector de variable para el eje Y del gráfico
    selectInput("yvar", "Variable en eje Y", choices = names(mtcars), selected = "mpg"),
    # Checkbox para activar o desactivar el coloreo por número de cilindros
    checkboxInput("color_by_cyl", "Colorear por número de cilindros", value = TRUE)
  ),
  dashboardBody(  # Cuerpo principal del dashboard con las pestañas de contenido
    tabItems(
      # Pestaña de exploración de datos
      tabItem(tabName = "exploracion",
              fluidRow(  # Fila para organizar los elementos horizontalmente
                box(title = "Gráfico de dispersión", width = 8, solidHeader = TRUE, status = "primary",
                    plotOutput("scatterPlot", height = "400px")),  # Área donde se mostrará el gráfico
                box(title = "Controles de usuario", width = 4, status = "info",
                    helpText("Seleccione las variables para el gráfico y coloreo."))  # Texto de ayuda
              )
      ),
      # Pestaña de resumen de datos
      tabItem(tabName = "resumen",
              fluidRow(
                box(title = "Resumen de Datos", width = 6, status = "success",
                    verbatimTextOutput("summary")),  # Muestra un resumen estadístico
                box(title = "Vista previa del dataset", width = 6, status = "warning",
                    tableOutput("table"))  # Tabla con las primeras filas del dataset
              )
      )
    )
  )
)

# --------------------------
# Lógica del servidor
# --------------------------
server <- function(input, output) {
  # Genera el gráfico de dispersión de forma reactiva según la selección del usuario
  output$scatterPlot <- renderPlot({
    # Crear el gráfico base con ggplot usando las variables seleccionadas
    p <- ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(size = 3)  # Puntos del gráfico con tamaño 3
    # Si el checkbox está activado, colorear los puntos por número de cilindros
    if (input$color_by_cyl) {
      p <- p + aes(color = as.factor(cyl)) +  # Convertir a factor para colorear
        labs(color = "Cilindros")  # Etiqueta de la leyenda
    }
    p + theme_minimal()  # Usar tema minimalista para el gráfico
  })
  # Generar y mostrar el resumen estadístico del dataset
  output$summary <- renderPrint({
    summary(mtcars)  # Muestra resumen (mínimo, máximo, media, etc.)
  })
  # Mostrar las primeras 10 filas del dataset
  output$table <- renderTable({
    head(mtcars, 10)
  })
}

# --------------------------
# Ejecutar la aplicación
# --------------------------
shinyApp(ui, server)  # Llamada para lanzar la app combinando interfaz y lógica