# Cargar paquete Shiny
library(shiny)

# ---------------------------
# UI (Interfaz de usuario)
# ---------------------------
ui <- fluidPage(
  # Título de la aplicación
  titlePanel("Histograma Dinámico"),
  # Layout con barra lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Número de intervalos (bins):",
                  min = 5,
                  max = 50,
                  value = 30),
      helpText("Ajusta el número de intervalos del histograma para observar la distribución del tiempo de espera entre erupciones.")
    ),
    mainPanel(
      plotOutput("distPlot"),
      br(),
      verbatimTextOutput("resumen")
    )
  )
)

# ---------------------------
# Server (Lógica del servidor)
# ---------------------------
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Obtener columna del tiempo de espera
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # Calcular la media para mostrarla como línea vertical
    media <- mean(x)
    # Dibujar el histograma
    hist(x, breaks = bins, col = "steelblue", border = "white",
         xlab = "Tiempo de espera hasta la siguiente erupción (minutos)",
         main = "Histograma del tiempo de espera")
    # Agregar línea vertical con la media
    abline(v = media, col = "red", lwd = 2, lty = 2)
    legend("topright", legend = paste("Media =", round(media, 1)), 
           col = "red", lty = 2, lwd = 2, bty = "n")
  })
  output$resumen <- renderPrint({
    # Mostrar resumen estadístico del tiempo de espera
    summary(faithful$waiting)
  })
}

# ---------------------------
# Ejecutar la aplicación
# ---------------------------
shinyApp(ui = ui, server = server)