# app.R
library(shiny)
library(shinydashboard)
library(palmerpenguins)
library(ggplot2)
library(dplyr)

# Cargar y limpiar datos
df_pinguinos <- na.omit(penguins)
colnames(df_pinguinos) <- c("especie", "isla", "longitud_pico_mm", "profundidad_pico_mm",
                            "longitud_aleta_mm", "masa_corporal_g", "sexo", "anio")

# --- Interfaz de Usuario (UI) ---
ui <- dashboardPage(

    # Cabecera del dashboard
    dashboardHeader(title = "Diamonds Dashboard"),

    # Barra lateral con el menú
    dashboardSidebar(
        sidebarMenu(
            # Cada menuItem es una sección. El tabName debe ser único.
            menuItem("Precio de Diamantes", tabName = "precios", icon = icon("dollar-sign")),
            menuItem("Explorador de Datos", tabName = "tabla", icon = icon("table")),
            menuItem("Análisis Adicional", tabName = "analisis", icon = icon("chart-line"))
        )
    ),

    # Cuerpo del dashboard donde se mostrará el contenido de cada sección
    dashboardBody(
        tabItems(
            # Contenido para la primera sección: "Precio de Diamantes"
            tabItem(tabName = "precios",
                # Usamos fluidRow para organizar los elementos
                fluidRow(
                    # Caja para los controles
                    box(title = "Controles", status = "primary", solidHeader = TRUE,
                        sliderInput("bins",
                                    "Número de divisiones (bins):",
                                    min = 1,
                                    max = 100,
                                    value = 30)
                    ),
                    # Caja para el gráfico
                    box(title = "Histograma", status = "primary", solidHeader = TRUE,
                        plotOutput("distPlot")
                    )
                )
            ),

            # Contenido para la segunda sección: "Explorador de Datos"
            tabItem(tabName = "tabla",
                h2("Aquí irá la tabla de datos del dataset diamonds")
                # Dejamos este espacio para el siguiente requerimiento
            ),

            # Contenido para la tercera sección: "Análisis Adicional"
            tabItem(tabName = "analisis",
                h2("Aquí irán otros análisis y gráficos")
                # Dejamos este espacio para futuros requerimientos
            )
        )
    )
)

# Server
server <- function(input, output) {
  output$tabla_head <- DT::renderDataTable({
    head(df_pinguinos)
  })
  output$n_pinguinos <- renderValueBox({
    valueBox(nrow(df_pinguinos), "Pingüinos registrados", icon = icon("users"), color = "blue")
  })
  output$prom_pico <- renderValueBox({
    pico <- round(mean(df_pinguinos$longitud_pico_mm), 2)
    valueBox(pico, "Prom. Longitud Pico (mm)", icon = icon("ruler-horizontal"), color = "green")
  })
  output$prom_masa <- renderValueBox({
    masa <- round(mean(df_pinguinos$masa_corporal_g), 2)
    valueBox(masa, "Prom. Masa Corporal (g)", icon = icon("weight"), color = "purple")
  })
  output$prom_aleta <- renderValueBox({
    aleta <- round(mean(df_pinguinos$longitud_aleta_mm), 3)
    valueBox(aleta, "Prom. Longitud Aleta (mm)", icon = icon("ruler-horizontal"), color = "red")
  })
  
  output$plot_especie <- renderPlot({
    ggplot(df_pinguinos, aes(x = especie, fill = especie)) +
      geom_bar() +
      labs(x = "Especie", y = "Cantidad", title = "Distribución por especie") +
      theme_minimal()
  })
  output$plot_relacion <- renderPlot({
    datos_filtrados <- df_pinguinos %>% filter(especie == input$especie_sel)
    ggplot(datos_filtrados, aes(x = longitud_pico_mm, y = masa_corporal_g)) +
      geom_point(color = "darkblue") +
      labs(x = "Longitud del Pico (mm)", y = "Masa Corporal (g)",
           title = paste("Relación Pico vs Masa -", input$especie_sel)) +
      theme_light()
  })
}

shinyApp(ui, server) 