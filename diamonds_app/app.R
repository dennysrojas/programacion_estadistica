# ==============================================================================
#
#                               SHINY DASHBOARD - ANÁLISIS DE DIAMANTES
#
# ==============================================================================
#
#   Autor:      Dennys Rojas
#   Fecha:      15 de junio de 2025
#   Versión:    1.0
#
#   Descripción:
#   Esta aplicación web interactiva, construida con Shiny y Shinydashboard,
#   permite el análisis y la visualización del conjunto de datos 'diamonds'
#   de la librería ggplot2. La aplicación está dividida en tres secciones
#   principales: un resumen de datos con métricas clave, un panel de
#   visualización con gráficos estáticos y dinámicos, y una sección de
#   exploración personalizada donde el usuario puede generar sus propios
#   gráficos.
#
# ==============================================================================


# ------------------------------------------------------------------------------
#   1. CARGA DE LIBRERÍAS Y DATOS
# ------------------------------------------------------------------------------

# Carga de las librerías
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)

# Carga del conjunto de datos 'diamonds'
data("diamonds")

# Se definen vectores con los nombres de las columnas numéricas y categóricas.
numeric_cols <- names(diamonds)[sapply(diamonds, is.numeric)]
categorical_cols <- names(diamonds)[sapply(diamonds, is.factor)]


# ------------------------------------------------------------------------------
#   2. DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI)
# ------------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  
  # Barra lateral de navegación.
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Resumen de Datos",
      tabName = "resumen",
      icon = icon("table")
    ),
    menuItem(
      "Análisis Gráfico",
      tabName = "visualizacion",
      icon = icon("chart-pie")
    ),
    menuItem(
      "Exploración Personalizada",
      tabName = "analisis",
      icon = icon("search")
    )
  )),
  
  # Cuerpo principal del Dashboard
  dashboardBody(tabItems(
    # --- Pestaña 1: Resumen de Datos ---
    tabItem(
      tabName = "resumen",
      
      fluidRow(
        valueBoxOutput("totalDiamantes"),
        valueBoxOutput("precioPromedio"),
        valueBoxOutput("quilatesTotales")
      ),
      fluidRow(
        valueBoxOutput("mediaProfundidad"),
        valueBoxOutput("mediaMesa"),
        valueBoxOutput("precioMaximo")
      ),
      fluidRow(
        box(
          title = "Dataset Completo de Diamantes",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DT::dataTableOutput("tablaDatos")
        )
      )
    ),
    
    # --- Pestaña 2: Análisis Gráfico ---
    tabItem(tabName = "visualizacion", fluidRow(
      box(
        title = "Gráfico Estático: Calidad del Corte",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput("staticCutPlot")
      )
    ), fluidRow(
      column(
        width = 6,
        box(
          title = "Gráfico Dinámico 1: Histograma de Precios",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          sliderInput(
            "bins",
            "Número de divisiones (bins):",
            min = 1,
            max = 100,
            value = 30
          ),
          plotOutput("distPlot")
        )
      ), column(
        width = 6,
        box(
          title = "Gráfico Dinámico 2: Precio según dimensiones físicas de los diamantes",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          sliderInput(
            "sampleSize",
            "Número de puntos a graficar:",
            min = 500,
            max = 20000,
            value = 2000,
            step = 500
          ),
          plotlyOutput("plot3D")
        )
      )
    )),
    
    # --- Pestaña 3: Exploración Personalizada ---
    tabItem(tabName = "analisis", fluidRow(
      column(
        width = 6,
        box(
          title = "Gráfico Personalizado 1",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          
          selectInput(
            "plot_type_A",
            "Paso 1: Seleccione el tipo de gráfico",
            choices = c(
              "Histograma",
              "Gráfico de Dispersión",
              "Gráfico de Cajas (Boxplot)"
            )
          ),
          
          uiOutput("plot_controls_A"),
          
          plotOutput("custom_plot_A")
        )
      ), column(
        width = 6,
        box(
          title = "Gráfico Personalizado 2",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          selectInput(
            "plot_type_B",
            "Paso 1: Seleccione el tipo de gráfico",
            choices = c(
              "Gráfico de Dispersión",
              "Histograma",
              "Gráfico de Cajas (Boxplot)"
            ),
            selected = "Gráfico de Dispersión"
          ),
          uiOutput("plot_controls_B"),
          plotOutput("custom_plot_B")
        )
      )
    ))
  ))
)





# ------------------------------------------------------------------------------
#   3. DEFINICIÓN DE LA LÓGICA DEL SERVIDOR
# ------------------------------------------------------------------------------

server <- function(input, output) {
  # --- Lógica Pestaña 1: Resumen ---
  output$totalDiamantes <- renderValueBox({
    valueBox(
      formatC(nrow(diamonds), format = "d", big.mark = ","),
      "Total de Diamantes",
      icon = icon("gem"),
      color = "purple"
    )
  })
  output$precioPromedio <- renderValueBox({
    avg_price <- round(mean(diamonds$price), 2)
    valueBox(
      paste0(
        "$",
        formatC(
          avg_price,
          format = "f",
          digits = 2,
          big.mark = ","
        )
      ),
      "Precio Promedio",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$quilatesTotales <- renderValueBox({
    total_carat <- round(sum(diamonds$carat), 0)
    valueBox(
      formatC(total_carat, format = "d", big.mark = ","),
      "Suma de Quilates",
      icon = icon("weight-hanging"),
      color = "blue"
    )
  })
  output$mediaProfundidad <- renderValueBox({
    avg_depth <- round(mean(diamonds$depth), 2)
    valueBox(
      paste0(avg_depth, "%"),
      "Profundidad Promedio",
      icon = icon("ruler-vertical"),
      color = "yellow"
    )
  })
  output$mediaMesa <- renderValueBox({
    avg_table <- round(mean(diamonds$table), 2)
    valueBox(
      avg_table,
      "Tamaño Promedio de Mesa",
      icon = icon("ruler-horizontal"),
      color = "red"
    )
  })
  output$precioMaximo <- renderValueBox({
    max_price <- max(diamonds$price)
    valueBox(
      paste0("$", formatC(
        max_price, format = "d", big.mark = ","
      )),
      "Precio Máximo",
      icon = icon("arrow-up"),
      color = "aqua"
    )
  })
  
  output$tablaDatos <- DT::renderDataTable({
    DT::datatable(
      diamonds,
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE
    )
  })
  
  # --- Lógica Pestaña 2: Análisis Gráfico ---
  output$staticCutPlot <- renderPlot({
    ggplot(diamonds, aes(x = cut, fill = cut)) + geom_bar() + geom_text(stat =
                                                                          'count',
                                                                        aes(label = ..count..),
                                                                        vjust = -0.5) + labs(title = "Distribución por Calidad de Corte", x = "Calidad del Corte", y = "Cantidad") + theme_minimal() + theme(legend.position = "none",
                                                                                                                                                                                                             plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$distPlot <- renderPlot({
    ggplot(diamonds, aes(x = price)) + geom_histogram(
      bins = input$bins,
      fill = "#007bc2",
      color = "white"
    ) + labs(x = "Precio (USD)", y = "Frecuencia") + theme_minimal()
  })
  
  sampled_data <- reactive({
    diamonds[sample(nrow(diamonds), size = input$sampleSize), ]
  })
  
  output$plot3D <- renderPlotly({
    plot_ly(
      data = sampled_data(),
      x = ~ x,
      y = ~ y,
      z = ~ z,
      type = "scatter3d",
      mode = "markers",
      color = ~ price,
      marker = list(
        size = 3,
        opacity = 0.7,
        colorscale = 'Viridis'
      )
    ) %>%
      layout(title = "Dimensiones Físicas de los Diamantes",
             scene = list(
               xaxis = list(title = "Dimensión X (mm)"),
               yaxis = list(title = "Dimensión Y (mm)"),
               zaxis = list(title = "Dimensión Z (mm)")
             ))
  })
  
  # --- Lógica Pestaña 3: Exploración Personalizada ---
  # 1. Generación de Controles Dinámicos (UI)
  output$plot_controls_A <- renderUI({
    req(input$plot_type_A)
    
    switch(
      input$plot_type_A,
      "Histograma" = tagList(
        hr(),
        p("Paso 2: Configure las opciones"),
        selectInput(
          "hist_var_A",
          "Variable:",
          choices = numeric_cols,
          selected = "price"
        ),
        sliderInput(
          "hist_bins_A",
          "Divisiones (bins):",
          min = 1,
          max = 100,
          value = 30
        )
      ),
      "Gráfico de Dispersión" = tagList(
        hr(),
        p("Paso 2: Seleccione las variables"),
        selectInput(
          "scatter_x_A",
          "Variable X:",
          choices = numeric_cols,
          selected = "carat"
        ),
        selectInput(
          "scatter_y_A",
          "Variable Y:",
          choices = numeric_cols,
          selected = "price"
        )
      ),
      "Gráfico de Cajas (Boxplot)" = tagList(
        hr(),
        p("Paso 2: Seleccione las variables"),
        selectInput(
          "box_cat_A",
          "Variable Categórica (X):",
          choices = categorical_cols,
          selected = "cut"
        ),
        selectInput(
          "box_num_A",
          "Variable Numérica (Y):",
          choices = numeric_cols,
          selected = "price"
        )
      )
    )
  })
  
  # 2. Generación del Gráfico Personalizado
  output$custom_plot_A <- renderPlot({
    req(input$plot_type_A)
    
    p <- switch(
      input$plot_type_A,
      "Histograma" = {
        req(input$hist_var_A)
        ggplot(diamonds, aes_string(x = input$hist_var_A)) +
          geom_histogram(
            bins = input$hist_bins_A,
            fill = "darkgreen",
            color = "white"
          ) +
          labs(
            title = paste("Histograma de", input$hist_var_A),
            x = input$hist_var_A
          )
      },
      "Gráfico de Dispersión" = {
        req(input$scatter_x_A, input$scatter_y_A)
        ggplot(diamonds,
               aes_string(x = input$scatter_x_A, y = input$scatter_y_A)) +
          geom_point(alpha = 0.5, color = "darkgreen") +
          labs(title = paste(
            "Dispersión:",
            input$scatter_x_A,
            "vs",
            input$scatter_y_A
          ))
      },
      "Gráfico de Cajas (Boxplot)" = {
        req(input$box_cat_A, input$box_num_A)
        ggplot(diamonds,
               aes_string(x = input$box_cat_A, y = input$box_num_A)) +
          geom_boxplot(aes_string(fill = input$box_cat_A)) +
          labs(title = paste("Boxplot de", input$box_num_A, "por", input$box_cat_A)) +
          theme(legend.position = "none")
      }
    )
    p + theme_minimal() + theme(plot.title = element_text(size = 16, face =
                                                            "bold"))
  })
  
  # -- Lógica para el Gráfico B (idéntica a la A, pero con sufijos _B para independencia) --
  output$plot_controls_B <- renderUI({
    req(input$plot_type_B)
    switch(
      input$plot_type_B,
      "Histograma" = tagList(
        hr(),
        p("Paso 2: Configure las opciones"),
        selectInput(
          "hist_var_B",
          "Variable:",
          choices = numeric_cols,
          selected = "depth"
        ),
        sliderInput(
          "hist_bins_B",
          "Divisiones (bins):",
          min = 1,
          max = 100,
          value = 30
        )
      ),
      "Gráfico de Dispersión" = tagList(
        hr(),
        p("Paso 2: Seleccione las variables"),
        selectInput(
          "scatter_x_B",
          "Variable X:",
          choices = numeric_cols,
          selected = "price"
        ),
        selectInput(
          "scatter_y_B",
          "Variable Y:",
          choices = numeric_cols,
          selected = "table"
        )
      ),
      "Gráfico de Cajas (Boxplot)" = tagList(
        hr(),
        p("Paso 2: Seleccione las variables"),
        selectInput(
          "box_cat_B",
          "Variable Categórica (X):",
          choices = categorical_cols,
          selected = "color"
        ),
        selectInput(
          "box_num_B",
          "Variable Numérica (Y):",
          choices = numeric_cols,
          selected = "price"
        )
      )
    )
  })
  output$custom_plot_B <- renderPlot({
    req(input$plot_type_B)
    p <- switch(
      input$plot_type_B,
      "Histograma" = {
        req(input$hist_var_B)
        ggplot(diamonds, aes_string(x = input$hist_var_B)) +
          geom_histogram(
            bins = input$hist_bins_B,
            fill = "navy",
            color = "white"
          ) +
          labs(
            title = paste("Histograma de", input$hist_var_B),
            x = input$hist_var_B
          )
      },
      "Gráfico de Dispersión" = {
        req(input$scatter_x_B, input$scatter_y_B)
        ggplot(diamonds,
               aes_string(x = input$scatter_x_B, y = input$scatter_y_B)) +
          geom_point(alpha = 0.5, color = "navy") +
          labs(title = paste(
            "Dispersión:",
            input$scatter_x_B,
            "vs",
            input$scatter_y_B
          ))
      },
      "Gráfico de Cajas (Boxplot)" = {
        req(input$box_cat_B, input$box_num_B)
        ggplot(diamonds,
               aes_string(x = input$box_cat_B, y = input$box_num_B)) +
          geom_boxplot(aes_string(fill = input$box_cat_B)) +
          labs(title = paste("Boxplot de", input$box_num_B, "por", input$box_cat_B)) +
          theme(legend.position = "none")
      }
    )
    p + theme_minimal() + theme(plot.title = element_text(size = 16, face =
                                                            "bold"))
  })
}


# ------------------------------------------------------------------------------
#   4. EJECUCIÓN DE LA APLICACIÓN
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)