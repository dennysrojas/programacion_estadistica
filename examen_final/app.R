## -----------------------------------------------------------------------------
## PROYECTO: Dashboard Interactivo de Análisis del Dataset "Boston"
## AUTOR:    Dennys Rojas Llangarí
## FECHA:    17 de junio de 2025
## VERSIÓN:  1.0
##
## DESCRIPCIÓN:
## Esta aplicación web, desarrollada con R y los paquetes Shiny y shinydashboard,
## ofrece un completo entorno para el análisis del dataset "Boston Housing".
## La aplicación está estructurada en tres secciones principales:
##   1. Una herramienta de predicción de valores de vivienda usando modelos
##      de Regresión Lineal y Random Forest.
##   2. Un panel de análisis estructural que detalla las dimensiones, tipos de
##      datos y valores faltantes del dataset.
##   3. Una galería de visualizaciones avanzadas e interactivas (3D, histogramas,
##      dispersión) con un enlace a un análisis externo.
## -----------------------------------------------------------------------------


#== SECCIÓN 1: CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS =======================

# install.packages(c("shiny", "shinydashboard", "MASS", "ggplot2", "dplyr", "DT", "randomForest", "plotly"))

library(shiny)
library(shinydashboard)
library(MASS)
library(ggplot2)
library(dplyr)
library(DT)
library(randomForest)
library(plotly)

# Carga del dataset `Boston` en el entorno de la aplicación.
data(Boston)


#== SECCIÓN 2: ENTRENAMIENTO DE MODELOS DE MACHINE LEARNING =====================

formula_modelo <- medv ~ lstat + rm + ptratio + nox + dis

set.seed(123)

# 2.1. Entrenamiento del Modelo de Regresión Lineal Múltiple (`lm`).
modelo_lm <- lm(formula_modelo, data = Boston)

# 2.2. Entrenamiento del Modelo de Random Forest.
modelo_rf <- randomForest(formula_modelo, data = Boston, ntree = 500)


#== SECCIÓN 3: DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI) =======================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Examen Final"),
  
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    
    menuItem("Predicción", tabName = "prediction_tab", icon = icon("brain")),
    menuItem(
      "Estructura de la tabla",
      tabName = "structure_tab",
      icon = icon("table-list")
    ),
    menuItem("Gráficos", tabName = "plots_tab", icon = icon("chart-pie"))
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "prediction_tab", fluidRow(
      box(
        title = "Valores de las Variables Independientes",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        p(
          "Ingrese los valores para las variables y presione un botón para predecir."
        ),
        numericInput(
          "lstat_input",
          "lstat:",
          value = round(mean(Boston$lstat), 1),
          min = 0
        ),
        numericInput("rm_input", "rm:", value = round(mean(Boston$rm), 1), min = 0),
        numericInput(
          "ptratio_input",
          "ptratio:",
          value = round(mean(Boston$ptratio), 1),
          min = 0
        ),
        numericInput(
          "nox_input",
          "nox:",
          value = round(mean(Boston$nox), 2),
          min = 0,
          step = 0.01
        ),
        numericInput(
          "dis_input",
          "dis:",
          value = round(mean(Boston$dis), 1),
          min = 0
        ),
        hr(),
        
        actionButton(
          "predict_lm_btn",
          "Regresión Lineal",
          icon = icon("chart-line"),
          class = "btn-info"
        ),
        actionButton(
          "predict_rf_btn",
          "Random Forest",
          icon = icon("tree"),
          class = "btn-success"
        )
      ),
      
      
      box(
        title = "Resultados de la Predicción",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        h4("Predicción con Regresión Lineal:"),
        # `verbatimTextOutput` es un placeholder para el texto de resultado.
        verbatimTextOutput("lm_prediction_output"),
        hr(),
        h4("Predicción con Random Forest:"),
        verbatimTextOutput("rf_prediction_output")
      )
    )),
    
    # Pestaña 2: Análisis Estructural
    tabItem(
      tabName = "structure_tab",
      fluidRow(
        valueBoxOutput("num_observaciones", width = 6),
        valueBoxOutput("num_variables", width = 6)
      ),
      fluidRow(
        box(
          title = "Resumen del Dataset por Variable",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput("structure_summary_table")
        )
      ),
      fluidRow(
        box(
          title = "Vista del Dataset",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          dataTableOutput("full_data_table")
        )
      )
    ),
    
    # Pestaña 3: Análisis Gráfico
    tabItem(
      tabName = "plots_tab",
      fluidRow(
        box(
          title = "Descripción y análisis de los Gráficos",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          p(
            "Para una descripción detallada y el análisis de los resultados de cada gráfico, por favor visite el siguiente documento:"
          ),
          tags$a(
            "Ver Análisis Detallado",
            href = "https://dennysrojas.github.io/examen_final_PROESTCCD_dfrl/",
            target = "_blank",
            # `target="_blank"` abre el enlace en una nueva pestaña.
            class = "btn btn-primary"
          )
        )
      ),
      fluidRow(
        box(
          title = "Dispersión 3D: LSTAT, RM y MEDV",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("scatter3d")
        )
      ),
      fluidRow(
        box(
          title = "Histograma y Densidad de la variable LSTAT",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          sliderInput(
            inputId = "bins_slider",
            label = "Ajustar número de barras",
            min = 5,
            max = 50,
            value = 30,
            step = 1
          ),
          plotlyOutput("histogram_lstat")
        ),
        box(
          title = "Dispersión 2D: LSTAT vs. Valor de Vivienda (MEDV)",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput("scatter2d_lstat_medv")
        ),
      )
    )
  ))
)


#== SECCIÓN 4: LÓGICA DEL SERVIDOR (Server) =====================================

server <- function(input, output) {
  lm_prediction_value <- eventReactive(input$predict_lm_btn, {
    new_data <- data.frame(
      lstat = input$lstat_input,
      rm = input$rm_input,
      ptratio = input$ptratio_input,
      nox = input$nox_input,
      dis = input$dis_input
    )
    
    prediction <- predict(modelo_lm, newdata = new_data)
    
    paste0("Valor de Vivienda Predicho: $",
           format(round(prediction * 1000, 0), big.mark = ","))
  })
  
  output$lm_prediction_output <- renderText({
    lm_prediction_value()
  })
  
  rf_prediction_value <- eventReactive(input$predict_rf_btn, {
    new_data <- data.frame(
      lstat = input$lstat_input,
      rm = input$rm_input,
      ptratio = input$ptratio_input,
      nox = input$nox_input,
      dis = input$dis_input
    )
    prediction <- predict(modelo_rf, newdata = new_data)
    paste0("Valor de Vivienda Predicho: $",
           format(round(prediction * 1000, 0), big.mark = ","))
  })
  output$rf_prediction_output <- renderText({
    rf_prediction_value()
  })
  
  
  output$num_observaciones <- renderValueBox({
    valueBox(
      value = nrow(Boston),
      subtitle = "Número de Registros (Filas)",
      icon = icon("ruler-horizontal"),
      color = "light-blue"
    )
  })
  output$num_variables <- renderValueBox({
    valueBox(
      value = ncol(Boston),
      subtitle = "Número de Variables (Columnas)",
      icon = icon("ruler-vertical"),
      color = "light-blue"
    )
  })
  
  output$structure_summary_table <- renderDataTable({
    resumen <- data.frame(
      Variable = names(Boston),
      Tipo_de_Dato = sapply(Boston, class),
      Valores_Faltantes_NA = sapply(Boston, function(x)
        sum(is.na(x)))
    )
    rownames(resumen) <- NULL
    datatable(resumen,
              options = list(pageLength = 15, scrollX = TRUE),
              class = 'cell-border stripe')
  })
  
  
  output$full_data_table <- renderDataTable({
    datatable(Boston,
              options = list(pageLength = 10, scrollX = TRUE),
              class = 'cell-border stripe')
  })
  
  
  # Gráfico 1: Dispersión 3D.
  output$scatter3d <- renderPlotly({
    plot_ly(
      Boston,
      x = ~ lstat,
      y = ~ rm,
      z = ~ medv,
      type = 'scatter3d',
      mode = 'markers',
      marker = list(
        color = ~ medv,
        colorscale = 'Viridis',
        showscale = TRUE,
        size = 4
      )
    ) %>%
      layout(title = "Relación entre LSTAT, RM y MEDV",
             scene = list(
               xaxis = list(title = 'LSTAT'),
               yaxis = list(title = 'RM'),
               zaxis = list(title = 'MEDV')
             ))
  })
  
  # Gráfico 2: Histograma con curva de densidad.
  output$histogram_lstat <- renderPlotly({
    p <- ggplot(Boston, aes(x = lstat)) +
      # El argumento `bins` ahora es dinámico y depende del valor del `sliderInput`.
      geom_histogram(
        aes(y = ..density..),
        bins = input$bins_slider,
        fill = "dodgerblue",
        alpha = 0.7
      ) +
      geom_density(color = "red", size = 1) +
      labs(title = "Distribución de la Variable LSTAT", x = "% de Población con Bajo Estatus (lstat)", y = "Densidad") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Gráfico 3: Dispersión 2D con categorías y línea de suavizado.
  output$scatter2d_lstat_medv <- renderPlotly({
    p <- ggplot(Boston, aes(x = lstat, y = medv)) +
      
      geom_point(aes(color = factor(chas)), alpha = 0.6) +
      geom_smooth(method = "loess",
                  color = "blue",
                  se = FALSE) +
      scale_color_manual(
        name = "Colinda con Río Charles",
        labels = c("No", "Sí"),
        values = c("0" = "orangered", "1" = "turquoise3")
      ) +
      labs(title = "LSTAT vs. Valor de Vivienda (MEDV)", x = "% de Estatus Bajo (lstat)", y = "Valor Mediano de Vivienda (miles $)") +
      theme_minimal()
    ggplotly(p)
  })
}


#== SECCIÓN 5: EJECUCIÓN DE LA APLICACIÓN ========================================
shinyApp(ui = ui, server = server)