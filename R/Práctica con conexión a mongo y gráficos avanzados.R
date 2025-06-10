# Instalar paquete si no lo tienes
#install.packages("mongolite")
#install.packages("ggthemes")
#install.packages("plotly")
#install.packages("viridis")
#install.packages("ggplot2")
#install.packages("lubridate")
# install.packages("treemapify")
# install.packages("forcats")



# 0. Conjunto de paquetes a utilziar
library(mongolite)    # Para conexión NoSQL (MongoDB)
library(dplyr)        # Manipulación de datos
library(tidyr)        # Transformación de datos
library(ggplot2)      # Visualización avanzada
library(ggthemes)     # Temas elegantes para ggplot2
library(viridis)      # Paleta de colores moderna
library(plotly)       # Gráficos interactivos
library(lubridate)
library(treemapify)
library(forcats)

# 1. Conectar a la colección 'restaurants'
conexion_restaurants <- mongo(collection = "restaurants",
                         db = "sample_restaurants",
                         url = "mongodb+srv://username:password@cluster0.zou4pc5.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")

# Obtener dataframes
df_restaurants <- conexion_restaurants$find()

View(df_restaurants)

#df_movies_pp <- mongo(collection = "movies", db = "sample_mflix", url = url = "mongodb+srv://username:password@cluster0.cfxy7r9.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")$find()
#View(df_movies_pp)

#df_movies_2 <- mongo(collection = "movies", db = "sample_mflix", url = url = "mongodb+srv://username:password@cluster0.cfxy7r9.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")$find()
#View(df_movies_2)

names(df_restaurants)


# Exploración de los datos (EDA)
# Inspección Inicial
glimpse(df_restaurants)

# 2. LIMPIEZA Y TRANSFORMACIÓN DE DATOS (VERSIÓN CON drop_na() CORREGIDO)

restaurantes_limpio <- df_restaurants %>%
  
  unnest_wider(address, names_sep = "_") %>%
  
  unnest(grades) %>%
  
  mutate(
    grades_date = as_date(date),
    grades_score = as.numeric(score),
    grades_grade = grade, 
    borough = as.factor(borough),
    cuisine = as.factor(cuisine)
  ) %>%
  
  select(
    restaurant_id, name, borough, cuisine, 
    grades_date, grades_grade, grades_score
  ) %>%
  
  drop_na(borough, cuisine, grades_score)

glimpse(restaurantes_limpio)

# --- GRÁFICO 1: TREEMAP DE COCINAS POR DISTRITO ---

treemap_data1 <- restaurantes_limpio %>%
  filter(borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx")) %>%
  
  count(borough, cuisine, name = "cantidad") %>%
  
  group_by(borough) %>%
  slice_max(order_by = cantidad, n = 15) %>%
  ungroup()

g_treemap1 <- ggplot(treemap_data1, 
                     aes(area = cantidad, fill = borough, label = cuisine, subgroup = borough)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 4) +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour = "black", fontface = "italic") +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T, size = 12) +
  scale_fill_viridis_d(option = "cividis") +
  labs(
    title = "Distribución de Tipos de Cocina por Distrito",
    subtitle = "El área de cada rectángulo es proporcional al número de restaurantes de esa cocina",
    fill = "Distrito"
  ) +
  theme(legend.position = "none")

print(g_treemap1)

# --- GRÁFICO 2: TREEMAP DE CALIFICACIÓN PROMEDIO POR COCINA ---

treemap_data2 <- restaurantes_limpio %>%
  group_by(cuisine) %>%
  summarise(
    cantidad = n(),
    puntaje_promedio = mean(grades_score, na.rm = TRUE)
  ) %>%
  filter(cantidad > 200) %>%
  ungroup()

g_treemap2 <- ggplot(treemap_data2, 
                     aes(area = cantidad, fill = puntaje_promedio, label = paste(cuisine, "\nPuntaje:", round(puntaje_promedio, 1)))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE, fontface = "bold") +
  scale_fill_viridis_c(name = "Puntaje Promedio", direction = -1) +
  labs(
    title = "Popularidad y Calidad de las Cocinas Más Comunes",
    subtitle = "El área es el número de restaurantes, el color es el puntaje promedio de inspección (menor es mejor)"
  ) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))

print(g_treemap2)

# --- GRÁFICO 3: HEATMAP DE CALIFICACIONES ---

heatmap_data <- restaurantes_limpio %>%
  filter(borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx")) %>%
  group_by(borough, cuisine) %>%
  summarise(cantidad = n(), .groups = 'drop') %>%
  group_by(borough) %>%
  slice_max(order_by = cantidad, n = 10) %>%
  ungroup() %>%
  left_join(
    restaurantes_limpio %>% group_by(borough, cuisine) %>% summarise(puntaje_promedio = mean(grades_score, na.rm = TRUE)),
    by = c("borough", "cuisine")
  )

g_heatmap <- ggplot(heatmap_data, aes(x = borough, y = cuisine, fill = puntaje_promedio)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + # El 'tile' es la celda del heatmap
  geom_text(aes(label = round(puntaje_promedio, 1)), color = "white", size = 4, fontface = "bold") +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed() + 
  labs(
    title = "Puntaje Promedio de Inspección: Distrito vs. Cocina",
    subtitle = "Comparativa de las 10 cocinas más populares en cada distrito",
    x = "Distrito",
    y = "Tipo de Cocina",
    fill = "Puntaje Promedio"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

print(g_heatmap)

# --- GRÁFICO 4: Scatter Plot Interactivo (Popularidad vs. Puntaje) ---

scatter_data <- restaurantes_limpio %>%
  group_by(cuisine) %>%
  summarise(
    cantidad_locales = n_distinct(restaurant_id),
    puntaje_promedio = mean(grades_score, na.rm = TRUE)
  ) %>%
  filter(cantidad_locales > 25)

plot_interactivo_dispersion <- plot_ly(
  data = scatter_data,
  x = ~cantidad_locales,
  y = ~puntaje_promedio,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = ~cantidad_locales,
    sizemin = 5,
    color = ~puntaje_promedio,
    colorscale = 'Viridis',
    reversescale = TRUE, 
    showscale = TRUE,
    colorbar = list(title = "Puntaje Prom.")
  ),
  hoverinfo = 'text',
  text = ~paste(
    '<b>', cuisine, '</b><br>',
    'Restaurantes:', cantidad_locales, '<br>',
    'Puntaje Promedio:', round(puntaje_promedio, 2)
  )
) %>%
  layout(
    title = "<b>Relación entre Popularidad y Puntaje Promedio de Inspección</b>",
    xaxis = list(title = "Número de Restaurantes (Popularidad)"),
    yaxis = list(title = "Puntaje Promedio de Inspección (Menor es Mejor)")
  )

plot_interactivo_dispersion

# --- GRÁFICO 5: GRÁFICO DE BURBUJAS INTERACTIVO (CORREGIDO) ---

bubble_data_fixed <- restaurantes_limpio %>%
  group_by(borough, cuisine) %>%
  summarise(
    cantidad = n(),
    puntaje_promedio = mean(grades_score),
    .groups = "drop"
  ) %>%
  filter(cantidad > 50) %>%
  mutate(
    borough = as.character(borough),
    cuisine = as.character(cuisine)
  )

plot_ly(
  data = bubble_data_fixed,
  x = ~cuisine,
  y = ~borough,
  type = 'scatter',
  mode = 'markers',
  size = ~cantidad,
  color = ~puntaje_promedio,
  colorscale = 'Viridis',
  marker = list(reversescale = TRUE),
  
  hoverinfo = 'text',
  text = ~paste(
    '<b>', cuisine, 'en', borough, '</b><br>',
    'Restaurantes:', cantidad, '<br>',
    'Puntaje Promedio:', round(puntaje_promedio, 2)
  )
) %>%
  layout(
    title = "Análisis de Cocinas por Distrito",
    xaxis = list(title = "Tipo de Cocina", tickangle = -45),
    yaxis = list(title = "Distrito"),
    coloraxis = list(colorbar = list(title = "Puntaje Promedio"))
  )

# --- GRÁFICO 6: SUNBURST INTERACTIVO (CORREGIDO) ---

data_base_sunburst <- restaurantes_limpio %>%
  filter(grades_grade %in% c("A", "B", "C"),
         borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx"))

level1_boroughs <- data_base_sunburst %>%
  group_by(borough) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  mutate(
    ids = borough,
    labels = borough,
    parents = "" 
  )

level2_cuisines <- data_base_sunburst %>%
  group_by(borough, cuisine) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  group_by(borough) %>%
  slice_max(order_by = cantidad, n = 10) %>%
  ungroup() %>%
  mutate(
    ids = paste(borough, cuisine, sep = " - "),
    labels = cuisine,
    parents = borough
  )

level3_grades <- data_base_sunburst %>%
  inner_join(select(level2_cuisines, borough, cuisine), by = c("borough", "cuisine")) %>%
  group_by(borough, cuisine, grades_grade) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  mutate(
    ids = paste(borough, cuisine, grades_grade, sep = " - "),
    labels = grades_grade,
    parents = paste(borough, cuisine, sep = " - ")
  )

sunburst_data_full <- bind_rows(level1_boroughs, level2_cuisines, level3_grades)

plot_ly(
  data = sunburst_data_full,
  ids = ~ids,
  labels = ~labels,
  parents = ~parents,
  values = ~cantidad,
  type = 'sunburst',
  branchvalues = 'total',
  hoverinfo = 'text',
  hovertemplate = paste(
    '<b>%{label}</b><br>',
    'Total Restaurantes: %{value}<br>',
    'Ruta: %{id}',
    '<extra></extra>'
  )
) %>%
  layout(
    title = "Jerarquía Interactiva de Calificaciones: Distrito -> Cocina -> Calificación"
  )
