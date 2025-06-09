# Instalar paquete si no lo tienes
#install.packages("mongolite")
#install.packages("ggthemes")
#install.packages("plotly")
#install.packages("viridis")
#install.packages("ggplot2")



# 0. Conjunto de paquetes a utilziar
library(mongolite)    # Para conexión NoSQL (MongoDB)
library(dplyr)        # Manipulación de datos
library(tidyr)        # Transformación de datos
library(ggplot2)      # Visualización avanzada
library(ggthemes)     # Temas elegantes para ggplot2
library(viridis)      # Paleta de colores moderna
library(plotly)       # Gráficos interactivos


# 1. Conectar a la colección 'movies'
conexion_movies <- mongo(collection = "movies",
                         db = "sample_mflix",
                         url = "mongodb+srv://dennysrojas:Dennys1101@cluster0.cfxy7r9.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")

# Conectar a la colección 'comments'
conexion_comments <- mongo(collection = "comments",
                           db = "sample_mflix",
                           url = "mongodb+srv://dennysrojas:Dennys1101@cluster0.cfxy7r9.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")

# Obtener dataframes
df_movies <- conexion_movies$find()
df_comments <- conexion_comments$find()

View(df_movies)
View(df_comments)

#df_movies_pp <- mongo(collection = "movies", db = "sample_mflix", url = url = "mongodb+srv://dennysrojas:Dennys1101@cluster0.cfxy7r9.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")$find()
#View(df_movies_pp)

#df_movies_2 <- mongo(collection = "movies", db = "sample_mflix", url = url = "mongodb+srv://dennysrojas:Dennys1101@cluster0.cfxy7r9.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0")$find()
#View(df_movies_2)

names(df_movies)


# 2.Exploración de los datos (EDA)
# Inspección Inicial
glimpse(df_movies)

unique(df_movies$genres)
df_movies %>% distinct(genres)

# Limpieza y Transformación
# La columna 'genres' contiene múltiples géneros separados por comas.
# Para un análisis correcto, debemos "desanidar" (unnest) cada género en su propia fila.
# Separamos la columna 'genres' en múltiples filas
# Quitamos filas donde el género o el año son NA
# Nos aseguramos que el año sea numérico
# Filtramos géneros ambiguos o no informativos
movies_limpio <- df_movies %>%
  separate_rows(genres, sep = ", ") %>%
  drop_na(genres, year) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!genres %in% c("Short", "Adult", "News", "Reality-TV", "Talk-Show"))

# Verifiquemos el resultado.
glimpse(movies_limpio)


#APLANAR LAS COLUMNAS ANIDADAS ---
# Partimos de tu dataframe original (movies_limpio)
# Usamos unnest_wider para "abrir" las cajas 'imdb' y 'awards'
movies_aplanado <- movies_limpio %>%
  unnest_wider(imdb, names_sep = "_") %>%    # Esto creará imdb_rating, imdb_votes, etc.
  unnest_wider(awards, names_sep = "_")  # Esto creará awards_wins, awards_nominations

# Verifiquemos el resultado. Ahora deberías ver las nuevas columnas.
View(glimpse(movies_aplanado))


# nueva limpieza (usando el dataframe aplanado) ---
# Ahora que tenemos las columnas correctas, podemos limpiar
movies_limpio_2 <- movies_aplanado %>%
  # Renombramos la columna que nos interesa a un nombre más corto
  rename(rating = imdb_rating) %>%
  # El resto de la limpieza que ya habíamos planeado
  separate_rows(genres, sep = ", ") %>%
  drop_na(genres, year, rating) %>%
  mutate(
    year = as.numeric(year),
    rating = as.numeric(rating)
  ) %>%
  filter(!genres %in% c("Short", "Adult", "News", "Reality-TV", "Talk-Show"))

View(glimpse(movies_limpio_2))

#¿Cómo se distribuyen las calificaciones y la duración de las películas?

# Gráfico 1: Distribución de Calificaciones de IMDb
p1 <- ggplot(movies_limpio_2, aes(x = rating)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  geom_vline(aes(xintercept = mean(rating, na.rm = TRUE)), color = "blue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 7.5, y = 0.5, label = "Media", color = "blue", fontface = "italic") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  labs(title = "Distribución de Calificaciones IMDb", x = "Calificación (0-10)", y = "Densidad") +
  theme_minimal()

# Imprimimos el gráfico
print(p1)

#Interpretación:
#La mayoría de las películas en nuestra base de datos son percibidas 
#como 'decentes' o 'buenas', con calificaciones que se agrupan por 
#encima del promedio. Es relativamente raro encontrar películas 
#consideradas extremadamente malas o excelentes."


# Asegurémonos que la columna de votos existe y es numérica
movies_limpio <- movies_limpio_2 %>%
  rename(votos = imdb_votes) %>%
  mutate(votos = as.numeric(votos)) %>%
  drop_na(votos)

#¿Existe una relación entre los votos (imdb_votes) y la calificación (rating)?
# Creamos un gráfico de dispersión (scatter plot)
p_relacion <- ggplot(movies_limpio, aes(x = votos, y = rating)) +
  geom_point(alpha = 0.1, color = "skyblue") + # alpha bajo para ver la densidad
  scale_x_log10(labels = scales::comma) + # Eje X en escala logarítmica para manejar la gran dispersión de votos
  labs(
    title = "Relación entre Votos y Calificación IMDb",
    x = "Número de Votos (Escala Log)",
    y = "Calificación (0-10)",
    caption = "Se aplica una escala logarítmica en el eje X para una mejor visualización."
  ) +
  theme_minimal()

print(p_relacion)

#Interpretación
#Para que una película acumule una gran cantidad de votos, 
#tiende a ser, como mínimo, aceptable para una audiencia masiva. 
#Por lo tanto, un alto número de votos no garantiza la excelencia, 
#pero sí parece ser un filtro contra películas muy malas.

#¿Que generos son más llamatvos según la clasificación?
#Instalación nuevos paquetes
#install.packages("treemapify")
library(treemapify)

#  Preparamos los datos
treemap_data <- movies_limpio %>%
  # Nos aseguramos que 'rated' y 'genres' no sean NA
  drop_na(rated, genres) %>%
  # Filtramos clasificaciones poco comunes para un gráfico más limpio
  filter(rated %in% c("R", "PG-13", "PG", "G", "Not Rated")) %>%
  # Contamos las combinaciones
  count(rated, genres, name = "conteo") %>%
  # Nos quedamos con los géneros más importantes para la visualización
  group_by(rated) %>%
  slice_max(order_by = conteo, n = 10) %>%
  ungroup()

#  Construimos el gráfico
ggplot(treemap_data, aes(area = conteo, fill = rated, label = genres, subgroup = rated)) +
  # Geometría principal del treemap
  geom_treemap() +
  
  # Geometría para los bordes de los subgrupos (cada clasificación)
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  
  # Geometría para el texto de los subgrupos (el nombre de la clasificación)
  geom_treemap_subgroup_text(
    place = "centre",
    grow = TRUE,
    alpha = 0.5,
    colour = "black",
    fontface = "italic",
    min.size = 0
  ) +
  
  # Geometría para el texto de cada recuadro (el nombre del género)
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    reflow = TRUE # El texto se ajusta al recuadro
  ) +
  
  # Personalización de la paleta de colores y las etiquetas
  scale_fill_viridis_d(option = "rocket", name = "Clasificación") +
  labs(
    title = "Composición de Géneros Dentro de Cada Clasificación de Edad",
    subtitle = "El área de cada rectángulo es proporcional al número de películas de ese género",
    caption = "Datos: sample_mflix de MongoDB"
  ) +
  theme(legend.position = "bottom")

library(stringr)
#¿Existe alguna relación oculta entre la calificación (rating), 
#la popularidad (número de votos) y la duración (runtime) de una película?

# Preparamos los datos
# Renombramos 'imdb_votes' a 'votos' y lo convertimos a numérico
# Asegurándonos de que estas columnas están en movies_limpio desde el paso de aplanado.
# Si no lo están, ejecuta el código de aplanado de la respuesta anterior.

movies_3d_data <- movies_limpio_2 %>%
  rename(votos = imdb_votes) %>% # Asumiendo que esta columna existe tras aplanar
  mutate(votos = as.numeric(votos),
         runtime = as.numeric(runtime)) %>%
  drop_na(rating, votos, runtime, genres) %>%
  # Extraemos el primer género como "género principal"
  mutate(genero_principal = str_extract(genres, "^[^,]+")) %>%
  # Tomamos una muestra para que el gráfico sea más ligero
  slice_sample(n = 5000)

# Construimos el gráfico 3D interactivo
plot_ly(
  data = movies_3d_data,
  x = ~rating,
  y = ~log10(votos), # Usamos log10 para manejar la gran escala de votos
  z = ~runtime,
  color = ~genero_principal, # Color por el género principal
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7),
  # Personalizamos el texto que aparece al pasar el cursor
  hoverinfo = 'text',
  text = ~paste(
    '<b>', title, '</b><br>',
    'Rating:', rating, '<br>',
    'Votos:', format(votos, big.mark = ","), '<br>',
    'Duración:', runtime, 'min'
  )
) %>%
  layout(
    title = "Relación 3D Interactiva: Calificación, Votos y Duración",
    scene = list(
      xaxis = list(title = "Calificación IMDb (0-10)"),
      yaxis = list(title = "Log(Número de Votos)"),
      zaxis = list(title = "Duración (minutos)")
    )
  )

#¿Cómo se distribuyen los géneros dentro de cada clasificación 
#de edad, y cuáles son las películas mejor calificadas dentro 
#de cada uno de esos nichos?


# Preparación de Datos Jerárquicos 
# Es un proceso de varios pasos para construir la estructura que plotly necesita.

# Primero, filtramos y preparamos los datos base
data_base <- movies_limpio_2 %>%
  rename(votos = imdb_votes) %>%
  mutate(votos = as.numeric(votos), rated = as.factor(rated)) %>%
  drop_na(rated, genres, votos, rating) %>%
  filter(rated %in% c("R", "PG-13", "PG", "G"), votos > 1000)

# Nivel 1: Las clasificaciones de edad (los padres principales)
level1_ratings <- data_base %>%
  group_by(rated) %>%
  summarise(
    total_votos = sum(votos),
    rating_medio = mean(rating)
  ) %>%
  mutate(
    ids = rated,
    labels = rated,
    parents = "Películas" # Todos son hijos de una raíz común
  )

# Nivel 2: Los géneros dentro de cada clasificación
level2_genres <- data_base %>%
  group_by(rated, genres) %>%
  summarise(
    total_votos = sum(votos),
    rating_medio = mean(rating)
  ) %>%
  mutate(
    ids = paste(rated, genres, sep = " - "), # ID único: "R - Drama"
    labels = genres,
    parents = rated
  )

# Nivel 3: Las 3 mejores películas por cada género/clasificación
level3_movies <- data_base %>%
  group_by(rated, genres) %>%
  slice_max(order_by = rating, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    ids = paste(rated, genres, title, sep = " - "), # ID único
    labels = title,
    parents = paste(rated, genres, sep = " - ")
  ) %>%
  rename(total_votos = votos, rating_medio = rating)


# Creamos el nodo raíz
root_node <- tibble(
  ids = "Películas",
  labels = "Películas",
  parents = "",
  total_votos = sum(level1_ratings$total_votos),
  rating_medio = mean(data_base$rating)
)

# Unimos todos los niveles en un solo dataframe
sunburst_data <- bind_rows(root_node, level1_ratings, level2_genres, level3_movies)

# Construcción del Gráfico Sunburst 
plot_ly(
  data = sunburst_data,
  type = 'sunburst',
  ids = ~ids,
  labels = ~labels,
  parents = ~parents,
  values = ~total_votos, # El tamaño del sector se basa en los votos
  branchvalues = 'total', # El tamaño del padre es la suma de sus hijos
  
  # El color se basa en la calificación media
  marker = list(
    colors = ~rating_medio,
    colorscale = 'Viridis', # Una paleta de colores perceptualmente agradable
    colorbar = list(title = "Rating Promedio")
  ),
  
  # Personalizamos el texto que aparece al pasar el cursor
  hovertemplate = paste(
    '<b>%{label}</b><br>',
    'Rating Promedio: %{color:.2f}<br>',
    'Votos Totales: %{value:,.0f}<br>',
    'Parent: %{parent}<extra></extra>' # <extra></extra> oculta info extra de plotly
  )
) %>%
  layout(
    title = list(text = "<b>Jerarquía Interactiva del Universo Cinematográfico</b>", font = list(size = 20))
  )
