install.packages("tidyverse")
install.packages("skimr")
install.packages("maps")
install.packages("corrplot")


#Paquetes necesarios
library(tidyverse)
library(skimr)
library(maps)
library(corrplot)
#----------------#
# Carga de datos #
#----------------#

df <- read_csv("C:/Users/denny/Documents/Programación/programacion_estadistica/R/train_1.csv")
View(df)

#-------------------#
# Limpieza de datos #
#-------------------#

print("Resumen inicial de datos (skimr)")
skim(df)

# Calcular la moda para 'Electrical'
mode_electrical <- df %>%
  filter(!is.na(Electrical)) %>%
  pull(Electrical) %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  head(1)

df_limpio <- df %>%
  mutate(
    across(c(Alley, PoolQC, Fence, MiscFeature, FireplaceQu), ~replace_na(as.character(.), "None")),
    across(c(GarageType, GarageFinish, GarageQual, GarageCond), ~replace_na(as.character(.), "None")),
    across(c(BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2), ~replace_na(as.character(.), "None")),
    MasVnrType = replace_na(as.character(MasVnrType), "None")
  ) %>%
  mutate(
    across(c(MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF,
             BsmtFullBath, BsmtHalfBath, GarageCars, GarageArea,
             PoolArea, MiscVal), ~replace_na(., 0))
  ) %>%
  mutate(LotFrontage = replace_na(LotFrontage, median(LotFrontage, na.rm = TRUE))) %>%
  mutate(
    GarageYrBlt = ifelse(GarageType == "None", 0, replace_na(GarageYrBlt, median(GarageYrBlt[GarageType != "None"], na.rm = TRUE)))
  ) %>%
  mutate(Electrical = replace_na(as.character(Electrical), mode_electrical))

print("Resumen de datos después de la limpieza")
skim(df_limpio)

print("Estructura del dataset después de la limpieza")
glimpse(df_limpio)


#----------------------------------------------------------------------------------------------#
# 1. ¿Cuáles son las 3 características más correlacionadas con el precio de venta (SalePrice)? #
#----------------------------------------------------------------------------------------------#

# Seleccionar solo las columnas numéricas
df_solo_numericos <- df_limpio %>%
  select(where(is.numeric)) %>%
  select(-Id)

# Calcular la matriz de correlación
matriz_correlacion <- cor(df_solo_numericos, use = "pairwise.complete.obs")

# Obtener las correlaciones con SalePrice
cor_con_saleprice <- as.data.frame(matriz_correlacion[, "SalePrice"])
colnames(cor_con_saleprice) <- "Correlation"

# Eliminar la correlación de SalePrice consigo mismo
cor_con_saleprice <- cor_con_saleprice %>%
  filter(rownames(.) != "SalePrice")

# Ordenar por el valor absoluto de la correlación de forma descendente
top_caracteristicas_correlacion <- cor_con_saleprice %>%
  mutate(AbsCorrelation = abs(Correlation)) %>%
  arrange(desc(AbsCorrelation)) %>%
  head(3)

print("Las 3 características más correlacionadas con SalePrice son:")
View(top_caracteristicas_correlacion)

#-----------------------------------------------------------------------------------------------#
# 2 ¿Cómo afecta la calidad general de la casa (OverallQual) al precio? (Ideal para un boxplot) #
#-----------------------------------------------------------------------------------------------#

# Convertir OverallQual a factor para un mejor etiquetado en el boxplot
df_limpio_overallqual <- df_limpio %>%
  mutate(OverallQual = as.factor(OverallQual))

# Crear el boxplot
p_overallqual_saleprice <- df_limpio_overallqual %>%
  ggplot(aes(x = OverallQual, y = SalePrice, fill = OverallQual)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Impacto de la Calidad General (OverallQual) en el Precio de Venta",
       x = "Calidad General de la Casa",
       y = "Precio de Venta") +
  theme_minimal() +
  theme(legend.position = "none")

p_overallqual_saleprice

#-------------------------------------------------------------------------------------#
# 3 ¿La presencia de un garaje o una piscina impacta significativamente en el precio? #
#-------------------------------------------------------------------------------------#

#Garage
p_garagetype_saleprice <- df_limpio %>%
  ggplot(aes(x = GarageType, y = SalePrice, fill = GarageType)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  guides(fill = "none") +
  labs(title = "Impacto del Tipo de Garaje en el Precio de Venta",
       x = "Tipo de Garaje",
       y = "Precio de Venta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_garagetype_saleprice

#Piscina
df_limpio_pool <- df_limpio %>%
  mutate(HasPool = as.factor(ifelse(PoolArea > 0, "Sí", "No")))

p_pool_saleprice <- df_limpio_pool %>%
  ggplot(aes(x = HasPool, y = SalePrice, fill = HasPool)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  guides(fill = "none") +
  labs(title = "Impacto de la Presencia de Piscina en el Precio de Venta",
       x = "Tiene Piscina",
       y = "Precio de Venta") +
  theme_minimal() 

p_pool_saleprice

#--------------------------------------------------------------------#
# 4 ¿Qué barrios (Neighborhood) son los más caros y los más baratos? #
#--------------------------------------------------------------------#

neighborhood_prices <- df_limpio %>%
  group_by(Neighborhood) %>%
  summarise(
    MedianSalePrice = median(SalePrice), 
    MeanSalePrice = mean(SalePrice),
    HouseCount = n()
  ) %>%
  arrange(desc(MedianSalePrice)) # Ordenar por precio mediano descendente

message("\nBarrios más caros (por precio mediano):")
print(head(neighborhood_prices, 5))

message("\nBarrios más baratos (por precio mediano):")
print(tail(neighborhood_prices, 5))

# Visualización opcional de los precios medianos por barrio
p_neighborhood_prices <- neighborhood_prices %>%
  
  mutate(Neighborhood = fct_reorder(Neighborhood, MedianSalePrice)) %>%
  ggplot(aes(x = Neighborhood, y = MedianSalePrice)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Precio Mediano de Venta por Barrio",
       x = "Barrio",
       y = "Precio Mediano de Venta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        plot.title = element_text(hjust = 0.5)) 

p_neighborhood_prices

#------------------------------------------------------------------------------------------------------------------------------------------#
# 5 ¿Qué columnas tienen la mayor cantidad de datos faltantes y qué estrategia propones para cada una (eliminar, imputar, o re-codificar)? #
#------------------------------------------------------------------------------------------------------------------------------------------#

datos_faltantes <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "MissingCount") %>%
  filter(MissingCount > 0) %>% 
  arrange(desc(MissingCount)) 

# Crear la gráfica de barras
p_datos_faltantes <- datos_faltantes %>%
  ggplot(aes(x = reorder(Column, MissingCount), y = MissingCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Columnas con Mayor Cantidad de Datos Faltantes",
       x = "Columna",
       y = "Cantidad de Datos Faltantes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

p_datos_faltantes

#------------------------------------------------------------------------------------------------#
# 6 Identifica y visualiza los outliers en el área habitable (GrLivArea). ¿Qué harías con ellos? #
#------------------------------------------------------------------------------------------------#

print("Visualización de Outliers para GrLivArea")
p_grlivarea <- df_limpio %>%
  ggplot(aes(y = GrLivArea)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Boxplot de GrLivArea",
       y = "Área Habitable (pies cuadrados)") +
  theme_minimal()
p_grlivarea

# Identificación de outliers usando el método del rango intercuartílico (IQR)
grlivarea_iqr <- IQR(df_limpio$GrLivArea)
grlivarea_q1 <- quantile(df_limpio$GrLivArea, 0.25)
grlivarea_q3 <- quantile(df_limpio$GrLivArea, 0.75)

limite_superior <- grlivarea_q3 + 1.5 * grlivarea_iqr
limite_inferior <- grlivarea_q1 - 1.5 * grlivarea_iqr

outliers_grlivarea <- df_limpio %>%
  filter(GrLivArea > limite_superior| GrLivArea < limite_inferior)

print(paste("Número de outliers identificados en GrLivArea:", nrow(outliers_grlivarea)))
print("Outliers en GrLivArea (valores extremos):")
print(outliers_grlivarea %>% select(Id, GrLivArea, SalePrice))