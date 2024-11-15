
# -------------------------------------- #
# Script de R para ejemplos del manual   #
# -------------------------------------- #

# Bibliotecas
library(tidyverse)
library(readxl)

# -------------------------------------- #
# Cargar los datos desde repositorio     #
# -------------------------------------- #
url_ocup <- "https://github.com/Sebastian-Dow/econometria_r/raw/main/data/output/GEIH_Ocupados_012023_122023.xlsx"
url_caract <- "https://github.com/Sebastian-Dow/econometria_r/raw/main/data/output/GEIH_CaractHogar_012023_122023.xlsx"

download.file(url_ocup, destfile = "GEIH_Ocupados.xlsx", mode = "wb")
download.file(url_caract, destfile = "GEIH_CaractHogar.xlsx", mode = "wb")

ocup <- read_xlsx("GEIH_Ocupados.xlsx")
caract <- read_xlsx("GEIH_CaractHogar.xlsx")

# -------------------------------------- #
# Unir módulos usando diferentes métodos #
# -------------------------------------- #
# Unión interna (solo coincidencias)
inner_data <- inner_join(ocup, caract, by = "id_hogar")

# Unión completa (todas las observaciones)
full_data <- full_join(ocup, caract, by = "id_hogar")

# Unión derecha (mantiene todas las filas de hogars)
right_data <- right_join(ocup, caract, by = "id_hogar")

# Unión izquierda (mantiene todas las filas de ocupados)
left_data <- left_join(ocup, caract, by = "id_hogar")

# -------------------------------------- #
# Filtrar datos                          #
# -------------------------------------- #
# Filtrar por asalariados y mayores de 25 años
filtered_data <- inner_data %>% 
  filter(asal == 1 & edad >= 25)

# -------------------------------------- #
# Estadísticas descriptivas              #
# -------------------------------------- #
# Estadísticas de tendencia central
mean_edad <- mean(filtered_data$edad, na.rm = TRUE)
median_edad <- median(filtered_data$edad, na.rm = TRUE)
mode_edad <- filtered_data$edad %>% table() %>% which.max()

# Estadísticas de dispersión
var_edad <- var(filtered_data$edad, na.rm = TRUE)
sd_edad <- sd(filtered_data$edad, na.rm = TRUE)
range_edad <- range(filtered_data$edad, na.rm = TRUE)
quantiles <- quantile(filtered_data$edad, na.rm = TRUE)

# -------------------------------------- #
# Gráficas                               #
# -------------------------------------- #
# Histograma de edad
hist(filtered_data$edad, main = "Distribución de Edades", xlab = "Edad", col = "skyblue", border = "white")

# Boxplot de horas trabajadas
boxplot(filtered_data$horas_pa ~ filtered_data$area, 
        main = "Horas trabajadas por área",
        xlab = "Área",
        ylab = "Horas trabajadas",
        col = "lightgreen")

# Relación entre edad y horas trabajadas
plot(filtered_data$edad, filtered_data$horas_pa, 
     main = "Relación entre edad y horas trabajadas",
     xlab = "Edad",
     ylab = "Horas trabajadas",
     col = "blue", pch = 16)

# -------------------------------------- #
# Correlación                            #
# -------------------------------------- #
# Correlación de Pearson entre edad y horas trabajadas
correlacion <- cor(filtered_data$edad, filtered_data$horas_pa, use = "complete.obs")

# -------------------------------------- #
# Filtrar datos para asalariados         #
# -------------------------------------- #
# Filtrar por asalariados
filtered_data <- data %>% filter(asal == 1)

# -------------------------------------- #
# Estadísticas descriptivas              #
# -------------------------------------- #
# Estadísticas de ingreso
mean_inglabo <- mean(filtered_data$inglabo, na.rm = TRUE)
median_inglabo <- median(filtered_data$inglabo, na.rm = TRUE)
quantiles_inglabo <- quantile(filtered_data$inglabo, na.rm = TRUE)

# -------------------------------------- #
# Gráficas: Ingreso vs otras variables   #
# -------------------------------------- #
# Boxplot: Ingreso vs Sexo
boxplot(filtered_data$inglabo ~ filtered_data$sexo, 
        main = "Distribución de Ingresos por Sexo",
        xlab = "Sexo (0 = Hombre, 1 = Mujer)",
        ylab = "Ingreso Laboral",
        col = c("lightblue", "pink"))

# Scatterplot: Ingreso vs Edad
plot(filtered_data$edad, filtered_data$inglabo,
     main = "Relación entre Edad e Ingreso Laboral",
     xlab = "Edad",
     ylab = "Ingreso Laboral",
     col = "darkgreen", pch = 16)

# Boxplot: Ingreso vs Nivel Educativo
boxplot(filtered_data$inglabo ~ filtered_data$edu, 
        main = "Distribución de Ingresos por Nivel Educativo",
        xlab = "Nivel Educativo",
        ylab = "Ingreso Laboral",
        col = rainbow(8))

# -------------------------------------- #
# Correlación                            #
# -------------------------------------- #
# Correlaciones entre variables
cor_ing_edad <- cor(filtered_data$inglabo, filtered_data$edad, use = "complete.obs")
cor_ing_edu <- cor(filtered_data$inglabo, filtered_data$anios_edu, use = "complete.obs")

