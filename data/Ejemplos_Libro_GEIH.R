# -------------------------------------- #
# Script de R para ejemplos del manual   #
# -------------------------------------- #

# -------------------------------------- #
# Bibliotecas                            #
# -------------------------------------- #
# Cap 2
library(tidyverse)
library(readxl)

library(ggplot2) # Cambiar gráficos

# Cap 3
library(nortest)
library(car)

# -------------------------------------- #
# Cargar los datos desde repositorio     #
# -------------------------------------- #
url_ocup <- "https://github.com/Sebastian-Dow/econometria_r/raw/main/data/output/GEIH_Ocupados_012023_122023.xlsx"
url_hogar <- "https://github.com/Sebastian-Dow/econometria_r/raw/main/data/output/GEIH_CaractHogar_012023_122023.xlsx"

download.file(url_ocup, destfile = "GEIH_Ocupados.xlsx", mode = "wb")
download.file(url_hogar, destfile = "GEIH_CaractHogar.xlsx", mode = "wb")

ocupados <- read_xlsx("GEIH_Ocupados.xlsx")
hogares <- read_xlsx("GEIH_CaractHogar.xlsx")

# -------------------------------------- #
# Revisar los datos                      #
# -------------------------------------- #
# Previsualizar los daots
head(ocupados)
head(hogares)
#Revisar estructura
str(ocupados)
str(hogares)

# -------------------------------------- #
# Unir módulos con filas coincidientes   #
# -------------------------------------- #
# Unión interna
inner_data <- inner_join(ocupados, hogares, by = c("id", "id_hogar"))

# -------------------------------------- #
# Seleccionar columnas                   #
# -------------------------------------- #
data <- inner_data %>%
  select(area, inglabo, asal, meses_trab, sexo, edad, edu, anios_edu)
# Crear variable asal en vez de seleccionarla (inglabo > 0? 1: 0)

# -------------------------------------- #
# Filtrar registros con NAs              #
# -------------------------------------- #
data <- data %>% na.omit()

# -------------------------------------- #
# Filtrar datos                          #
# -------------------------------------- #
data <- data %>%
  filter(asal == 1) # & inglabo > 0) %>% # Asalariados entre 18 y 65 años 
  drop_na() # Eliminar registros con NA para las variables

nrow(dplyr::filter(data, inglabo == 0))

# -------------------------------------- #
# Submuestra y estandarización           #
# -------------------------------------- #
set.seed(12345)
data <- data %>%
  filter(abs(scale(inglabo)) < 2.5) %>%
  sample_n(5000)

# -------------------------------------- #
# Estadísticas descriptivas generales    #
# -------------------------------------- #
mean_inglabo <- mean(data$inglabo, na.rm = TRUE)
median_inglabo <- median(data$inglabo, na.rm = TRUE)
var_inglabo <- var(data$inglabo, na.rm = TRUE)
sd_inglabo <- sd(data$inglabo, na.rm = TRUE)
quantiles_inglabo <- quantile(data$inglabo, na.rm = TRUE)
deciles_inglabo <- quantile(data$inglabo, na.rm = TRUE, probs = seq(0, 1, by = 0.1))

# -------------------------------------- #
# Estadísticas descriptivas agrupadas    #
# -------------------------------------- #
stats_by_sex <- data %>%
  group_by(sexo) %>%
  summarise(
    mean_inglabo = mean(inglabo, na.rm = TRUE),
    sd_inglabo = sd(inglabo, na.rm = TRUE),
    median_inglabo = median(inglabo, na.rm = TRUE),
    q1_inglabo = quantile(inglabo, na.rm = TRUE, probs = 0.25),
    q1_inglabo = quantile(inglabo, na.rm = TRUE, probs = 0.75)
  )

stats_by_edu <- data %>%
  group_by(anios_edu) %>%
  summarise(
    mean_inglabo = mean(inglabo, na.rm = TRUE),
    sd_inglabo = sd(inglabo, na.rm = TRUE),
    median_inglabo = median(inglabo, na.rm = TRUE),
    q1_inglabo = quantile(inglabo, na.rm = TRUE, probs = 0.25),
    q1_inglabo = quantile(inglabo, na.rm = TRUE, probs = 0.75)
  )

# -------------------------------------- #
# Gráficas                               #
# -------------------------------------- #
# Configuración del espacio gráfico
# par(mfrow = c(2, 2))
par(mfrow = c(1, 1))

# Histograma: Distribución de Edades
hist(data$edad,
     main = "Distribución de Edades",
     xlab = "Edad",
     col = "skyblue", border = "white")

# Scatterplot: Edad vs Ingreso Laboral
plot(data$edad, data$inglabo,
     main = "Relación entre Edad e Ingreso Laboral",
     xlab = "Edad",
     ylab = "Ingreso Laboral",
     col = "darkgreen", pch = 16)

# Boxplot: Ingreso por Sexo # 6e+06
boxplot(data$inglabo ~ data$sexo,
        main = "Distribución de Ingresos por Sexo",
        xlab = "Sexo (0 = Hombre, 1 = Mujer)",
        ylab = "Ingreso Laboral",
        col = c("lightblue", "pink")) # Cambiar color

# Boxplot: Ingreso por Nivel Educativo
boxplot(data$inglabo ~ data$edu,
        main = "Distribución de Ingresos por Nivel Educativo",
        xlab = "Nivel Educativo",
        ylab = "Ingreso Laboral",
        col = rainbow(8))

# Boxplots por ciudad
par(mfrow = c(1, 3))
boxplot(inglabo ~ sexo, data = filter(data, area == "BOGOTA"),
        main = "Distribución de Ingresos (Bogotá)",
        xlab = "Sexo",
        ylab = "Ingreso Laboral",
        col = c("lightblue", "pink"))
boxplot(inglabo ~ sexo, data = filter(data, area == "MEDELLIN"),
        main = "Distribución de Ingresos (Medellín)",
        xlab = "Sexo",
        ylab = "Ingreso Laboral",
        col = c("lightblue", "pink"))
boxplot(inglabo ~ sexo, data = filter(data, area == "CALI"),
        main = "Distribución de Ingresos (Cali)",
        xlab = "Sexo",
        ylab = "Ingreso Laboral",
        col = c("lightblue", "pink"))

# -------------------------------------- #
# Creación  de variables para modelo     # Verificar para usar edad como exper
# -------------------------------------- #
model_data <- data %>%
  select(inglabo, meses_trab, sexo, edad, anios_edu) %>%
  mutate(
    anios_trab = meses_trab / 12,            # Crear variable de años trabajados
    anios_trab2 = anios_trab^2,              # Crear variable de años trabajados al cuadrado
    edad2 = edad^2,
    log_inglabo = log(inglabo),              # Logaritmo de ingreso laboral
    log_anios_trab = log(anios_trab),        # Logaritmo de años trabajados
    log_edad = log(edad),                    # Logaritmo de edad
    log_anios_edu = log(anios_edu + 1)       # Logaritmo de años de educación
  )

# Verificar sumar 1 a los logaritmos

# -------------------------------------- #
# Análisis exploratorio: model_data      #
# -------------------------------------- #

# Estadísticas descriptivas
mean_anios_trab <- mean(model_data$anios_trab, na.rm = TRUE)
mean_log_inglabo <- mean(model_data$log_inglabo, na.rm = TRUE)

# Gráficos exploratorios
par(mfrow = c(1, 1))
# Scatterplot: Años trabajados vs Ingreso
plot(model_data$anios_trab, model_data$inglabo,
     main = "Años Trabajados vs Ingreso Laboral",
     xlab = "Años Trabajados",
     ylab = "Ingreso Laboral",
     col = "darkgreen", pch = 16)

# Boxplot: Logaritmo de ingreso por Nivel Educativo
boxplot(model_data$log_inglabo ~ model_data$anios_edu,
        main = "Log(Ingreso) por Años de Educación",
        xlab = "Años de Educación",
        ylab = "Log(Ingreso Laboral)",
        col = rainbow(8))

# -------------------------------------- #
# Correlación                            #
# -------------------------------------- #
cor_ing_edad <- cor(data$inglabo, data$edad, use = "complete.obs")
cor_ing_edu <- cor(data$inglabo, data$anios_edu, use = "complete.obs")

# -------------------------------------- #
# Modelos de regresión                   #
# -------------------------------------- #

# Modelo lineal clásico
modelo_clasico <- lm(inglabo ~ anios_edu + edad + edad2, data = model_data)

# Resumen del modelo clásico
summary(modelo_clasico)

# Intervalo de confianza al 95%
confint(modelo_clasico, level = 0.95)

# Modelo log-log
modelo_loglog <- lm(log_inglabo ~ log_anios_edu + log_edad, data = model_data)

# Resumen del modelo log-log
summary(modelo_loglog)

# Modelo log-lin
modelo_loglin <- lm(log_inglabo ~ anios_edu + anios_trab + anios_trab2, data = model_data)

# Resumen del modelo log-lin
summary(modelo_loglin)

# -------------------------------------- #
# Análisis de residuos                   #
# -------------------------------------- #

# Obtener los residuos del modelo clásico
residuos_clasico <- residuals(modelo_clasico)

# Pruebas de normalidad de los residuos Shapiro-Wilk
shapiro.test(residuos_clasico)
ks.test(residuos_clasico, "pnorm", mean(residuos_clasico), sd(residuos_clasico))  # Kolmogorov-Smirnov

# Cargar paquete nortest para Anderson-Darling
ad.test(residuos_clasico)

# Gráfico de residuos
plot(modelo_clasico)

# -------------------------------------- #
# Pruebas de hipótesis                   #
# -------------------------------------- #

# Prueba de hipótesis sobre combinación lineal: H0: anios_edu = anios_trab
linearHypothesis(modelo_clasico, "anios_edu = anios_trab")

# Prueba múltiple: H0: anios_edu = anios_trab y anios_trab2 = 0
linearHypothesis(modelo_clasico, c("anios_edu = anios_trab", "anios_trab2 = 0"))

# -------------------------------------- #
# Métricas de ajuste del modelo          #
# -------------------------------------- #

# R2 ajustado
adj_r_squared <- summary(modelo_clasico)$adj.r.squared

# Estadístico F
f_statistic <- summary(modelo_clasico)$fstatistic


