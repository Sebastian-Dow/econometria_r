# -------------------------------------- #
# Script de R para ejemplos del manual   #
# -------------------------------------- #

# -------------------------------------- #
# Bibliotecas                            #
# -------------------------------------- #
# Cap 2
library(tidyverse)
library(readxl)
library(ggplot2)

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
  select(area, inglabo, asal, sexo, edad, edu, anios_edu)
# Crear variable asal en vez de seleccionarla (inglabo > 0? 1: 0)

# -------------------------------------- #
# Filtrar registros con NAs              #
# -------------------------------------- #
data <- data %>% na.omit()

# -------------------------------------- #
# Filtrar datos                          #
# -------------------------------------- #
data <- data %>%
  filter(asal == 1) %>% # & inglabo > 0) %>% # Asalariados entre 18 y 65 años 
  drop_na() # Eliminar registros con NA para las variables

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
  group_by(anios_edu) %>%tf
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

# Histograma: Distribución de Edades
ggplot(data, aes(x = edad)) +
  geom_histogram(fill = "darkorange", color = "white", bins = 20) +
  labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Scatterplot: Edad vs Ingreso Laboral
ggplot(data, aes(x = edad, y = inglabo)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  labs(title = "Relación entre Edad e Ingreso Laboral", x = "Edad", y = "Ingreso Laboral") +
  theme_minimal()

# Boxplot: Ingreso por Sexo
ggplot(data, aes(x = as.factor(sexo), y = inglabo, fill = as.factor(sexo))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightgreen", "purple")) +
  labs(
    title = "Distribución de Ingresos por Sexo",
    x = "Sexo (0 = Hombre, 1 = Mujer)",
    y = "Ingreso Laboral",
    fill = "Sexo"
  ) +
  theme_minimal()

# Boxplot: Ingreso por Nivel Educativo
ggplot(data, aes(x = as.factor(edu), y = inglabo, fill = as.factor(edu))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribución de Ingresos por Nivel Educativo",
    x = "Nivel Educativo",
    y = "Ingreso Laboral",
    fill = "Nivel Educativo"
  ) +
  theme_minimal()

# Boxplots por ciudad
ggplot(filter(data, area %in% c("BOGOTA", "MEDELLIN", "CALI")), aes(x = as.factor(sexo), y = inglabo, fill = as.factor(sexo))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightgreen", "purple")) +
  facet_wrap(~area) +
  labs(
    title = "Distribución de Ingresos por Sexo y Ciudad",
    x = "Sexo",
    y = "Ingreso Laboral",
    fill = "Sexo"
  ) +
  theme_minimal()

# -------------------------------------- #
# Creación  de variables para modelo     #
# -------------------------------------- #
model_data <- data %>%
  select(inglabo, sexo, edad, anios_edu) %>%
  mutate(
    edad2 = edad^2,                          # Edad al cuadrado
    log_inglabo = log(inglabo + 1),              # Logaritmo de ingreso laboral
    log_edad = log(edad),                    # Logaritmo de edad
    log_anios_edu = log(anios_edu + 1)       # Logaritmo de años de educación
  )

# Verificar sumar 1 a los logaritmos

# -------------------------------------- #
# Análisis exploratorio: model_data      #
# -------------------------------------- #

# Estadísticas descriptivas
mean_edad <- mean(model_data$edad, na.rm = TRUE)
mean_log_inglabo <- mean(model_data$log_inglabo, na.rm = TRUE)

# Gráficos exploratorios
par(mfrow = c(1, 1))
# Scatterplot: Años trabajados vs Ingreso
plot(model_data$edad, model_data$inglabo,
     main = "Edad vs Ingreso Laboral",
     xlab = "Edad",
     ylab = "Ingreso Laboral",
     col = "darkgreen", pch = 16)

# Boxplot: Ingreso por Nivel Educativo
boxplot(model_data$inglabo ~ model_data$anios_edu,
        main = "Ingreso por Años de Educación",
        xlab = "Años de Educación",
        ylab = "Ingreso Laboral",
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
modelo_loglin <- lm(log_inglabo ~ anios_edu + edad + edad2, data = model_data)

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

# Prueba de hipótesis sobre combinación lineal: H0: anios_edu = edad
linearHypothesis(modelo_clasico, "anios_edu = edad")

# Prueba múltiple: H0: anios_edu = edad y edad2 = 0
linearHypothesis(modelo_clasico, c("anios_edu = edad2", "edad2 = 0"))

# -------------------------------------- #
# Métricas de ajuste del modelo          #
# -------------------------------------- #

# R2 ajustado
adj_r_squared <- summary(modelo_clasico)$adj.r.squared

# Estadístico F
f_statistic <- summary(modelo_clasico)$fstatistic
