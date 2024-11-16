
##-----------------------------------##
## Procesamiento: GEIH - 2023        ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-----------------------------------##

# Cargar librerías
library(tidyverse)
library(janitor)

# Definir directorio de trabajo
setwd("C:\\Users\\Portatil\\Desktop\\econometria_r\\data\\")

##-------------------------------------------##
## Primero: cargar ambos módulos (2023)      ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------##

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Módulo de ocupados
ocup.geih.2023 = vector(mode = "list", length = 12)
names(ocup.geih.2023) = meses
for (i in 1:12) {
  print(paste0(meses[i], " - ", 2023))
  ocup.geih.2023[[i]] = read.csv(paste0("GEIH-2023\\", meses[i], "\\Ocupados.csv"),
                                 sep = ";") %>% clean_names() %>%
    dplyr::filter(area %in% c(11,5, 76, 8, 68,
                              17, 66,54, 52, 73,
                              23, 13, 50)) %>% mutate(year = 2023, month = i)
}

df.ocup.geih = do.call(rbind, ocup.geih.2023)

# Módulo de características generales
car.geih.2023 = vector(mode = "list", length = 12)
names(car.geih.2023) = meses
for (i in 1:12) {
  print(paste0(meses[i], " - ", 2023))
  car.geih.2023[[i]] = read.csv(paste0("GEIH-2023\\",meses[i],
                                       "\\Características generales, seguridad social en salud y educación.csv"),
                                 sep = ";") %>% clean_names() %>%
    dplyr::filter(area %in% c(11,5, 76, 8, 68,
                              17, 66,54, 52, 73,
                              23, 13, 50)) %>% mutate(year = 2023, month = i)
}

df.car.geih = do.call(bind_rows, car.geih.2023)

##----------------------------------------##
## Segundo: unión entre los módulos       ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##----------------------------------------##

# Crear id personas
df.ocup.geih$id <- paste0(df.ocup.geih$directorio,"-", df.ocup.geih$secuencia_p,"-", df.ocup.geih$orden)
df.ocup.geih$id_hogar <- paste0(df.ocup.geih$directorio,"-", df.ocup.geih$secuencia_p)

# Crear id hogares
df.car.geih$id <- paste0(df.car.geih$directorio,"-", df.car.geih$secuencia_p,"-", df.car.geih$orden)
df.car.geih$id_hogar <- paste0(df.car.geih$directorio,"-", df.car.geih$secuencia_p)

# Excluir variables
ocup <- df.ocup.geih %>% select(-c(periodo, hogar, mes, per, regis, ft))
car <- df.car.geih  %>% select(-c(periodo, hogar, regis, fex_c18))

##---------------------------------------------##
## Tercero: seleccionar variables (ocupados)   ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##---------------------------------------------##

keep.ocup <- c(
  "year",                            # Año
  "month",                           # Mes
  "id",                              # Llave persona
  "id_hogar",                        # Llave hogar
  "area",                            # Área
  "dpto",                            # Departamento
  "clase",                           # Dominio (urbano o rural)
  "p6800",                           # Horas de trabajo en PA
  "p6430",                           # Posición en PA
  "p6426",                           # Tiempo trabajando en la empresa
  "p6500",                           # ¿Cuánto ganó?
  "p6920",                           # Pensiones
  "p6510", "p6510s1","p6510s2",        # Horas extra
  "p6545", "p6545s1","p6545s2",        # Primas
  "p6580","p6580s1","p6580s2",         # Bonificaciones
  "p6585s1","p6585s1a2", "p6585s1a1",  # Alimentación
  "p6585s2","p6585s2a2", "p6585s2a1",  # Transporte
  "p6585s3","p6585s3a2","p6585s3a1",   # Subsidio familiar
  "p6585s4","p6585s4a2", "p6585s4a1",  # Subsidio educativo
  "p6630s1","p6630s1a1",            # Prima de servicios
  "p6630s2","p6630s2a1",            # Prima de navidad
  "p6630s3","p6630s3a1",            # Prima de vacaciones
  "p6630s4","p6630s4a1",            # Viáticos permanentes
  "p6630s6","p6630s6a1",            # Bonificaciones anuales
  "p6750",                          # Ganancia neta
  "p3073",                          #¿a cuántos meses equivale la ganancia neta?
  "p550",                           # Ganancia neta en CD
  "p7040",                          # ¿tiene segunda actividad?
  "p7070",                          # ¿cuánto recibió o ganó el mes pasado en ese 2do trabajo?
  "p7045",                          # ¿cuántas horas trabajó en el segundo trabajo
  "p6590", "p6590s1",               # Ingreso en especie (IE): alimentos
  "p6600", "p6600s1",               # IE: vivienda
  "p6610", "p6610s1",               # IE: transporte
  "p6620", "p6620s1",               # IE: electrodomésticos, ropa, etc.
  "inglabo",
  "fex_c18"
)

### Filtrar variables
ocup = ocup %>% select(keep.ocup)

### Crear y recodificar variables

# Dominio
ocup$area[ocup$area == "8"] = "BARRANQUILLA"
ocup$area[ocup$area == "11"] = "BOGOTA"
ocup$area[ocup$area == "68"] = "BUCARAMANGA"
ocup$area[ocup$area == "76"] = "CALI"
ocup$area[ocup$area == "13"] = "CARTAGENA"
ocup$area[ocup$area == "54"] = "CUCUTA"
ocup$area[ocup$area == "73"] = "IBAGUE"
ocup$area[ocup$area == "17"] = "MANIZALES"
ocup$area[ocup$area == "5"] = "MEDELLIN"
ocup$area[ocup$area == "23"] = "MONTERIA"
ocup$area[ocup$area == "52"] = "PASTO"
ocup$area[ocup$area == "66"] = "PEREIRA"
ocup$area[ocup$area == "50"] = "VILLAVICENCIO"

# Variable para asalariados
# Dummy asalariados
ocup$asal <- ifelse(ocup$p6430 %in% c(1,2,3,7), 1, 0)

# Variable para independientes
ocup$ind <- ifelse(ocup$p6430 %in% c(4,5,8), 1, 0)

# Variable para trabajadores familiares sin remuneración
ocup$trab_fam <- ifelse(ocup$p6430 ==6, 1, 0)

# Horas trabajadas en la primera actividad
ocup$horas_pa <- ocup$p6800

# Horas trabajadas en la segunda actividad
ocup$horas_sa <- ocup$p7045

# One-hot encoding para la variable "posición laboral"
# Eliminar a las observaciones con posición laboral == otros
ocup$posicion <- factor(ocup$p6430, levels = c(1:9))
ocup$posicion <- dplyr::recode(ocup$posicion, "1" = "obrero", "2" = "obrero",
                                   "3" = "domestico", "4" = "propia", "5" = "patrono",
                                   "6" = "domestico", "7" = "domestico",
                                   "8" = "obrero", "9" = "otros")
ocup <- ocup %>% mutate(value = 1)  %>% spread(posicion, value,  fill = 0 )

# Número de meses trabajando
ocup$meses_trab <- ocup$p6426

# Dummy Bogotá
ocup$capital <- ifelse(ocup$dpto == 11, 1, 0)

# Seleccionar variables finales para el módulo de ocupados
sub.ocup = ocup %>% select(year, month, id, id_hogar, clase, dpto, area, capital,
                           inglabo, asal, ind, trab_fam, horas_pa,
                           horas_sa, obrero, domestico, propia,
                           patrono, meses_trab)

writexl::write_xlsx(sub.ocup, "output//GEIH_Ocupados_012023_122023.xlsx")

##-------------------------------------------------------------##
## Cuarto: seleccionar variables (características generales)   ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##-------------------------------------------------------------##

keep.car <- c(
  "id",                              # Llave persona
  "id_hogar",                        # Llave hogar
  "p3271",                           # Sexo
  "p6040",                           # Edad
  "p3042",                           # Nivel educativo máximo
  "p3042s1",                         # Grado
  "p3043",                           # Título o diploma
  "dpto",                            # Departamento
  "p6050",                           # ¿Jefe del hogar?
  "p6090"                            # Afiliado a salud
)

### Filtrar variables
car = car %>% select(keep.car)

# Variable edad y edad al cuadrado
car$edad <- car$p6040
car$edad_sqr <- car$edad^2

# Años de educación
car$edu <- car$p3042
car$grado <- car$p3042s1
car <- car %>% mutate(grado = replace(grado, grado %in% c(98, 99), NA)) # Faltantes como NA

car$anios_edu <- 0
car$anios_edu[which(car$edu == 1 & !is.na(car$edu))] = 0
car$anios_edu[which(car$edu == 2 & !is.na(car$edu))] = car$grado[which(car$edu == 2 & !is.na(car$edu))]
car$anios_edu[car$edu == 3 & !is.na(car$edu)] = 1 + car$grado[car$edu == 3 & !is.na(car$edu)]
car$anios_edu[car$edu == 4 & !is.na(car$edu)] = 6 + car$grado[car$edu == 4 & !is.na(car$edu)]
car$anios_edu[(car$edu == 5 | car$edu == 6) & !is.na(car$edu)] = 6 + car$grado[(car$edu == 5 | car$edu == 6) & !is.na(car$edu)]
car$anios_edu[car$edu %in% c(7:10) & !is.na(car$edu)] = 12 + car$grado[car$edu %in% c(7:10) & !is.na(car$edu)]
car$anios_edu[car$edu %in% c(11,12) & !is.na(car$edu)] = 17 + car$grado[car$edu %in% c(11,12) & !is.na(car$edu)]
car$anios_edu[car$edu %in% c(13) & !is.na(car$edu)] = 19 + car$grado[car$edu %in% c(13) & !is.na(car$edu)]

# Dummy Sexo
car$sexo <- ifelse(car$p3271 == 1, 0, 1)

# Dummy jefe del hogar
car$jefe <- ifelse(car$p6050 == 1, 1, 0)

# Dummy (personas de 25 años o más sin educación)
car$edad_25_ne <- ifelse(car$edad >= 25 &
                            car$edu == 1, 1, 0)

# Dummy para afiliado a salud
car$salud <- ifelse(car$p6090 == 1, 1, 0)

# Seleccionar variables finales para el módulo de características generales
sub.car = car %>% select(id, id_hogar, sexo, edad, edad_sqr, edu,
                         grado, anios_edu, jefe, edad_25_ne, salud)

writexl::write_xlsx(sub.car, "output//GEIH_CaractHogar_012023_122023.xlsx")

##----------------------------------------##
## Tercero: unión entre los módulos       ##-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##----------------------------------------##

# Pendiente por incluir en el manuscrito


