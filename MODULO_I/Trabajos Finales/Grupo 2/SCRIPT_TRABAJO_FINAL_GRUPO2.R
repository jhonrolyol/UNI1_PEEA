# ==================================
# Script: TRABAJO FINAL 
# Tema: manipulate/stringr/lubridate
# ==================================
# ===================================
# Integrantes Grupo N° 2:
# - BURGOS CASTILLO, MARÍA TATIANA
# - HUAMANCHUMO LUIS, ERICK JAVIER
# - RUMICHE CORONADO, RODRIGO DAVID
# - ALIAGA ALIAGA, FÁTIMA
# ===================================

rm(list = ls())

# Obtener directorio actual
getwd()

# Cambiar el directorio actual
setwd("C:/Users/Muriel Huamanchumo/Documents/CE Econometría/Sesion 1/Trabajo Final R")

# ===================================
# 0. Instalacion de librerias
# ===================================
install.packages("readxl")
install.packages("data.table")
install.packages("manipulate")
install.packages("lubridate")
install.packages("stringr")
install.packages("ggplot2")

library(data.table)
library(manipulate)
library(lubridate)
library(stringr)
library(readxl)
library(ggplot2)
library(dplyr)

# ===================================
# 1. Importación de datos
# ===================================
# Importar el archivo xlsx
#df_vacunacion <- read_excel("vacunacion.xlsx")
df_vacunacion <- fread(file = "LORETO VACUNACION 2.csv",sep =";",header = T)

View(df_vacunacion)
summary(df_vacunacion)

# Función para aplicar a cada columna
sapply(df_vacunacion,FUN = class)

# ===================================
# 2. Análisis de fechas
# ===================================
df_vacunacion$FECHA_VACUNACION <- ymd(df_vacunacion$FECHA_VACUNACION) #convertir de numero a fecha
df_vacunacion$mes_vacunacion <- month(df_vacunacion$FECHA_VACUNACION)
df_vacunacion$year_vacunacion <- year(df_vacunacion$FECHA_VACUNACION)
df_vacunacion$dia_vacunacion <- day(df_vacunacion$FECHA_VACUNACION)
df_vacunacion$fecha <- strftime(df_vacunacion$FECHA_VACUNACION,format="%Y%m")
df_vacunacion$fecha_actual <- today()

# Columna de fecha (formato R)  
df_vacunacion$FECHA <- as.Date(paste0(df_vacunacion$fecha,"01"),"%Y%m%d")

#Calculo de periodo de tiempo
elapsed.time <- df_vacunacion$FECHA_VACUNACION %--% df_vacunacion$fecha_actual
df_vacunacion$duracion <- as.duration(elapsed.time) / dmonths(1) #

t(colnames(df_vacunacion)) #saber la posición de la columna
cols <- c(3,5,7:12)
df_vacunacion <- as.data.frame(df_vacunacion)

# ===================================
# 3. Análisis de texto
# ===================================
df_vacunacion[,cols] <- sapply(df_vacunacion[,cols],FUN = str_to_title)
df_vacunacion$GRUPO_RIESGO <- str_to_sentence(df_vacunacion$GRUPO_RIESGO)

# Quitar espacios vacios
df_vacunacion[,cols] <- sapply(df_vacunacion[,cols],FUN = str_trim)

df_vacunacion$NEW_sEXO <- ifelse(str_length(df_vacunacion$SEXO)==8,
                            str_pad(df_vacunacion$SEXO, width="9", side="left",pad="-"),
                            str_pad(df_vacunacion$SEXO, width="10", side="left",pad="/"))

# Añadir un character
df_vacunacion$FABRICANTE <- ifelse(str_length(df_vacunacion$FABRICANTE)==6,
                                   str_c("1. ", df_vacunacion$FABRICANTE),ifelse(str_length(df_vacunacion$FABRICANTE)==7,
                                   str_c("2. ", df_vacunacion$FABRICANTE),
                                   ifelse(str_length(df_vacunacion$FABRICANTE)==9,
                                   str_c("3. ", df_vacunacion$FABRICANTE),
                                   str_c("4. ", df_vacunacion$FABRICANTE))))

# Quitar un character
df_vacunacion$NEW_sEXO <- str_remove(df_vacunacion$NEW_sEXO, pattern = "-")
df_vacunacion$NEW_sEXO <- str_remove(df_vacunacion$NEW_sEXO, pattern = "/")

# Unir character
df_vacunacion$DESCRIPCION <- str_glue("{df_vacunacion$GRUPO_RIESGO} de sexo {df_vacunacion$SEXO} con 
                                      {df_vacunacion$EDAD} años que vive en {df_vacunacion$DEPARTAMENTO}")
df_vacunacion$DESCRIPCION <- str_replace_all(df_vacunacion$DESCRIPCION, pattern = fixed(df_vacunacion$DEPARTAMENTO, ignore_case = T),replacement = toupper)
df_vacunacion$DESCRIPCION <- str_replace_all(df_vacunacion$DESCRIPCION, pattern = fixed(df_vacunacion$SEXO, ignore_case = T),replacement = tolower)

#df_vacunacion$NEW_DEP <- str_sort(df_vacunacion$DEPARTAMENTO)
#table(df_vacunacion$COD_DEP)

df_vacunacion$ID <- 1                                          
df_vacunacion$COD_DEP <- ifelse(df_vacunacion$DEPARTAMENTO == "Amazonas",1,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Ancash",2,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Apurimac",3,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Arequipa",4,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Ayacucho",5,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Cajamarca",6,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Callao",7,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Cusco",8,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Huancavelica",9,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Huanuco",10,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Ica",11,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Junin",12,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "La Libertad",13,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Lambayeque",14,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Lima",15,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Loreto",16,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Madre De Dios",17,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Moquegua",18,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Pasco",19,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Piura",20,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Puno",21,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "San Martin",22,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Tacna",23,
                                  ifelse(df_vacunacion$DEPARTAMENTO == "Tumbes",24,25))))))))))))))))))))))))       


table(is.na(df_vacunacion$EDAD)==TRUE, exclude = NULL)
df_vacunacion <- df_vacunacion[is.na(df_vacunacion$EDAD) == FALSE,]
df_vacunacion$EDAD <- ifelse(df_vacunacion$EDAD>100,100,df_vacunacion$EDAD)
df_vacunacion$DOSIS <- ifelse((df_vacunacion$DOSIS=="4"|df_vacunacion$DOSIS=="5"),"3",df_vacunacion$DOSIS)

dt_1 <- as.data.frame(setDT(df_vacunacion)[,list(N_VACUNADOS = sum(ID,na.rm = T)),
                         by = c("PROVINCIA","DOSIS","EDAD","SEXO","FABRICANTE","FECHA")])
quantile(dt_1$EDAD,)

setDT(dt_1)[,RANGO_EDAD:= ifelse(EDAD<=30, "[0 - 30]",ifelse(EDAD>30 & EDAD<=46, "[30 - 46]",
                                                             ifelse(EDAD>46 & EDAD<=64, "[46 - 64]","[64 - más]")))]

dt_1 <- as.data.frame(dt_1)
df_vacunacion <- as.data.frame(df_vacunacion)

# ===================================
# 4. Análisis Gráfico
# ===================================
# 4.1 Gráfico de regresión
manipulate(
  ggplot(dt_1, aes_string(x = x.variable,y = y.variable))+
    geom_point() + geom_smooth(formula = y ~ x, method = method),
  x.variable = picker("N_VACUNADOS"),
  y.variable = picker("EDAD"),
  method = picker("lm", "glm", "gam", "loess", "rlm"))

# 4.2 Gráfico de barras
manipulate(
plot(x = as.factor(df_vacunacion[,Factor]),
     main = paste0("Distribución de Vacunados por ",Factor),
     xlab = Factor,
     ylab = "Número de Vacunados",
     col = rgb(0,1,1, alpha = 0.2)),Factor = picker("SEXO", "PROVINCIA","DOSIS","FABRICANTE"))

# 4.3 Gráfico de cajas
manipulate(
  boxplot(dt_1$N_VACUNADOS ~ dt_1[,Factor], outline = outline, 
          xlab = Factor, 
          ylab ="N Vacunados", # etiqueta al eje
          col = rgb(0,1,1, alpha = 0.2), 
          main = paste0("Boxplot del Numero de Vacunados por ",Factor), # titulo
          border = "black", # color al borde
          outpch = 24, # Símbolo para los outliers
          outbg = "green", # Color de los datos atípicos
          whiskcol = "red", # Color de los bigotes
          whisklty = 3), # Tipo de línea para los bigotes
  outline = checkbox(FALSE, "Show outliers"), 
  Factor = picker("SEXO", "PROVINCIA","FABRICANTE","RANGO_EDAD","DOSIS"))

# 4.4 Gráfico de histograma
manipulate(
hist(dt_1[,Factor], probability = TRUE, main = "Histograma del Numero de Vacunados",
    xlab = "N° vacunados",col = "grey", border = "blue", breaks=bins),
    bins = slider(1, 30, step=2, initial =5, label="Bins"),
    resetSeed = button("Reset Seed"),
    Factor = picker("N_VACUNADOS", "EDAD"))
lines(density(dt_1$EDAD), col = "red", lwd = 2)

# genera data agrupada
dt_2 <- as.data.frame(setDT(df_vacunacion)[,list(N_VACUNADOS = sum(ID,na.rm = T)),
                                           by = c("FECHA","PROVINCIA","DOSIS")])
dt_3 <- as.data.frame(setDT(dt_2)[,list(N_VACUNADOS = sum(N_VACUNADOS)),
                                           by = c("DOSIS","FECHA")])
dt_3 <- dt_3[order(dt_3$DOSIS,dt_3$FECHA),]
dt_3 <- setDT(dt_3)[, ACUM := cumsum(N_VACUNADOS), c("DOSIS")][]
dt_3$AVANCE_VACUNACION <- round(as.numeric((format((dt_3$ACUM/1037055)*100, scientific = F))),2)

# 4.5 Gráfico de lineas
manipulate(
  ggplot(dt_2, aes_string(x=x.variable, y=y.variable))+
    geom_line(aes_string(color=z.variable))+ggtitle("LORETO: AVANCE DE LA VACUNACION")+labs(x="Periodo",y="n vacunas"),
  x.variable = picker("FECHA"),
  y.variable = picker("N_VACUNADOS"),
  z.variable = picker("PROVINCIA","DOSIS"))

manipulate(
  ggplot(dt_3, aes_string(x=x.variable, y=y.variable))+
    geom_line(aes_string(color=z.variable))+ggtitle("LORETO: AVANCE DE LA VACUNACION")+labs(x="Periodo",y="% Poblacion"),
  x.variable = picker("FECHA"),
  y.variable = picker("AVANCE_VACUNACION"),
  z.variable = picker("DOSIS"))





