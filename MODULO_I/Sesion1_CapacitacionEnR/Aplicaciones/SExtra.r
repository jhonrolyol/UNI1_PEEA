# ==================================
# Sesión 01: Introducción a R - Extra
# Manejo de librerías
# Fecha: 23/04/2022
# ==================================
rm(list = ls())

# ===================================
# Importación de datos
# ===================================
# Cambiar el directorio actual
setwd("D:/rperezpalmap/Escritorio/Cursos Online/01 UNI_PEEA/PEEA XIII/Sesión 01 - R/Aplicaciones")
getwd()

# Descargar datos desde la web
url <- "https://files.minsa.gob.pe/s/eRqxR35ZCxrzNgr/download"
download.file(url = url,
              destfile = "positivos_covid.csv",
              mode = "wb")


# ======================================
# Manejo de base de datos con data.table
# ======================================
# install.packages("data.table")
library(data.table)

# Importar datos
  # data.frame
  df_positivos <- read.csv("positivos_covid.csv",sep =";",header = T)

  # data.table: más eficiente la carga de información
  dt_positivos <- fread(file = "positivos_covid.csv",sep =";",header = T)

# Filtrar la información
  # data.frame
  df_positivos[df_positivos$DEPARTAMENTO == "LIMA",]
  
  # data.table
  dt_positivos[DEPARTAMENTO == "LIMA" & EDAD > 25,]
  
# Seleccionar columnas
  # data.frame 
  df_positivos[df_positivos$DEPARTAMENTO == "LIMA",c("UBIGEO","id_persona","SEXO")]
  # data.table 
  dt_positivos[DEPARTAMENTO == "LIMA" & EDAD > 25,c("UBIGEO","id_persona","SEXO")]

# Crear variables 
  # data.frame
  df_positivos$COD_PERIODO <- substr(df_positivos$FECHA_RESULTADO,1,6)
  # data.table
  dt_positivos[,COD_PERIODO:=substr(FECHA_RESULTADO,1,6)]
  dt_positivos[,c("COD_PERIODO","FLAG_LIMA"):=list(substr(FECHA_RESULTADO,1,6),
                                                   ifelse(DEPARTAMENTO == "LIMA",1,0))]
  
# Agregación de datos
  # data.frame
  df_datos <- aggregate(formula = EDAD ~ SEXO + COD_PERIODO,
                        data = df_positivos,
                        FUN = mean)
  # data.table 
  dt_datos <- dt_positivos[,list(EDAD_AVG = mean(EDAD,na.rm = T),
                                 N = .N,
                                 N_LIMA = sum(FLAG_LIMA,na.rm = T)),
                           by = c("SEXO","COD_PERIODO")]
  dt_datos  
  
# Cambiar nombres
  # data.frame
  colnames(df_datos) <- c("GENERO","COD_PERIODO","EDAD_AVG") 
  # data.table
  setnames(dt_datos,old = c("SEXO"),new = c("GENERO"))
  
# Ordernar columnas
  # data.table
  setcolorder(dt_datos, c("COD_PERIODO","GENERO"))
  dt_datos
  
  
# ================================
# Importar data para usar en latex
# ================================
# install.packages("xtable")
library(xtable)
xtable(dt_datos)  
  
  