# ==================================
# Sesi�n 01: Introducci�n a R
# Manejo de datos
# Fecha: 23/04/2022
# ==================================

ls() # Muestra todos los objetos en el enviroment
rm(list = ls()) #Inicio de sus c�digos

# ===================================
# Importaci�n de datos
# ===================================
# Cambiar el directorio actual
setwd("D:/rperezpalmap/Escritorio/Cursos Online/01 UNI_PEEA/PEEA XIII/Sesi�n 01 - R/Aplicaciones")
getwd()

# Descargar datos desde la web
url <- "https://files.minsa.gob.pe/s/t9AFqRbXw3F55Ho/download"
download.file(url = url,
              destfile = "fallecidos_covid.csv",
              mode = "wb")
?download.file #help

# Importar el archivo csv
df_fallecidos <- read.csv(file = "fallecidos_covid.csv",
                          sep = ";",   #Separador
                          header = T,
                          encoding = "UTF-8")  #T�tulos

View(df_fallecidos)
summary(df_fallecidos)

# =======================================
# Manipulaci�n de datos
# =======================================

# Filtrar los datos por una condici�n:
df_1 <- df_fallecidos[df_fallecidos$DEPARTAMENTO == "AMAZONAS",]
print(df_1)

# Filtrar los datos por una condici�n y motrar s�lo 3 columnas:
df_2 <- df_fallecidos[df_fallecidos$DEPARTAMENTO == "AMAZONAS",
                      c("UUID","EDAD_DECLARADA","SEXO")]
print(df_2)

# Seleccionar los primeros "n" datos
df_fallecidos[1:150,c("UUID","EDAD_DECLARADA","SEXO")]
head(df_fallecidos,n = 150)

# Seleccionar los �ltimos "n"datos
tail(df_fallecidos,n = 150)

# Crear nuevas variables
df_fallecidos$COD_PERIODO <- substr(df_fallecidos$FECHA_FALLECIMIENTO,1,6) 
df_fallecidos$FECHA <- as.Date(as.character(df_fallecidos$FECHA_FALLECIMIENTO),"%Y%m%d")
class(df_fallecidos$FECHA_FALLECIMIENTO)

# Contar el n�mero de missings o valores perdidos
sum(is.na(df_fallecidos$UUID))
sapply(df_fallecidos,FUN = function(x){sum(is.na(x))})

# Agrupar los datos
df_datos <- aggregate(formula = EDAD_DECLARADA ~ SEXO + COD_PERIODO,
                      data = df_fallecidos,
                      FUN = length)
df_datos  

# Cambiar el Nombre de columnas 
colnames(df_datos) <- c("GENERO","COD_PERIODO","N_FALLECIDOS")
View(df_datos)

# ================================================
# Gr�ficos en R 
# ================================================
# Columna de fecha (formato R)  
df_datos$FECHA <- as.Date(paste0(df_datos$COD_PERIODO,"01"),"%Y%m%d")

# Gr�fico de barras con el n�mero de fallecidos por sexo
plot(x = as.factor(df_fallecidos$SEXO),
     main = "Distrbuci�n de fallecidos por sexo",
     xlab = "G�nero",
     ylab = "N�mero de fallecidos",
     col = "darkblue")

# Gr�fico de l�neas 
plot(x = df_datos[df_datos$GENERO == "MASCULINO",]$FECHA,
     y = df_datos[df_datos$GENERO == "MASCULINO",]$N_FALLECIDOS,
     type = "l",
     col = "blue",
     lwd = 2,#ancho
     lty = 3,# tipo de l�nea
     main = "Evoluci�n del n�mero de fallecidos por g�nero",
     xlab = "Fecha",
     ylab = "N�mero de fallecidos")
lines(x = df_datos[df_datos$GENERO == "FEMENINO",]$FECHA,
      y = df_datos[df_datos$GENERO == "FEMENINO",]$N_FALLECIDOS,
      type = "l",
      col = "RED",
      lwd = 2,#ancho
      lty = 1,# tipo de l�nea
      main = "Evoluci�n del n�mero de fallecidos por g�nero",
      xlab = "Fecha",
      ylab = "N�mero de fallecidos")
legend('topleft',
       legend = c("Masculino","Femenino"),
       lty= c(3,1),
       col = c("blue","red"))

# Gr�fico de Histograma
hist(df_fallecidos$EDAD_DECLARADA,
     main = "Histograma de edad de los fallecidos",
     xlab = "EDAD",
     col = "darkblue")
abline(a = 10000,b = 0, col = "red")


# Librer�as para geolocalizaci�n:
# ggmaps 
# ggplot2
# leaflet



