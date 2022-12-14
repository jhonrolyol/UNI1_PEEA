#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
#  XIII PROGRAMA DE ESPECIALIZACI?N EN ECONOMETRIA APLICADA  
#
# CURSO: Software Skills 
# 
# TEMA: Series de tiempo / Modelamiento de datos
#
# Docente: Richard P. P?rez Palma Ponce
# 
# Integrantes:
#              * VALDIVIESO MORALES, Juan Ricardo
#              * ARAUJO GUEVARA, Antony Anderson
#              * MENDIOLA CONTRERAS, Luis Enrique
#              * SANTOS VIERA, Wilder Jhonatan
#              * ORDO?EZ LEON, Jhon Roly
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Antes de nada, limpiamos el workspace por si hubiera alguna informacion cargada
rm(list = ls())

# Limpiamos la consola
cat("\014")

# vemos el directorio de trabajo
getwd()

#Fijamos el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Llamamos lospackage instalados y que utilizaremos
library(tseries) # An?lisis de series temporales
library(quantmod) # Marco de modelado financiero cuantitativo
library(lubridate) # Hace que el manejo de fechas sea un poco m?s f?cil 
library(stringr) # Contenedores simples y consistentes para operaciones de cadenas comunes
library(tidyverse) # conjunto de paquetes que funcionan en armon?a porque comparten representaciones de datos comunes
library(dplyr) # Una herramienta r?pida y consistente para trabajar con marcos de datos como objetos, tanto en la memoria como fuera de la memoria
library(ggplot2) # Un sistema para crear gr?ficos
library(forecast) # Muestra y analiza pron?sticos de series temporales univariadas
library(scales) # Determinar autom?ticamente roturas y etiquetas para ejes y leyendas.
library(stats) # C?lculos estad?sticos y generaci?n de n?meros aleatorios
library(readxl) # Importar archivos de Excel en R
library(tsoutliers) # Detecci?n de valores at?picos en series temporales
library(fma) # Conjuntos de datos de "Pron?sticos: m?todos y aplicaciones"
library(ggfortify) # Herramientas de trazado unificado para estad?sticas de uso com?n, como GLM, series temporales, familias de PCA, agrupamiento y an?lisis de supervivencia
library(lmtest) # Una colecci?n de pruebas, conjuntos de datos y ejemplos para la verificaci?n de diagn?stico en modelos de regresi?n lineal
library(uroot) #Ra?ces unitarias estacionales y pruebas de estabilidad estacional.

#Importamos la informaci?n del YahooFinance
getSymbols(Symbols = c('MSFT'),
           from = "2010-01-01",
           to = "2022-01-03")

#Con el paquete lubridate vamos a manipular las fechas para trabajar la serie de datos
Sys.getlocale("LC_TIME") #Para ver la configuraci?n regional por la fecha y s?mbolos

#Asignamos el dataframe a trabajar
Cierre <- data.frame(MSFT$MSFT.Close)
Cierre <- data.frame(Cierre,rownames(Cierre))
colnames(Cierre) <- append('Precio','fecha')
str(Cierre)

# transformando a tipo serie
Cierre.ts<-ts(Cierre$Precio,start=c(2010,01),end=c(2022,01),frequency=12)
plot(Cierre.ts)

Cierre$fecha <- as.Date(Cierre$fecha,format="%d/%m/%Y") # transformando a formato fecha
class(Cierre$fecha)
str(Cierre.ts)

# Validando test de tuckey parea detectar estacionalidad
# ------------------------
boxplot(split(Cierre.ts, cycle(Cierre.ts)), names = month.abb, col = "blue",
        xlab = "Periodos",main = "Microsoft")
#Gr?ficamente no hay estacionalidad

# Se detecta correctamente un cambio de nivel y un cambio temporal.
outlier.cierre <- tso(Cierre.ts,types = c("AO","LS","TC"),maxit.iloop = 15)#sacamos muestras de cada 15 puntos
outlier.cierre
summary(outlier.cierre)
length(outlier.cierre$outliers)
plot(outlier.cierre)
##########################################

#Para identificar la forma de la distribuci?n de la serie, recurrimos primero a graficar el histograma
#de frecuencias de las observaciones, luego ajustamos por distribuci?n de probabilidad.
hist(Cierre$Precio, probability = T, main = "Histograma del Precio mensual de la acci?n de Microsoft")
lines(density(Cierre$Precio),col="green",lwd=3)
curve(dnorm(x,mean=mean(Cierre$Precio),sd=sd(Cierre$Precio)), from=800,to=1600, add=TRUE, col="blue", lwd=2)

W <- density(Cierre$Precio) 
plot(W, main="Densidad de kernel") #Para visualizar a que distribuci?n se podria ajustar
polygon(W, col="blue", border="red")

#Para verificar si la serie es estacionaria o no
autoplot(Cierre.ts, ts.colour="blue", ts.linetype="dashed", title= "Precio mensual de acc??n", 
                 ylab="Microsoft", xlab="A?os")

memsft <- mean(Cierre$Precio) #la media es constante, pero no es cero 
sdmsft <- sd(Cierre$Precio) #pero la varianza es constante. 
#Se puede decir que hay estacionariedad debil

#Descomponemos la serie
Cierre.ts.desc <- decompose(Cierre.ts) #se observa tendencia creciente, y estacionalidad
autoplot(Cierre.ts.desc)

#Gr?ficamente se evidencia tendencia, estacionalidad y heterocedasticidad en la parte aleatoria.

#############################################################
############ Enfoque de Descomposicion ######################
#############################################################

Cierre.desc_m <-  decompose(Cierre.ts, type = 'multiplicative')

Cierre.desc_a <-  decompose(Cierre.ts, type = 'additive')

#Grafico de descomposici?n de la serie
plot(Cierre.ts.desc, xlab="A?o")
#Es un modelo aditivo (x+s+a)

### Test de Raices Unitarias ##
###############################
#library(uroot) 
#ADF.test(Cierre.ts,selectlags=list(mode=c(1,2,3,4,5,6,7,8),Pmax=8),itsd=c(1,0,0))
#??uroot

##  Augmented Dickey-Fuller Test:
plot(Cierre.ts)
adf.test((Cierre.ts),alternative = "stationary")
test <- adf.test((Cierre.ts),alternative = "stationary")
test$p.value # p = 0.6774 >0.05 tiene r.u. x lo tanto no es estacionaria
# H0: la st tiene r.u. x lo tanto no es estacionaria
# H1: la st no tiene r.u. x lo tanto  es estacionaria

########################################################
### Eliminando componentes en la serie de tiempo #######
########################################################

# A la serie original,le quitamos la componente de estacionalidad,
# nos quedamos solo con la tendencia.
# En primer lugar, vamos a eliminar la heterocedasticidad 
# con una transformaci?n logar?tmica, 
# la cual suele inducir normalidad y linealidad:
# -------------------------------------------------------

Tendencia_Cierre <- (Cierre.ts)
plot(Tendencia_Cierre)
adf.test(Tendencia_Cierre)

# Ahora, para conseguir la estacionariedad en la media, podemos 
# aplicar una diferenciaci?n a la serie. 
# Hay que resaltar, que es frecuente combinar la transformaci?n 
# logar?tmica con la diferenciaci?n para 
# conseguir una serie estacionaria (en media y varianza)
# -------------------------------------------------------

data_Cierre <- diff(Tendencia_Cierre)
plot(data_Cierre)
adf.test(data_Cierre)
#Se logra estacionariedad con un rezago

data_Cierre2 <- diff(Cierre.ts)
plot(data_Cierre2)
adf.test(data_Cierre2)
#Se logra estacionariedad con solo un rezago sobre la data observada

#Queda duda si existe estacionariedad (la distribuci?n deberia ser normal)
acf(Cierre.ts) #el gr?fico es netamente decreciente por lo que no requiere la parte AR
#pacf(Cierre.ts,30,main="",sub="Funci?n de Autocorrelaci?n Parcial")

## Estudiando las componentes de una serie temporal mediante modelos autoregresivos
## Para hacer un ajuste automático del mejor modelo usaremos la función
## auto.arima de la libreria forecast
fit <- auto.arima(Cierre.ts, ic = 'aicc')
fit

##Los resultados me indican que mi mejor modelo estacionario es un arima(0,1,0) 
x11()
plot(Cierre.ts)
lines(fitted(fit),col='red')

#Estimaci?n del modelo

#Modelo 1
arima1<-arima(Cierre.ts,order=c(1,1,0))
arima1

coeftest(arima1)

#El modelo 1 nos indica que no es estacionario.
#Por lo tanto, el mejor modelo estacionario es un arima(0,1,0)

##############################################
####Estacionariedad de los residuales#########
##############################################

plot.ts(arima1$residuals,sub="Residuales del modelo AR(1)", xlab="Tiempo",ylab="Residuales")  

#Autocorrelaci?n de los residuales
acf(arima1$residuals,sub="Autocorrelaciones de los Residuales del modelo AR(1)", xlab="Tiempo",ylab="Autocorrelaci?n")
pacf(arima1$residuals,sub="Autocorrelaciones Parciales de los Residuales del modelo AR(1)", xlab="Tiempo",ylab="Autocorrelaci?n")
#No hay autocorrelaci?n entre los residuos

#Normalidad de los residuales
qqnorm(arima1$residuals, sub="Gr?fico Q para evaluar normalidad");qqline(arima1$residuals)
#El modelo se presenta como una buena opci?n pues se ajusta como una distibuci?n normal
#a la serie observada.


#####ESTIMACI?N######
###########
###1 ETS###
#Estimamos un modelo de suavizado exponencial (ETS) usando la funci?n ets con los 
#par?metros por defecto.
fit_ets_default <- ets(Cierre.ts)
errores_fit_ets_default <- residuals(fit_ets_default)
checkresiduals(errores_fit_ets_default)

#Luego pasamos el modelo como input para una predicci?n (usando la funci?n textsl{forecast}) para los pr?ximos 12 meses.
fcast_ets_default <- forecast(fit_ets_default, h = 24)

#Finalmente dibujamos la predicci?n con la funci?n plot.
plot(fcast_ets_default)
summary(fcast_ets_default)
#fcast_ets_default[["mean"]]
#Media de proyecciones= 

###1.1 ETS CON TENDENCIA ####

#Creamos el primer modelo con la funcion ets con tendencia y la llamamos fit_ets_damped_trend. 
fit_ets_damped_trend <- ets(Cierre.ts, damped = TRUE)

#Sacamos la predicci?n de ?ste para los pr?ximos 12 meses.
fcast_ets_damped_trend <- forecast(fit_ets_damped_trend, h = 24)

#Por ?ltimo, dibujamos la serie con su predicci?n.
plot(fcast_ets_damped_trend)


####1.2 SIN TENDENCIA ####

#Creamos el primer modelo con la funcion ets sin tendencia y la llamamos fit_ets_no_trend.
fit_ets_no_trend <- ets(Cierre.ts, model = "ZNZ")

#Sacamos la predicci?n de ?ste para los pr?ximos 12 meses.
fcast_ets_no_trend <- forecast(fit_ets_no_trend, h = 24)

#Por ?ltimo, dibujamos la serie con su predicci?n.
plot(fcast_ets_no_trend)


##2. STL (Con estacionalidad y autorregresivo(1))
tsmod <- stlm(Cierre.ts, modelfunction = ar)
plot(forecast(tsmod, h = 24))









