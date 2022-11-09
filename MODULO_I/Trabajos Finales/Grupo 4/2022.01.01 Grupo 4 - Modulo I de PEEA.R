############################################################
#                   MÓDULO I                               #
############################################################

############################################################
# Módulo I   : Nivelación de Software Skill
# Software   : R
# Tema       : Series de Tiempo y Financieras
# Packages   : quantmod
#              fPortfolio
#              PerformanceAnalytics

# Integrantes: CHECYA PARAVECINO, SHERMELY
#              PISFIL BENITES, NILTHON IVAN
#              PRADILLO LUGO, CARLOS EDUARDO
#              TORRES GARCÍA, CARLOS EDUARDO
#              SEGURA VERANO, MIGUEL ANTONIO
#############################################################

rm(list = ls()) # Limpiamos el workspace
cat("\014")     # Limpiamos la consola

# determinar el directorio a trabajar
setwd("D:/datos/uni/especializacion/expo")
getwd()

# Instalacion de Librerias
#install.packages("quantmod")
#install.packages("timeSeries")
#install.packages("tseries")
#install.packages("fBasics")
#install.packages("fAssets")
#install.packages("fPortfolio")
#install.packages("PerformanceAnalytics")
#install.packages("PortfolioAnalytics")
#install.packages("sos")

# Cargar librerias
library(quantmod)
library(timeSeries)
library(tseries)
library(fBasics)
library(fAssets)
library(fPortfolio)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(sos)

#######################################

# Descripcion de los Package
listDescription(quantmod)
listDescription(fPortfolio)
listDescription(PerformanceAnalytics)

# Creamos un vector con los packages
Grupo_4 <- c("quantmod","fPortfolio","PerformanceAnalytics")

# Consultar con el profesor el por qué no se muestran los resultados del "FOR"
# for (i in Grupo_4) {
#  length(ls.str(paste0('package:',i), mode='function'))
#}

# Mostramos las funciones y cantidad de funciones que tiene cada package 
length(ls.str('package:quantmod', mode='function'))    # tiene 200 funciones 
lsf.str("package:quantmod") 
ls("package:quantmod")

length(ls.str('package:fPortfolio', mode='function'))  # tiene 374 funciones
lsf.str("package:fPortfolio") 
ls("package:fPortfolio")

length(ls.str('package:PerformanceAnalytics', mode='function')) # tiene 205 funciones
lsf.str("package:PerformanceAnalytics") 
ls("package:PerformanceAnalytics") 

# Este codigo busca a que Package instalado pertenece la funcion
help.search("fPortfolio") # Este es un ejemplo, Aqui se agregan las funciones que no se conocen
??fPortfolio              # Este es un ejemplo, Aqui se agregan las funciones que no se conocen

# Este codigo busca a que Package instalado o no instalado pertenece la funcion
findFn("fPortfolio")                           # Este es un ejemplo, Aqui se agregan las funciones que no se conocen
RSiteSearch("fPortfolio",restrict="functions") # Este es un ejemplo, Aqui se agregan las funciones que no se conocen
# Tambien se puede buscar las funciones de los packages en https://rseek.org/

################################################################################

# Identificamos a 5 empresas de tecnologia que cotizan en bolsa
empresas_tecnologias<-c("TSLA","AAPL","DIS","GOOG","FB")

# Descargamos la data de Yahoo Finance con getSymbols
getSymbols(empresas_tecnologias, from="2018-01-01", to="2022-04-29")

# Mostramos los primeros datos de cada empresa (TESLA)
head(TSLA)

# Obtenemos un grafico de series de tiempo
chartSeries(Cl(TSLA),theme = chartTheme("white"))
chartSeries(Cl(TSLA),theme = chartTheme("black"))

# Juntamos en una dataframe el precio al cierre ajustado de las acciones
cotizan<- merge.xts(TSLA[,6],AAPL[,6],DIS[,6],GOOG[,6],FB[,6])

# Encontramos otra fomra de crear el dataframe con los precios ajustado

# Creamos un Portafolio vacio, es decir,lleno de NULL
cotizan_2<- NULL

# Descargamos el precio al cierre ajustado de las acciones (columna 6) y las agregamos al Portafolio (cotizan_2)
for(nom in empresas_tecnologias) {
  cotizan_2<- cbind(cotizan_2,getSymbols(nom, src="yahoo",from = "2018-01-01", to = "2022-03-31", periodicity = "daily",auto.assign=FALSE)[,c(6)])
}

# Colocamos el nombre de las empresas
colnames(cotizan)<-c("TSLA","AAPL","FB","GOOG","DIS")

# Visualizamos las primeras filas
head(cotizan)

# Presentamos graficamente las acciones en una serie (TSLA)
cotizan.ts <-as.timeSeries(cotizan)
seriesPlot(cotizan.ts[,'TSLA'])
returnPlot(cotizan.ts[,'TSLA'])

par(mfcol=c(3,3))
seriesPlot(cotizan.ts)

# Obtenemos los retorno de las acciones
cartera_retornos<- returns(cotizan)

# Mostramos las primeras filas del dataframe
head(cartera_retornos)

# Eliminanmos el dato perdido, la primera fila
cartera_retornos<-cartera_retornos[-1,]

# Mostramos las primeras filas del dataframe sin la primera fila, se elimino en la 
head(cartera_retornos)

# Mostramos estadisticos basicos
basicStats(cartera_retornos)

# Mostramos la cartera eficiente
cartera_optima <- portfolio.optim(cartera_retornos)

# Mostramos la Ponderacion de la cartera
ponderacion<-cartera_optima$pw

# Insertamos los nombres de las empresas
names(ponderacion)<-colnames(cartera_retornos)
ponderacion

# Comparamos el comportamiento de las series de DIS y AAPL
seriesPlot(cotizan.ts[,'AAPL'])
seriesPlot(cotizan.ts[,'DIS'])

cartera_optima$pm
cartera_optima$ps

# Graficamos la cartera de retornos
chart.CumReturns(cartera_retornos, legend.loc = 'topleft', main = '')

# Graficamos la frontera de eficiencia 
plot(portfolioFrontier(as.timeSeries(cartera_retornos)),which=1)
plot(portfolioFrontier(as.timeSeries(cartera_retornos)),which=3)

# Realizamos un analisis tecnico con bandas de bollinger
# Mostramos el volumen de las transacciones y el movimiento promedio

# Las bandas de Bollinger indicador gráfico utilizado en el analisis tecnico 
# para establecer un rango de precios de un activo.

chartSeries(TSLA, theme = chartTheme('white'),
            TA=c(addVo(),addBBands(), addMACD()))

chartSeries(AAPL, theme = chartTheme('white'),
            TA=c(addVo(),addBBands(), addMACD()))

chartSeries(DIS, theme = chartTheme('white'),
            TA=c(addVo(),addBBands(), addMACD()))

chartSeries(GOOG, theme = chartTheme('white'),
            TA=c(addVo(),addBBands(), addMACD()))

chartSeries(FB, theme = chartTheme('white'),
            TA=c(addVo(),addBBands(), addMACD()))

# Gracias totales
