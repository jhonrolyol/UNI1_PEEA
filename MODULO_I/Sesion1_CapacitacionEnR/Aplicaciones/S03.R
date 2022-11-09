# ==================================
# Sesión 01: Introducción a R
# Manejo de librerías
# Fecha: 23/04/2022
# ==================================
rm(list = ls())

# ==================================================
# Instalación de Librerías
# (Se realiza sólo una vez en el mismo ordenador)
# ==================================================
# install.packages("quantmod")

# ==================================================
# Cargar librerías
# (Se realiza siempre que van utilizar la función de dicha librería)
# ==================================================
library(quantmod)
getSymbols(Symbols = c("FB","AAPL","GOOG"),
           from = "2016-01-01",
           to = "2022-03-31")


barChart(AAPL)
addBBands()
addMACD()

getSymbols(Symbols = "GDPC1",
           src = "FRED")
