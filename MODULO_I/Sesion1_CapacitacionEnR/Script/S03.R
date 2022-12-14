# ==================================
# Sesi?n 01: Introducci?n a R
# Manejo de librer?as
# Fecha: 23/04/2022
# ==================================
rm(list = ls())
        
# ==================================================
# Instalaci?n de Librer?as
# (Se realiza s?lo una vez en el mismo ordenador)
# ==================================================
# install.packages("quantmod")

# ==================================================
# Cargar librer?as
# (Se realiza siempre que van utilizar la funci?n de dicha librer?a)
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
