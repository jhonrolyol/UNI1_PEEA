# ===========================================
# Sesión Adicional: Web Scraping
# Método: Instalar packages
# ===========================================
rm(list =ls())

library(data.table)
library(ggplot2)

# ====================================
# quantmod: Yahoo Finance
# ====================================
# install.packages("quantmod")
library(quantmod)

quantmod::getSymbols(c("MSFT","AMZN"),src = "yahoo",
                     from = "2016-01-01")

quantmod::candleChart(AMZN,subset = "last 3 months")

# FRED: Data US / Indicadores Macroeconómicos mundiales
quantmod::getSymbols(c("GDPC1","DEXUSEU"),src = "FRED")

# ====================================
# wbstats: World Bank
# ====================================
# install.packages("wbstats")
library(wbstats)

dt_countries <- data.table(wbstats::wb_cachelist$countries)
dt_countries[region == "Latin America & Caribbean",]

dt_indicadores <- data.table(wbstats::wb_cachelist$indicators)
dt_fuentes <- data.table(wbstats::wb_cachelist$sources)

# Descargar la información
v_pais <- c("ARG","BOL","CHL","PER","BRA","ECU","PRY","URY")
v_indicadores <- c("CC.EST")
# dt_indicadores[source_id == 3, ]

dt_datos <- wb_data(indicator = v_indicadores,
                    country = v_pais,
                    start_date = 2010,
                    end_date = 2020) %>% data.table()

ggplot(dt_datos) +
  geom_line(mapping = aes(x = date,y = CC.EST,color = country))+
  facet_wrap(.~country)

# Descargar PBI
v_indicadores <- c("NY.GDP.PCAP.KN","FP.CPI.TOTL.ZG")
dt_datos2 <- wb_data(indicator = v_indicadores,
                     country = v_pais,
                     start_date = 2010,
                     end_date = 2020) %>% data.table()

