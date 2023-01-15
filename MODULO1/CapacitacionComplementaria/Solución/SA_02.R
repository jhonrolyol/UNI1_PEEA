# ===========================================
# Sesión Adicional: Web Scraping
# Método: APIs (XML y JSON)
# ===========================================
rm(list=ls())

# ========================================
# API: JSON
# ========================================
indicador <- c("PN01728AM")
fecinicial <- "2003-1"
fecfinal <- "2022-05"
  
url <- paste0("https://estadisticas.bcrp.gob.pe/estadisticas/series/api",
              "/",indicador,"/json/",fecinicial,"/",fecfinal)

# install.packages("jsonlite")
library(jsonlite)
obJSON <- jsonlite::fromJSON(url)
dt_datos <- obJSON$periods %>% data.table()
colnames(dt_datos) <- c("Fecha","PBI")
sapply(dt_datos,class)

dt_datos[,PBI := as.numeric(PBI)]

# ========================================
# API: XML
# ========================================
library(httr)
library(XML)

direccion <- "Congreso de la republica,Peru"
direccion_encode <- URLencode(direccion)

url <- paste0("https://nominatim.openstreetmap.org/search?format=",
              "xml&q=",direccion_encode)

objHTML <- httr::GET(url)
prs_XML <- XML::xmlParse(objHTML)
l_XML <- XML::xmlToList(prs_XML)

# Latitud 
l_XML$place[["lat"]]
# Longitud 
l_XML$place[["lon"]]
# Dirección Exacta
nombre_completo <- l_XML$place[["display_name"]]

library(stringr)
str_conv(nombre_completo,encoding ="UTF-8")
dt_maps <- data.table(t(sapply(l_XML$place,c)))

