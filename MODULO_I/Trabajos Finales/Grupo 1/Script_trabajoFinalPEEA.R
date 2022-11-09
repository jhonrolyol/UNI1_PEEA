
#####DIPLOMADO:ECONOMETRÍA APLICADA
#####TEMA:DETERMINANTES SOCIOECONÓMICOS DEL ACCESO AL AGUA DE CALIDAD EN EL PERU,2019
#####ALUMNOS:
#CARPIO SANCHEZ, EUGENIA CECILIA
#HERNÁNDEZ VÁSQUEZ , AKRAM ABDUL
#VASQUEZ FLORES, YENI VIOLETA
#GARCÍA AGUILAR, FÉLIX ANGEL
#RIOS-LUNA RUIZ MARCO BRUNO
#ALEY APARCANA, KEVIN ERICK


# Cargamos las librerias necesarias
# Activar cuando no se tienen instaladas
install.packages("tidyverse")
install.packages("haven")
install.packages("foreign")
install.packages("car")
install.packages("ROCR")
install.packages("dplyr")
install.packages("epiDisplay")
install.packages("caTools")
install.packages("expss")
install.packages("MLmetrics")

#Llamar a las librerias 
library(tidyverse)
library(haven)
library(foreign)
library(car)
library(ROCR)
library(dplyr)
library(epiDisplay)
library(caTools)
library(expss)
library(MLmetrics)

# ===================================
# IMPORTANDO LOS DATOS
# ===================================

# Indicando el directorio de trabajo y verificamos si el dictorio especificado es el correcto
#setwd('C:/Users/Darwin/Documents/UNI/MODULO I/Clase2/TrabajoFinalR')
getwd()

# Descargando los módulo 100 y 34 desde la web de microdatos del INEI
url <- "http://iinei.inei.gob.pe/iinei/srienaho/descarga/STATA/687-Modulo01.zip"
download.file(url = url,
              destfile = "Modulo01.zip",
              mode = "wb")

url <- "http://iinei.inei.gob.pe/iinei/srienaho/descarga/STATA/687-Modulo34.zip"
download.file(url = url,
              destfile = "Modulo34.zip",
              mode = "wb")

# Descomprimiendo los las dos carpetas descargadas
unzip(zipfile="./Modulo01.zip",exdir="./data")
unzip(zipfile="./Modulo34.zip",exdir="./data")

# ======================================
#PAQUETE FOREIGN
# ======================================
#La importancia de este paquete es que permite importar bases de datos de:
#'SPSS', 'Stata', 'Systat', 'Weka', 'dBase', 'Minitab', 'S', 'SAS', , y más

#Importando las bases de datos en formato dta (Stata) utilizando el paquete foreign
sumaria <- read_dta("data/687-Modulo34/sumaria-2019.dta")
base100 <- read_dta("data/687-Modulo01/enaho01-2019-100.dta")

#Unimos las bases de datos importadas(sumaria y modulo 100)
base2019_final <- left_join(sumaria, base100, by=c("conglome", "vivienda", "hogar", "ubigeo"))


#Exploramos algunas caracteristicas de la base de datos y variables
head(base2019_final)
table(base2019_final$pobreza, useNA = "alw")
table(base2019_final$estrato.x, useNA = "alw")
table(base2019_final$conglome, useNA = "alw")
table(base2019_final$factor07.x, useNA = "alw")
table(base2019_final$result, useNA = "alw")
table(base2019_final$p110a, useNA = "alw")
table(base2019_final$p110a, useNA = "alw")
table(base2019_final$ubigeo.x, useNA = "alw")
table(base2019_final$dominio.x, useNA = "alw")
table(base2019_final$nbi2, useNA = "alw")
table(base2019_final$mieperho, useNA = "alw")

#Liberamos memoria, eliminando las bases que ya no se usaran
rm(list = c("base100", "sumaria"))

# Filtramos la base de datos final para incluir solo a los hogares encuestados
base2019_final <- filter(base2019_final, base2019_final$result == 1)


# ================================================================================================
# Calculamos y recodificamos las variables seseccionadas para el análisis econométrico posterior
# ================================================================================================

# Calculamos la variable concentracion adecuada de cloro 
str(base2019_final$p110a)
base2019_final <- base2019_final %>%
  mutate(cloro = ifelse(p110a==2 | p110a==3, 0,
                        ifelse(p110a==1, 1, NA)))

var_lab(base2019_final$cloro) = "Cantidad de cloro"
val_lab(base2019_final$cloro) = num_lab("
             0 Inadecuada    
             1 Adecuada   
")

# Filtramos la base de datos final para excluir los mising de la varible cloro
base2019_final <- filter(base2019_final, base2019_final$cloro<= 1)

# Tabulamos la variable concentracion adecuada de cloro
table(base2019_final$cloro, useNA = "alw")

# Creamos la variable area de residencia 
str(base2019_final$estrato.x)
base2019_final <- base2019_final %>%
  mutate(area = ifelse(estrato.x>0 & estrato.x<=5, 0,
                       ifelse(estrato.x>=6 & estrato.x<=8, 1, NA)))

var_lab(base2019_final$area) = "Area de residencia"
val_lab(base2019_final$area) = num_lab("
             0 Urbana    
             1 Rural   
")

# Tabulamos la variable area
table(base2019_final$area, useNA = "alw")

# Creamos la variable pobre 
str(base2019_final$pobreza)
base2019_final <- base2019_final %>%
  mutate(pobre = ifelse(pobreza==1, 0,
                        ifelse(pobreza==2, 1,
                               ifelse(pobreza==3, 2, NA
                               ))))

var_lab(base2019_final$pobre) = "Pobreza del hogar"
val_lab(base2019_final$pobre) = num_lab("
             0 Muy pobre    
             1 Pobre
             2 No pobre
")

# Tabulamos la variable pobre
table(base2019_final$pobre, useNA = "alw")


# Creamos la variable region natural
str(base2019_final$dominio.x)
base2019_final <- base2019_final %>%
  mutate(region_nat = ifelse(dominio.x==1 | dominio.x==2 | dominio.x==3 | dominio.x==8, 0,
                             ifelse(dominio.x==4 | dominio.x==5 | dominio.x==6, 1,
                                    ifelse(dominio.x==7, 2, NA
                                    ))))

var_lab(base2019_final$region_nat) = "Region natural"
val_lab(base2019_final$region_nat) = num_lab("
             0 Costa    
             1 Sierra
             2 Selva
")

# Tabulamos la variable region natural
table(base2019_final$region_nat, useNA = "alw")

# Calculamos la variable hacinamiento 
str(base2019_final$nbi2)
base2019_final <- base2019_final %>%
  mutate(hacinamiento = ifelse(nbi2==0, 0,
                               ifelse(nbi2==1, 1, NA)))

var_lab(base2019_final$hacinamiento) = "Hacinamiento"
val_lab(base2019_final$hacinamiento) = num_lab("
             0 Sin hacinamiento    
             1 Hacinamiento   
")

# Tabulamos la variable hacinamiento
table(base2019_final$hacinamiento, useNA = "alw")


# ======================================
# PAQUETE STATS
# ======================================
#Exlicar la importancia del paquete de manera general
#Se han realizando la especificación y estimación de 6 modelos logísticos binomiales y 6 modelos probit binomiales
#Como variable dependiente tenemos: la concentracion adecuada de cloro (0:Inadecuada; 1:Adecuada)
#Como variables independientes:area(0:Urbana; 1:Rural),pobre(0:Muy pobre;1:Pobre;2:No pobre), 
#region_nat(0:Costa;1:Sierra;2:Selva)
#hacinamiento(0:Sin hacinamiento;1:Hacinamiento),mieperho

#Modelo logístico 

modelo1 <- glm(cloro ~ area,
               family=binomial(link='logit'), data=base2019_final)
modelo2 <- glm(cloro ~ factor(pobre),
               family=binomial(link='logit'), data=base2019_final)
modelo3 <- glm(cloro ~ factor(region_nat),
               family=binomial(link='logit'), data=base2019_final)
modelo4 <- glm(cloro ~ hacinamiento,
               family=binomial(link='logit'), data=base2019_final)
modelo5 <- glm(cloro ~ factor(area) + factor(pobre) + factor(region_nat) + factor(hacinamiento),
               family=binomial(link='logit'), data=base2019_final)
modelo6 <- glm(cloro ~ factor(area) + factor(pobre) + factor(region_nat) + factor(hacinamiento) + factor(region_nat) * factor(area) + mieperho,
               family=binomial(link='logit'), data=base2019_final)
logistic.display(modelo1)
summary(modelo1)
logistic.display(modelo2)
summary(modelo2)
logistic.display(modelo3)
summary(modelo3)
logistic.display(modelo4)
summary(modelo4)
logistic.display(modelo5)
summary(modelo5)
logistic.display(modelo6)
summary(modelo6)

#Modelo probit 
modelo7 <- glm(cloro ~ area,
               family=binomial(link='probit'), data=base2019_final)
modelo8 <- glm(cloro ~ factor(pobre),
               family=binomial(link='probit'), data=base2019_final)
modelo9 <- glm(cloro ~ factor(region_nat),
               family=binomial(link='probit'), data=base2019_final)
modelo10 <- glm(cloro ~ hacinamiento,
                family=binomial(link='probit'), data=base2019_final)
modelo11 <- glm(cloro ~ area + factor(pobre) + factor(region_nat) + factor(hacinamiento),
                family=binomial(link='probit'), data=base2019_final)
modelo12 <- glm(cloro ~ factor(area) + factor(pobre) + factor(region_nat) + factor(hacinamiento) + factor(region_nat) * factor(area)+mieperho,
                family=binomial(link='probit'), data=base2019_final)
probit.display(modelo7)
summary(modelo7)
probit.display(modelo8)
summary(modelo8)
probit.display(modelo9)
summary(modelo9)
probit.display(modelo10)
summary(modelo10)
probit.display(modelo11)
summary(modelo11)
probit.display(modelo12)
summary(modelo12)

#LR test (Razón de verosimilitud)
#Se evalúa si las variables explicativas factor(region_nat) * factor(area) y mieperho  mejoran el modelo
#H0:factor(region_nat) * factor(area) y mieperho no mejoran el modelo
#H1: al menos una de ellas si mejora el modelo
lrtest(modelo11, modelo12)

# ======================================
# PAQUETE ROCR
# ======================================
#Exlicar la importancia del paquete de manera general
#Haciendo un nálisis de sensitividad y specificidad de los modelos logit y probit anteriormente estimados

#Curvas ROC y AUC en los modelos logit
par(mfrow = c(2,3))
lroc(modelo1, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo2, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo3, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo4, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo5, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo6, title=TRUE, auc.coords=c(.5,.1))

#Curvas ROC y AUC en los modelos probit
par(mfrow = c(2,3))
lroc(modelo7, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo8, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo9, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo10, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo11, title=TRUE, auc.coords=c(.5,.1))
lroc(modelo12, title=TRUE, auc.coords=c(.5,.1))


# ======================================
# PAQUETE MLMETRICS
# ======================================
#Exlicar la importancia del paquete de manera general
#Evaluando el nivel de predicción del modelo elegido
logreg <- glm(cloro ~ area + factor(pobre) + factor(region_nat) + factor(hacinamiento)+ mieperho + factor(region_nat) * factor(area),
              family=binomial(link = "logit"), data=base2019_final)
pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)
table(logreg$fitted.values, useNA = "alw")
Accuracy(y_pred = pred, y_true = base2019_final$cloro)






