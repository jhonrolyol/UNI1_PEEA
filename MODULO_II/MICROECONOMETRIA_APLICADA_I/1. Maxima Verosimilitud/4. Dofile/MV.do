********************************************************************************
************Método de Máxima Verosimilitud para Modelos Lineales****************
********************************************************************************
sysuse auto, clear	/*Abre base de datos del sistema de Stata. Para correr una línea, utilizamos ctrl+d*/

***Estimación por máxima verosimilitud. Aqui debemos tomar en cuenta que, si asumimos
***que la variable de interés sigue una distribución normal, es necesario incluir
***el parámetro sigma dentro de los parámetros a estimar.
***Normalden es un comando que permite evaluar la distribuciòn normal
***en la variable y con media xb y desviación estánbdar sigma.

*program drop lfmv
program lfmv
  args lnf xb sigma
  local y "$ML_y1"
  quietly replace `lnf' = ln(normalden(`y', `xb',`sigma'))
end

ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:)
ml maximize

*ml graph
predict u, stdp

**Si lo comparamos con un modelo de regresión lineal estimado por MCO, encontramos
**que el parámetro beta es el mismo que por MV. Situación distinta para sigma.
**Los errores estándar también son distintos, debido a que el método de MV fuerza
**la estimación de errores que sigan la distribución deseada:

reg rep78 headroom mpg
predict e, stdp
hist u, normal
hist e, normal

********************************************************************************
*******************************Test Estadísticos********************************
********************************************************************************

***1. Test de Wald
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:)
ml max

test headroom
test headroom=5
test headroom=5,accumulate
test headroom=mpg,accumulate	/*Esto nos permitirá agregar más hipótesis*/
test mpg,accumulate

test headroom=5
test mpg=2,accum

***2. Test de Ratio de Verosimilitud
**Caso 1: Una restricción
*Modelo irrestricto
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:)
ml max

lrtest, saving(a1)

*Modelo restricto
ml model lf lfmv (xb: rep78 = ) (sigma:)
ml max

lrtest,using(a1)

**Caso 2: Más de una restricción de nulidad:
*Modelo irrestricto
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:)
ml max

estimates store A

*Modelo restricto
ml model lf lfmv (xb: rep78 = ) (sigma:)
ml max

estimates store B
lrtest A B

**Caso 3: Más de una restricción (no necesariamente de nulidad):
*Modelo irrestricto
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:)
ml max

lrtest, saving(a3)

*Modelo restricto
*constraint drop _all
constraint define 1 [xb]headroom=2
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:),constraint(1)
ml max

lrtest,using(a3)

*constraint define 2 [sigma]_cons=1	/*En caso se desee incluir al sigma entre
*las restricciones*/

***3. Test del Score
**Esta opción no se encuentra disponible en Stata, pero es posible programarla

*Modelo restricto
constraint define 1 [xb]headroom=2
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:),constraint(1)
ml max

*ereturn list
matrix b0=e(b)

*Inclusión de parámetros en el modelo irrestricto (sin maximizar)
ml model lf lfmv (xb: rep78 = headroom mpg) (sigma:), maximize init(b0) iter(0)

matrix S=e(gradient)*e(V)*e(gradient)'
scalar s=el(S,1,1)
scalar p_s=chi2tail(1,s)

*Mostrar resultados
di as txt "chi2(1)     = " as result %9.2g `=s'
di as txt "Prob > chi2 = " as result %9.4g `=p_s'
