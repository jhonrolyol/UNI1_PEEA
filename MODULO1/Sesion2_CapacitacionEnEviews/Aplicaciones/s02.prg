'============================
' Sesi�n 02: Introducci�n a Eviews
' An�lisis Estad�stico
' Fecha: 24/09/2022
'============================

'=========================
'Paso 0: Creaci�n del workfile
'=========================
wf u 1000

'Funciones de distribuci�n de densidad
'Funciones de distribuci�n acumuladas

for !c = -10 to 10 
	vector(21) densnorm(!c+11) = @dnorm(!c/2)
	vector(21) cumnorm(!c+11)  = @cnorm(!c/2)
next

'Graficar vectores
freeze(g1)  densnorm.line
freeze(g2)  cumnorm.line

'Combinar gr�ficos
graph g3.merge  g1  g2
g3.save(t = gif)   "Gr�fico 1"

'Funcion de distribuci�n inversa (quantil)
scalar p5 = @qnorm(0.05/2)

'================================
'Generaci�n de n�meros aleatorios
'================================
'Definir una semilla de aleaoriedad
rndseed 1911

series rnd_norm = @rnorm*3 + 2
series rnd_chisq = @rchisq(3)
series rnd_tdist   = @rtdist(5)

group variables rnd_norm rnd_chisq rnd_tdist 
freeze(g4) variables.distplot(m) hist(anchor=0, scale=dens) kernel(k=e, ngrid=100) theory()

'==============================
'Pruebas de hip�tesis
'==============================
'Test de Jarque Bera

'Paso 1: Ho: Hay normalidad de la serie
'Paso 2: Calcular el estad�grafo
!s = @skew(rnd_norm)
!k = @kurt(rnd_norm)
!n = @obs(rnd_norm) 

scalar JB = (!s^2 + ((!k-3)^2)/4)*(!n/6)

'Paso 3: Comparar
rnd_norm.hist
rnd_norm.distplot(m) hist() kernel() theory()

scalar chisqtabla = @qchisq(0.95,2)
	' JB = 1.22 vs ChisqTabla = 5.99 
	' Cae en la zona de no rechazo

	'Calcular el p-value 
	scalar p_value = 1 - @cchisq(JB,2)
	'p-value  < 0.05 => Rechazo H0 
	'p-value es 0.54, por ende NO RECHAZO Ho

'Paso 4: Concluir
	'Con un nivel de confianza de 95%, el valor de JB
	' no supera el chisq en tabla, por ende no se rechaza Ho
		
