*==================================
* Introducción Stata
* Fecha: 30/04/2022
*==================================
*Stata es case sensitive: discrimina entre minúsculas y maýusculas
*Limpiar variables
clear 

*==================================
* Definir nuestro espacio de trabajo
*==================================
*Establecer ruta de trabajo
cd  "C:\Users\rpere\Desktop\Online\UNI\PEEA\PEEA XII\03 Stata\Aplicación"
*Extraer ruta de trabajo actual
pwd
*Archivos en el directorio actual
dir

*Creación del log
	*Cerrando cualquier log existente
	capture log close
	
	*Creación del log
	log using "S01.txt", replace
	
*=====================================
* Creación de objetos
*=====================================
*Escalares (r)
scalar edad = 29
display edad
scalar nombre = "Richard"
display nombre

*Matrices (e)
	*3x1
	matrix A = (4\8\9)
	matrix list A

	*1x3 
	matrix B = (4,8,9)
	matrix list B
	
	*3x3
	matrix C= (4,8,9)\(1,2,3)\(4,5,6)
	matrix list C
	
	*Operaciones con matrices
		*Transpuesta
		matrix D= B'
		matrix list D
		
		*Multiplicación (AxB) [3x1][1x3] = [3x3]
		matrix E = A*B
		matrix list E
		
		*Determinante
		display det(C)
		
	*Cruzar matrices 
		*Abajo
		matrix A1 = A\D
		matrix list A1
		*Al costado
		matrix A2 = A,D
		matrix list A2
		
		*help matrix

*Otras operaciones con matrices
	*Calcular coeficientes (betas) para un MCO:
	* b = (X'X)^(-1)*X'Y
	
    *Cargar una base de datos 
	use http://www.stata-press.com/data/r9/auto.dta , replace
	display _N
	
	*Calcular betas por MCO
	matrix accum  xtx = weight foreign, 
	matrix list xtx
	*help matrix accum 

	matrix vecaccum ytx = price weight foreign
	matrix list ytx
	*help matrix vecaccum 
	
	matrix betas = invsym(xtx)*ytx'
	matrix list betas 
	
	*Regresión
	reg price weight foreign
	
*=============================
*Estructuras de control 
*Bucles y condicionales
*=============================
* Llamar a al variable: `' (Alt + 96)

* Bucles: Textos o Variables
	*Textos 
	foreach x in "Yeni" "Wilder" "Shermely"  {
		display length("`x'")
	}	
	
	*Variables
	foreach x of varlist  make price mpg {
		summarize `x'
	}
	
* Bucles: Repeticiones por números
	forvalues i = 10(5)40 {
		generate d`i' = 0 
	}
	
* Condicionales
local m = 1
forvalues i = 10(5)40 {
	if `i' == 35 {
		display "Se ha encontrado el número 35 en el orden `m'"
	}
	local m = `++m'
}

*==================================
*Análisis Estadístico
*==================================
*1) Ho: Hay normalidad 
*2) Calcular un estadígrafo
*	JB = (n/6)*(S^2 + ()(K-3)^2)/4)
	summarize price, detail
	*help summarize
	scalar JB  = (r(N)/6)*(r(skewness)^2 + ((r(kurtosis)-3)^2)/4)
	display JB
*3) Comparar con estadístico en tabla
	display invchi2(2,0.95)
	display 1-chi2(2,JB)
*4) Concluir:
	*El JB es 43.92, mayor que la chi2 en tabla, por lo tanto 
	*se rechaza normalidad de la variable price
	* p-value = 0.0000 < 0.05 => Rechazo Ho de normalidad
	
*Calcular variables aleatorias
generate var1 = runiform()
generate var2 = rnormal(0,1)
gen var3 = rchi2(5)
*help rnormal

*Gráfico de dispersión
graph matrix var1 var2 var3, half msize(1) mcolor(red)
help graph matrix







