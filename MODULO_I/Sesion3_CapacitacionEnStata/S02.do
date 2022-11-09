*===================================
* Introducción a Stata
* Manejo de base de datos
*===================================
clear 

*===================================
*Definir espacio de trabajo
*===================================
*Ruta 
cd "C:\Users\rpere\Desktop\Online\UNI\PEEA\PEEA XII\03 Stata\Aplicación\E02"

*Crear la bitácora
	*Cerrar bitácora abierta
	capture log close 
	*Creación del log
	log using "S02.txt",replace
	
*=====================================
* 01 Importación de datos
*=====================================
unicode encoding set latin1
unicode translate *dta
use sumaria-2020.dta, replace

*=====================================
* 02 Análisis de la información
*=====================================
* Mostrar datos
	*percepho: total de perceptores de ingresos
	*mieperho: total de miembros del hogar
	*inghog1d: ingreso bruto
	*inghog2d: ingreso neto total
	*dominio
	*estrato: estrato geográfico
	
	list dominio estrato mieperho percepho
	list dominio estrato mieperho percepho in 10/20	
	display _N
	
*Creación de variables
	* Ingreso promedio por perceptor de ingreso
	generate ing_prom1d = inghog1d/percepho
	gen ing_prom2d = inghog2d/percepho
	
	list ing_prom1d ing_prom2d in 100/200

	* Ingreso promedio por miembros del hogar 
	gen ing_prom1d_m = inghog1d/mieperho
	gen ing_prom2d_m = inghog2d/mieperho
	
	list ing_prom1d_m ing_prom2d_m in 100/200

	* Crear variable que me indique si el número de miembros del hogar es igual al de perceptores de ingresos 
	gen flag1 = 0 
	replace flag1 = 1 if mieperho == percepho
	list mieperho percepho flag1
	summarize flag1
	
	* Creación de variables superhabitados
	gen flag_overpop = 1 if mieperho >= 6
	replace flag_overpop = 0 if mieperho < 6
	
	summarize flag_overpop 
	
	* Agrupamiento de información
	sort flag_overpop
	by flag_overpop: summarize inghog1d, detail

	* Pobreza 
	codebook pobreza
	sort flag_overpop pobreza
	by flag_overpop pobreza: summarize inghog1d
	
	* Etiqueta de variables y categorías
	label variable flag_overpop  "Hogares con al menos 6 miembros"
	
	* Etiqueta de valores
	label define etiquetas  0 "Hogares con menos de 6 miembros" 1 "Hogares con 6 o más miembros"
	
		*Anexar a la variables la etiqueta
		label values flag_overpop etiquetas
		codebook flag_overpop
		