'============================
' Sesión 02: Introducción a Eviews
' Importación y manejo de datos
' Fecha: 24/09/2022
'============================
'Borrar cualquier objeto que esté abierto
close @all

'Definir ruta de trabajo 
%ruta = @runpath 'Extrae la ruta donde esta guardado el program S03
cd  %ruta  'Cambiar el directorio actual por el de %ruta 
'cd  "C:\Users\rpere\Desktop\Online\UNI\PEEA\PEEA XII\02 Eviews\Aplicaciones"

'=============================
'Paso 0: Creación de workfile
'=============================
'wf m 2004.1  2020.7

'=============================
'Paso 1: Importar datos: read (1)
'=============================
'read(b2)  "BBDD_VariablesMacro.xls"  5

'=============================
'Paso 1: Importar datos: import (2)
'=============================
import BBDD_VariablesMacro.xlsx range=Hoja1  colhead = 2 namepos = first

'=============================
' Transformación de variables
'=============================
'Crecimiento interanual del PBI
'series pbi_crec =  pbi/pbi(-12) - 1
'graph g1.line pbi_crec
delete fecha
string lista = @wlookup("*","series")

for %j {lista}
	series {%j}_crec =  {%j}/{%j}(-12) - 1 
next  

'==============================
'Análisis Univariado
'==============================
'graph a1_g_pbi.line  pbi_crec
'freeze(a1_hist_pbi)   pbi_crec.hist

for %j {lista}
	graph a1_g_{%j}.line  {%j}_crec
	freeze(a1_hist_{%j})  {%j}_crec.hist
next  

'==============================
'Análisis Univariado
'==============================
'Gráficos
graph a2_g.line  tc pbi_crec
'Cambiar el eje del tc (secundario)
a2_g.setelem(1)  axis(r)
'Sombrear la crisis suprime (2008)
a2_g.draw(shade,bottom,gray) 2007.9 2010.6
a2_g.draw(shade,bottom,gray) 2020.1 2020.8
show  a2_g
'Agregar texto 
	'Título 
	a2_g.addtext(t)  Figura 1: Crecimiento del PBI y evolución del TC
	a2_g.addtext(0.5,0.5) Crisis Suprime
	a2_g.addtext(4.25,0.1) Covid-19

'Formato de Gráfico
	'Color de línea
	a2_g.setelem(1) lcolor(green)
	a2_g.setelem(2) lcolor(@rgb(200,100,200))
	
	'Ancho de línea
	a2_g.setelem(2)  lwidth(2)

	'Leyenda
	a2_g.setelem(1) legend(Tipo de Campo Interbancario Venta)
	a2_g.setelem(2) legend(Crecimiento internual del PBI Real)

	'Color Fondo Interno
	a2_g.options   fillcolor(rgb(185,205,229))
	
	'Color Fondo Externo
	a2_g.options backcolor(white)

	'Guardar gráfico 
	a2_g.save(t= gif)  Gráfico PBI-TC
