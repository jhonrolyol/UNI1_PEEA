'================================
' Sesión 02: Introducción a Eviews
' Importación y manejo de datos
' Fecha: 24/04/2022
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

'==============================
'Análisis Bivariado
'==============================
'Matriz de Correlaciones 
%lista2 = @wlookup("*crec","series")
group gr1  {%lista2}

freeze(a3_mcorr) gr1.cov corr prob

'Gráfico de Dispersión
freeze(a3_scatmat)  gr1.scatmat linefit

'==============================
'Análisis Multivariado
'==============================
%smple = "@first 2019.12"    'Muestra de estimación
%smplf = "2020.1 2021.12"    'Muestra de evaluación (test-fuera de tiempo)

smpl  %smple
'Regresión por Mínimos Cuadrados Ordinarios (MCO) 
equation model1.ls pbi_crec c ti_crec tc_crec empleo_crec  ipc_crec

	'Significancia individual  (t-statistic): todas las variables son significativas porque tienen un prob menor a 5% con excepción de TI_CREC.

	'Significancia global (F-Statistic): Al menos una variable del modelo es significativa dado que la prob(F) es menor que el 5%. 

	'R2: 0.33 (33%)/ R2 Ajustado (31.6%):
	'Las variables regresoras explica en un 33% la variabilidad del crecimiento del PBI
	' R2 Ajustado: Penaliza la inclusión de muchas variables


'Supuesto 1: Estabilidad de parámetros	
freeze(a4_EstPar)  model1.multibreak 
	'Conclusión: Analizas las gráficas de cusum y cusum2 no hay presencia de quiebres estructurales. Recomendación: Crear variables dummy y evaluar en las fechas que indica el multibreak

'Supuesto 2: Normalidad de errores
freeze(a5_histograma)  model1.hist
	'Conclusión: Normalidad de errores
	
'Supuesto 3: No multicolinealidad
freeze(a6_vif) model1.varinf 
	'Umbral: VIF < 10 => No hay multicolinealidad
	'Conclusión: No hay multicolinealidad

'Supuesto 4: No heterocedasticidad 
	'Ho: No hay heterocedasticidad (Homocedasticidad) 
freeze(a7_bpg)   model1.hettest(type = "bpg") @regs
freeze(a8_white) model1.white(c)	
	'Conclusión: No hay heterocedasticidad

'Supuesto 5: No autocorrelación
	'Ho: No hay autocorrelación de orden "n"
	'Durbin Watson Statistic en la regresión inicial 
		'DW cercano a 2 (1.8,2.2) : no hay autocorrelacion orden 1
		'DW muy pequeño, problemas de especificación 
		' Y = AKL 
freeze(a9_correl)  model1.correl(24) 
freeze(a9_bg)  model1.auto(3) 
		'Conclusión: Potencial problema de autocorrelación de orden 2 

	equation model2.ls  pbi_crec c tc_crec empleo_crec  ipc_crec ar(1) ar(2)

'========================================
'Forecast 
'========================================
'Forecast dentro de la muestra (i: in the sample)
model1.fit(e,g)  pbi_crec_fi1
model2.fit(e,g)  pbi_crec_fi2
graph a10_g1.line   pbi_crec  pbi_crec_fi1  pbi_crec_fi2

'Forecast fuera de la muestra (o: out of time) 
smpl  %smplf
model1.fit(e,g)  pbi_crec_fo1
model2.fit(e,g)  pbi_crec_fo2
graph a10_g2.line   pbi_crec  pbi_crec_fo1  pbi_crec_fo2

'========================================
'Model Object 
'========================================
smpl %smple
model1.makemodel(ObjMod1)
model2.makemodel(ObjMod2)

smpl %smplf
ObjMod1.solveopt(s = b) 'stochastic – collect means and bounds)
ObjMod1.stochastic(i = n, r = 10000,  b= 0.90, c = t)
ObjMod1.solve

graph a11_g1.line pbi_crec  pbi_crec_0l  pbi_crec_0h  pbi_crec_0m


smpl %smplf
ObjMod2.solveopt(s = b) 'stochastic – collect means and bounds)
ObjMod2.solveopt(d=s)
ObjMod2.stochastic(i = n, r = 10000,  b= 0.90, c = t)
ObjMod2.solve

graph a11_g2.line pbi_crec  pbi_crec_0l  pbi_crec_0h  pbi_crec_0m





















