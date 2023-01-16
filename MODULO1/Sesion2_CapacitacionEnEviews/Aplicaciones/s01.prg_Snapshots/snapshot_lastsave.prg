'============================
' Sesión 02: Introducción a Eviews
' Fecha: 24/09/2022
'============================
'Ejecutar: F10 o boton RUN

'============================
'Paso 0: Creación de Workfile
'============================
'u: undated   
'wf   u   1000

'Series de tiempo 
	'q: quarterly (trimestral)
	'm: monthly (mes)
	'a: annual (año)
wf   q   1990.1  2022.1

'============================
'Creación de Objetos
'============================
'No es case sensitive: No discrimina entre minúsculas y mayúsculas
'Nota de programación: Orientada a objetos

'[Objeto](`Esp.`)  (Nombre)  `Especificación adicional´

'Vector 
vector(4)  v_squares
	'Llenado de información (1)
	v_squares(1)  = 1
	v_squares(2)  = 4
	v_squares(3)  = 9
	v_squares(4)  = 16 
	
	'Llenado de información (2)
	v_squares.fill  1,4,9,16 

	'Llenado de información (3)
	for  !row  = 1 to 4 
		v_squares(!row) = !row^2
	next
	
	scalar edad  = 29
	'   (  !  ): Permite hacer uso de un scalar sin guardar en WF 
	'   ( % ): Permite hacer uso de un texto sin guardar en WF
	
'Matriz
matrix(2,2)  m_cubos 
	'Llenado de información (1)
	m_cubos(1,1) = 1
	m_cubos(1,2) = 8
	m_cubos(2,1) = 27
	m_cubos(2,2) = 64

	'Operaciones con matrices
	'Transpuesta 
	matrix  mt  = @transpose(m_cubos)

	'Suma 
	matrix  mt_sum = m_cubos + mt 

	'Inversa 
	matrix  matinv = @inverse(m_cubos)

	'Otros
	scalar det = @det(m_cubos)             'Determinate
	scalar rango = @rank(m_cubos)       'Rango
	sym   mtsym = m_cubos*mt               'Matriz Simétrica
	vector  vp  = @eigenvalues(mtsym)  'Valores propios 

' Series 
series t = @trend
series est = @seas(4)

series pbi = @trend + @seas(4)*10 + @rnorm*5 + 100
graph g1.line  pbi 

series cons = @trend + @seas(4)*10 + @rnorm*5 + 80
series tc = 3.5 + @rnorm*0.1

'Grupo de series
group g_variables pbi  cons tc 
	' Gráficos
	graph g2.line  g_variables
	g2.setelem(3)  axis(r)
	' Matriz de correlaciones
	freeze(MatCorr) g_variables.cor
	
'Ecuación
	'ls: least squares (Método de Estimación:Mínimos Cuadrados)
	equation eq1.ls  pbi  c cons tc 

'Tablas 
	table resultados 
	resultados(1,1) = "Resultados de las Operaciones con Matrices"
	
	resultados(3,1) = "Determinante"
	resultados(4,1) = "Rango"
	resultados(5,1) = "Traza"
	resultados(6,1) = "Orden de la matriz"

	resultados(3,2) = @det(m_cubos)
	resultados(4,2) = @rank(m_cubos)
	resultados(5,2) = @trace(m_cubos)
	resultados(6,2) = @str(@rows(m_cubos)) + "x" + @str(@columns(m_cubos))
	
	' 2 + 2 = 4  => suma de números
	' "2" + "2" = "22" => concatenación de textos

'Formato de tablas
	'Cambiar el color del texto
	resultados.settextcolor(A1) @rgb(14,113,235)
	
	'Cambiar el color del fondo 
	resultados.setfillcolor(A1:D8) @rgb(242,242,247)

	'Cambiar el ancho de las columnas
	resultados.setwidth(A) 20 

	'Cambiar formato de número
	resultados.setformat(B) g 

	'Guardar como LaTeX
	%ruta = @runpath   'Ruta donde se encuentra guardado el program
	cd %ruta

	freeze(Modelo) eq1
	modelo.save(t= tex, texspec,clipboard) "reg.tex"





