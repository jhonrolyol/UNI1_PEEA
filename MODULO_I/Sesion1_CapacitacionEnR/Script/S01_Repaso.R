# ==================================
# Repaso sesión 01: Introducción a R
# Nombre: Jhon Roly Ordoñez Leon
# Fecha: 24/04/2022
# ==================================

# ============================================
# Nota: Para poder ejecutar una línea de
# código en R seleccionar la línea(as) y
# usar las teclas Control + Enter
# ============================================

#===============================================
#  Comandos Básicos
#==============================================

# Para obtener la ruta(path) actual usar:

  getwd()

# Si queremos cambiar la ruta(path) actual usamos:
  
  setwd("C:\Users\GAMER\Desktop\UsoDiario") 
  
  # Nota: Para evitar el error cambiar 
  # el Backslach por el Slach 
  
  setwd("C:/Users/GAMER/Desktop/UsoDiario")

# Creamos algunas variables
  
  Nombre <- "Jhon Roly" # Caracter
  print(Nombre) # Imprime texto por consola
  
  Edad <- 27L # L nos indica que la variable Edad es estero
  print(Edad) # Imprime texto por consola
  
  Talla <- 1.76 # Decimal
  print(Talla) # Imprime texto por consola
  
  Flag_Casado <- FALSE # Lógico o Booleano
  print(Flag_Casado) # Imprime texto por consola

  Fecha <- Sys.Date() # Fecha 
  print(Fecha) # Imprime texto por consola

# Si queremos ver el tipo de dato usamos:
  
  class(Nombre) # Character
  
  class(Edad) # Integer
  
  class(Talla) # Numeric
  
  class(Flag_Casado) # Logical
  
  class(Fecha) # Date
  
# Asignación de variables (alt + - (a la izquierda del shift derecho)) 

  Fecha1 <- "20220424" # Esta es la asignación más usada
  
  "20220424" -> Fecha2 # Esta es la asignación menos usada
  
  Fecha3 = "20220424" # No se recomienda usar
  
  # Nota:
  # R es case sensitive (Diferencia mayúsculas de minúsuculas)
  
  fecha1 <- "20220424" # Con letra inicial en minuscula
  
# Si queremos borrar cualquiere elemento de la ventana environment usamos:
  
  rm(Flag_Casado) # Borra la variable Flag_Casado
  
  
  
# ===========================================
# Ahora seguimos con la creación de objetos
# ==========================================
  
  
# Iniciamos creando vectores
  
  V_Nombres <- c("Jhon Roly","Betzaida","Yordan") # Vector de nombres
  print(V_Nombres)
  class(V_Nombres)
  
  V_Edades <- c(27,18,15) # Vector de edades
  print(V_Edades)
  class(V_Edades)
  
  V_EstadoCivil <- c(FALSE,FALSE,FALSE) # Vector de estado civil
  print(V_EstadoCivil)
  class(V_EstadoCivil)
  
# Crear una matriz
  
  M_Cuadrada <- matrix(data = c(1,2,3,4),nrow = 2 ,ncol = 2,)
  print(M_Cuadrada)
  class(M_Cuadrada)
  # Nota: Para formar una matriz cuadra se necesita tener en 
  # la data un número determinado de elementos para formar una 
  # matriz cuadrada
  
  M_Rectangular <- matrix(data = c(1,2,3,4,5,6),nrow = 2,ncol = 3)
  print(M_Rectangular)
  class(M_Rectangular)
  
# Creando factores
  
  V_Profesiones <- sample(x=c("Economista","Administrador","Contador"),
                            size = 100,
                            replace = TRUE) # Genera una muestra de tamaño 100

  V_F_Profesional <- factor(V_Profesiones)
  print(V_F_Profesional)
  
  # Veamos en diagramas con la función plot
  
  plot(V_Profesiones)
  plot(V_F_Profesional)
  
  
# Ahora veamos los data.frame()
  
  DF_Informacion <- data.frame(Nombre=V_Nombres,
                               Edad = V_Edades,
                               EstadoCivil = V_EstadoCivil)
  # Si queremos entrar a una columna del data.frame (usar $):
  
  DF_Informacion$Nombre # Nombre
  print(DF_Informacion$Nombre)
  
  DF_Informacion$Edad # Edad
  print(DF_Informacion$Edad)
  
  DF_Informacion$EstadoCivil # Estado civil
  print(DF_Informacion$EstadoCivil)
  
# Creamos una lista de un conjunto de objetos 
  
  
  L_Objetos <- list(Data = DF_Informacion,
                    Vector = V_Nombres,
                    Matriz = M_Cuadrada)
  print(L_Objetos)

  
# Para entrar a los elementos de una lista
  
  L_Objetos$Data
  L_Objetos[[3]]
  
# Función para aplicar a cada columna  
  
  sapply(DF_Information, function)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




