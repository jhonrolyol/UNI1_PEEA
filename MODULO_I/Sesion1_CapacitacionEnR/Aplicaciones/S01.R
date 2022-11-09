# ==================================
# Sesión 01: Introducción a R 
# Fecha: 23/04/2022
# ==================================

# Ejecutar código: control + enter

# =================================
# Comandos Básicos 
# =================================

# Obtener directorio actual
getwd()

# Cambiar el directorio actual
setwd("D:/rperezpalmap/Escritorio/Cursos Online/01 UNI_PEEA/PEEA XIII/Sesión 01 - R/Aplicaciones")

# Crear unas variables 
nombre <- "Richard"
edad <- 29L  # L: Entero 
talla <- 1.75
flag_casado <- FALSE
fecha <- Sys.Date()
  
# Tipo de dato
class(nombre)
class(edad)
class(talla)
class(flag_casado)
class(fecha)

# Asignación de variables (alt + - (a la izquierda del shift derecho))
fecha1 <- "20220423"  # La más usada
"20220423" -> fecha2
fecha3 = "20220423"
  # R es case sensitive (Diferencia mayúsculas y minúsuculas)
  Fecha1 <- "20220422"

# Borrar objetos
rm(flag_casado)

# ========================================
# Creación de objetos
# ========================================

# Vector
v_nombres <- c("Akram","Ana","Antony")
v_edad <- c(30,30,30)
v_casado <- c(TRUE,FALSE,TRUE)

class(v_nombres)
typeof(v_nombres)

# Matriz
m_cuadrados <- matrix(data = c(1,4,9,16,25,36),nrow = 2,ncol = 3)

# Factor
v_profesiones <- sample(x = c("Economista","Administrador","Contador"),
                        size = 100,
                        replace = TRUE)
v_f_profesiones <- factor(v_profesiones)
v_f_profesiones

plot(v_profesiones)
plot(v_f_profesiones)

# data.frame
df_informacion <- data.frame(nombre = v_nombres,
                             edad  = v_edad,
                             estado_civil = v_casado)

  # Entrar a una columna del data.frame ($):
  df_informacion$nombre
  plot(df_informacion$edad)
  
# Lista (Conjunto de objetos)
l_objetos <- list(DATA = df_informacion,
                  VECTOR = v_nombres,
                  MATRIZ = m_cuadrados)
print(l_objetos)

  # Entrar a los elementos de una  lista
  l_objetos$DATA
  l_objetos[[3]]
  
# Función para aplicar a cada columna
  sapply(df_informacion,FUN = class)
  sapply(df_informacion,FUN = mean)

# Operaciones con matrices 
A <- matrix(sample(seq(1:20),size = 12),nrow = 3, ncol = 4)
B <- matrix(sample(seq(1:20),size = 12),nrow = 4, ncol = 3)

  # Suma
  C <- A + t(B)
  C
  
  # Multiplicación (3x4)(4x3) = (3x3)
  D <- A%*%B
  D
  
  # Matriz Inversa
  D^-1 # Eleva a la -1 a todos los elementos. No es matriz inversa
  solve(D)
  
  # Determinante
  det(D)

  # Descomposición de Cholesky (Simulaciones de montecarlo)
  chol(A%*%t(A))

# ====================================
# Análisis Estadístico
# ====================================

# Gráfico combinado 
par(mfrow = c(1,2))
  
v_dnormal <- dnorm(seq(-5,5,0.25))
plot(v_dnormal, type = "l", main = "Función de densidad",col = "blue")

v_pnormal <- pnorm(seq(-5,5,0.25))
plot(v_pnormal, type = "l", main = "Función Acumulado",col = "blue")

par(mfrow = c(1,1))
dev.off() # Borrar Gráfico

# Función Inversa
prob <- qnorm(0.05/2)
prob

quantile(v_dnormal)

# Generación de números aleatorios
  # Distribución normal (media = 10, desv.est= 2)
  v_aleatorios <- rnorm(n = 1000,
                        mean = 10,
                        sd = 2)
  v_aleatorios
  plot(v_aleatorios,type = "l")
  hist(v_aleatorios)

  # Distribución de Bernoulli
  v_aleatorios_ber <- rbinom(n = 1000,
                             size = 1,
                             prob = 0.5)
  plot(v_aleatorios_ber)
  hist(v_aleatorios_ber)  
  
# Bucles y condicionales
  
seq(1,10)
1:10

  # Estructura repetitivas: Bucles - for
  for (i in 1:100){
    print(2^i)
  }
  
  for (i in v_nombres){
    print(paste0("Bienvenido al curso: ", i))
  }
  
  suma <- 0
  for (i in 1:10){
    print(suma)
    suma <- suma + log(2^i)
  }

  # Estructuras condicionales
  # Determinar si el número 10 es par 
  10/3
  10%%3 # Resto de la división
  3>2
  4>10
  3==6  
  4==4  
  
  if (23%%2 == 0 ){
    print("El número es par")
  } else {
    print("el número es impar")
  } 
  
  # Combinación de ambas estructuras
  for (i in 1:1000){
    if (i%%2 == 0 ){
      print(paste0("El número ",i," es par"))
    } else {
      print(paste0("El número ",i," es impar"))
    }
  }

  # Generar una matriz de 20x20, donde el elemento de la diagonal principal
  # sea la suma del orden al cubo
  # caso contrario la múltiplicación
  m_ej1 <- matrix(NA,nrow = 20, ncol = 20)
  
  for (filas in 1:20){
    for (columnas in 1:20){
      if (filas == columnas){
        m_ej1[filas,columnas] <- (filas+columnas)^3
      } else {
        m_ej1[filas,columnas] <- filas*columnas
      }
    }
  }
  m_ej1

  for (filas in 1:20){
    for (columnas in 1:20){
      if (filas == columnas){
        m_ej1[filas,columnas] <- (filas+columnas)^3
      } else {
        m_ej1[filas,columnas] <- 0 # Fuera de la diagonal 0
      }
    }
  }
  m_ej1
  
# ====================================
# Creación de funciones 
# ====================================  
f_EsPar <- function(numero){
  if (numero%%2 == 0 ){
    print("El número es par")
  } else {
    print("El número es impar")
  } 
}
  
# Determinar el número fibonacci de orden 
f_EsPar(40)  
f_EsPar(41) 

f_NumFibonacci <- function(orden){
  phi <- (1+sqrt(5))/2
  fib <- (phi^orden-(1-phi)^orden)/sqrt(5)
  return(fib)
}  

f_NumFibonacci(10)

# Determinar si un número es fibonacci o no
f_EsFibonacci <- function(num){
  a1 <- 5*num^2+4
  a2 <- 5*num^2-4
  
  if(sqrt(a1)%%1 == 0 | sqrt(a2)%%1 == 0){
    print("Es fibonacci")
  } else {
    print("No es fibonacci")
  }
}

f_EsFibonacci(60)


# Solución alternativa: Felix 
f_fibbo <- function(num){
  res_p <- 5*(num^2)+4
  res_n <- 5*(num^2)-4
  if(round(sqrt(res_p),0) == sqrt(res_p) | round(sqrt(res_n),0) == sqrt(res_n) ){
    print("Si es fibonacci")
  }else{
    print("no es fibonacci")
  }
}

f_fibbo(55)
f_fibbo(60)

# =========================================
# Tratamiento de strings (cadenas de texto)
# =========================================

# Concatenar
M <- "Programa de Especialización"
N <- "Econometría Aplicada"
P <- "XIII"

paste0(M,N,P) #Concatena sin patrones (espacios, comas)
paste(M,N,P, sep = ",")
paste(M,N,P, sep = " ")

v_variables <- c("CONSUMO","EXPORTACIONES","GASTOS")
paste(v_variables,collapse = " + ") 

# Mayúsuculas y minúsculas
v_nombres

"AKRAM" %in% v_nombres
"AKRAM" %in% toupper(v_nombres)
"akram" %in% tolower(v_nombres)

# Extraer valores de un texto
v_fechas <- c("20210101","20220304") #YYYYMMDD
v_YM_fechas <- substr(v_fechas,1,6)
v_D_fechas <- substr(v_fechas,7,8)



