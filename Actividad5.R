# Punto 1. Se comparan indicadores de dos sierras mecanicas en un proceso de corte
library(readxl)

datos_sierra_1 <- read_excel("datos_tarea 5 1.xlsx", sheet = "PROB 1", col_names = TRUE)
datos_sierra_2 <- read_excel("datos_tarea 5 1.xlsx", sheet = "PROB1_SIERRA2", col_names = TRUE)
# X1 : Presion de Mordaza (psi)
# X2 : Velocidad de la sierra (m/min)
# X3 : Velocidad de posicionamiento (%)
# X4: Presion de avance (psi)

# Obtener vector de media poblacion de datos_sierra_1
mu_sierra_1 <- colMeans(datos_sierra_1)
# Obtener vector de media poblacion de datos_sierra_2
mu_sierra_2 <- colMeans(datos_sierra_2)

# Tambien se le conoce como delta de medias es decir: X1 - X2
diferencia_medias <- mu_sierra_1 - mu_sierra_2

# Obtener S de datos_sierra_1 - matriz de covarianza
S_sierra_1 <- cov(datos_sierra_1)
# Obtener S de datos_sierra_2
S_sierra_2 <- cov(datos_sierra_2)

# n , total de datos de las sierras
n1 <- nrow(datos_sierra_1)
n2 <- nrow(datos_sierra_2)

# Matriz de Covarianza ponderada -  (1/n1)S1 + (1/n2)S2
Sp <- ((1/n1)*S_sierra_1) + ((1/n2)*S_sierra_2)

# (1/n1)S1 + (1/n2)S2 ^-1 -- Inversa de Sp
Sp_inv <- solve(Sp)


# Estadistico de prueba T^2 = (u1-u2)'Sp^-1(u1-u2)
T_cuadrado <- t(diferencia_medias) %*% Sp_inv %*% diferencia_medias
# Se convierte a numero para poder utilizarlo.
T_cuadrado <- as.numeric(T_cuadrado)

# Nivel de significancia - alpha = 5%
alpha <- 0.05
# Numero de variables
p <- ncol(datos_sierra_1)
# Valor critico chi-cuadrado
valor_critico <- qchisq(1-alpha, df=p)
valor_critico

print("Valor de T_Cuadrado: ")
print(T_cuadrado)
print("Valor critico: ")
print(valor_critico)

# Se compara el estadistico de prueba con el valor critico
if(T_cuadrado > valor_critico){
  print("Se rechaza la hipotesis nula T^2 > valor critico")
}else{
  print("No se rechaza la hipotesis nula T^2 < valor critico")
}

# Intervalo de confianza considerando valor_critico
limite_inferior <- diferencia_medias - sqrt(valor_critico)*sqrt(diag(Sp))
limite_superior <- diferencia_medias + sqrt(valor_critico)*sqrt(diag(Sp))
# limite inferior < u1-u2 < limite superior
print(limite_inferior)
print(limite_superior)
