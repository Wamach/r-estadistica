# Actividad 5 Punto 2.

# En este actividad no se tienen los datos de las poblaciones, 
# pero se tienen las matrices de covarianzas y n de cada poblacion
# Tambien se tiene su vector de media poblacional

# Se comparan dos maquinarias pesadas de perforación profunda. 
# Se miden las siguientes variables:
# X1 = peso de arrastre, 
# X2 = peso de rotación, 
# X3 = ángulo, 
# X4 = velocidad de la barrena,
# X5 = peso del lodo, 
# X6 = peso de levante y 
#X7 = Torque de fondo.

maquina1_S <- matrix(c(599.92092,	645.84289,	45.50137,	39.21181,	227.3114,	188.15965,	43.29901,
                         645.84289,	696.7868,	48.46133,	41.69736,	244.34511,	202.89604,	46.87387,
                         45.50137,	48.46133,	4.8676,	4.04492,	17.47205,	14.16381,	2.87871,
                         39.21181,	41.69736,	4.04492,	4.11783,	14.73732,	11.3687,	2.59717,
                         227.3114,	244.34511,	17.47205,	14.73732,	97.0569,	70.60495,	16.07998,
                         188.15965,	202.89604,	14.16381,	11.3687, 70.60495,	63.48021,	13.68793,
                         43.29901,	46.87387,	2.87871,	2.59717,	16.07998,	13.68793,	3.31663), nrow = 7 , ncol = 7, byrow = TRUE)
maquina2_S <- matrix (c(575.27043,	618.91886,	45.70737,	38.20195,	225.8663,	175.25235,	41.49424,
                        618.91886,	667.58465,	48.858,	40.40396,	243.20193,	189.21062,	44.853,
                        45.70737,	48.858,	4.71999,	3.95054,	18.03344,	13.29122,	3.01997,
                        38.20195,	40.40396,	3.95054,	4.17723,	14.44286,	10.82841,	2.56922,
                        225.8663,	243.20193,	18.03344,	14.44286,	95.49805,	67.83094,	16.08471,
                        175.25235,	189.21062,	13.29122,	10.82841,	67.83094,	58.56777,	12.80255,
                        41.49424,	44.853,	3.01997,	2.56922,	16.08471,	12.80255,	3.14181),  nrow = 7 , ncol = 7, byrow = TRUE)

n_maquina1 <- 60
n_maquina2 <- 80

# Funcion para verificar si se trata de muestras grandes o no
muestras_grandes <- function(n1, n2){
  if(n1 > 50 & n2 > 50){
    print("Se trata de muestras grandes")
  }else{
    print("No se trata de muestras pequeñas")
  }
}
muestras_grandes(n_maquina1, n_maquina2)

mu_maquina1 <- c(124.030, 133.900, 8.961, 7.988, 51.300, 39.730, 8.917)
mu_maquina2 <- c(125.730, 137.920, 8.206, 8.367, 50.930, 40.726, 9.179)

# Delta de medias
diferencia_medias <- mu_maquina1 - mu_maquina2

# Matriz de Covarianza ponderada -  (1/n1)S1 + (1/n2)S2
Sp <- ((1/n_maquina1)*maquina1_S) + ((1/n_maquina2)*maquina2_S)
# Matriz de Covarianza ponderada inversa
Sp_inv <- solve(Sp)


# Estadistico de prueba T^2 = (u1-u2)'Sp^-1(u1-u2)
T_cuadrado <- t(diferencia_medias) %*% Sp_inv %*% diferencia_medias
# Se convierte a numero para poder utilizarlo.
T_cuadrado <- as.numeric(T_cuadrado)

print("Valor de T_Cuadrado: ")
print(T_cuadrado)

# Obtener valor critico
p <- 7 # Cantidad de columnas
alpha <- 0.05 # Nivel de significancia
valor_critico <- qchisq(1-alpha, df=p)
print(T_cuadrado)
print(valor_critico)

# Se compara el estadistico de prueba con el valor critico
if(T_cuadrado > valor_critico){
  print("Se rechaza la hipotesis nula T^2 > valor critico")
}else{
  print("No se rechaza la hipotesis nula T^2 < valor critico")
}
