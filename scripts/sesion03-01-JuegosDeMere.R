#############################################################
# Fundamentos Matemáticos de la Estadística. Curso 2019-2020.
# sesion03-01
#############################################################

################################
# Juego 1 del caballero de Mere
################################

# Empezamos con una limpieza inicial de la memoria de R
rm(list=ls())

# Vamos a simular 4 * N partidas
N = 100000

# Generamos  N tiradas del dado
dado = sample(1:6, 4 * N, replace = TRUE)

# Los agrupamos en partidas cada cuatro tiradas.
deMere1 = matrix(dado, ncol = 4, byrow = TRUE)

# Localizamos las apariciones del 6.
esSeis = (deMere1 == 6)

# Contamos cuantas apariciones del seis hay en cada partida.
cuantosSeis = rowSums(esSeis)

# Localizamos aquellas partidas con al menos un 6.
partidasGanadoras = (cuantosSeis > 0)

# Y medimos la proporcion de partidas ganadoras.
(proporcion = table(partidasGanadoras)/length(partidasGanadoras))

# El resultado no es 2/3
2/3

################################
# Juego 2 del caballero de Mere
################################

# Limpieza inicial
rm(list=ls())

# Como antes, vamos a simular 24 * N partidas
N = 24 * 100000

# Generamos las N tiradas de dos dados que representamos por números 
# del 1 al 36. Un 36 indica seis doble.
dado = sample(1:36, N, replace = TRUE)

# Los agrupamos en partidas cada 24 tiradas.
deMere2 = matrix(dado, ncol = 24, byrow = TRUE)

# Localizamos las apariciones del 36 (representa 6 doble).
es36 = (deMere2 == 36)

# Contamos cuantas apariciones del 36 hay en cada partida.
cuantos36 = rowSums(es36)

# Localizamos aquellas partidas con al menos un 36.
partidasGanadoras = (cuantos36 > 0)

# Y medimos la proporcion de partidas ganadoras.
(proporcion = table(partidasGanadoras)/length(partidasGanadoras))

# Tampoco en este caso el resultado es 2/3.

