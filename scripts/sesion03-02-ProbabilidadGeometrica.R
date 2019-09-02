#############################################################
# Fundamentos Matemáticos de la Estadística. Curso 2019-2020.
# sesion03-02
#############################################################

##########################
# Probabilidad Geométrica
##########################


# La función runif sirve para elegir puntos al azar de 
# manera uniforme dentro de un intervalo.

# Fijamos la semilla del generador pseudoaleatorio.
set.seed(2019)

# Elegimos n0 puntos 
n0 = 1000000

puntos = runif(n0, min = 0, max = 1)

head(puntos)
tail(puntos)

# Veamos qué proporción de ellos pertenecen al intervalo $[1/3, 2/3]$ 

enIntervalo = (1/3 <= puntos) & (puntos <= 2/3)

# La proporción se parece mucho al valor que esperábamos.
(proporcion = mean(enIntervalo))

