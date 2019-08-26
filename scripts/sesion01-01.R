#############################################################
# Fundamentos Matemáticos de la Estadística. Curso 2019-2020.
# sesion00-01
#############################################################


# Vectores en R
###############

# Empezamos creando un vector numérico usando c, paréntesis y comas para separar los datos.

edades = c(22, 21, 18, 19, 17, 21, 18, 20, 17, 18, 17, 22, 20, 19, 18, 20, 19)

# Puedes usar funciones de R para calcular propiedades del vector:

length(edades) # número de elementos

mean(edades) # media

sum(edades) # suma

# Vectorización de operaciones
##############################

# R aplica operaciones aritméticas o funciones matemáticas elemento a elemento.
edades + 3 

edades^2

sqrt(edades) # A diferencia con mean, sqrt actúa sobre el vector en conjunto.

# Cuando operamos con dos vectores también es elemento a elemento

c(2, 3, 5) * c(4, -1, 7)


# Vectores de caracteres
########################

# Los elementos de un vector también pueden ser palabras o frases
aves = c("Frailecillo", "Vuelvepiedras", "Avoceta", "Chorlitejo Patinegro")

nchar(aves) # La función nchar mide el número de caracteres de una palabra

# Los elementos de un vector son todos del mismo tipo (homogeneidad).
# Si tratas de mezclar tipos, R los convertirá al tipo más genérico.
c(1, "A", 7)  #Mira las comillas del resultado.

# Generando vectores
####################

# Forma rápida de generar enteros consecutivos
12:31 

# La función seq

seq(from = 20.1, to = 27, by = 0.7) # Elegimos inicio, fin e incremento

seq(0, 100, length.out = 11) # Podemos decidir cuántos valores se generan.

# La función rep

rep(c("A", "B", "C"), each = 3) # número de repeticiones

rep(c("A", "B", "C"), times = 3) # El mismo número pero observa la diferencia

rep(c("A", "B", "C"), times = c(2, 4, 3)) # número distinto para cada uno

# La función sample

sample(1:100, size = 3) # Ejecuta varias veces esta línea para ver lo que hace.

sample(1:6, size = 20) # Esto producirá un error, ¿por qué?

sample(1:6, size = 100, replace = TRUE) # 100 tiradas de un dado

sample(c("A", "T", "G", "C"), size = 42, replace = TRUE) # No solo números

# sample también sirve para "barajar" un conjunto de datos 
(datos = sample(1:100, size = 10, replace = TRUE))
# Ejecuta varias veces seguidas la siguiente línea
sample(datos)

# Comparaciones,  valores lógicos, funciones which y sort
##########################################################

# El resultado de una comparación es un valor TRUE/FALSE

3 < 5

5 < 4

# Las comparaciones también se pueden aplicar vectorialmente:

edades
edades < 20

# Para saber si dos valores son iguales en R se usa ==
edades == 19

# y para ver si son distintos se usa !=
edades != 19

# Además en R TRUE equivale a 1, mientras que FALSE vale 0. En particular
sum(edades == 19) # nos dice cuántos valores iguales a 19 hay en edades

# Para saber cuáles son (sus posiciones) usamos which
which(edades == 19)

# Los operadores lógicos son & para y (es decir AND) y | para o (es decir OR) 
# Por ejemplo para encontrar las posiciones con edades entre 19 y 21
which( (edades >= 19) & (edades <= 21))

# Otro operador útil es %in%, que se usa para ver si un valor pertenece a un
# conjunto dado de velores, habitualmente otro vector. 
# Por ejemplo para encontrar las posiciones con edades iguales a 17 o 21:
which(edades %in% c(17, 21))

# Con la función sort ordenamos un vector
sort(edades) # por defecto orden creciente
sort(edades, decreasing = TRUE)  # orden decreciente

# Selección de elementos de un vector
#####################################

# Para seleccionar elementos de un vector se usan corchetes []
edades

# Seleccionamos el elemento en la tercera posición
edades[3]

# Los elementos en las posiciones 3, 5, y 13
edades[c(3, 5, 13)]
edades[c(3, 13, 5)] # El orden importa

# Podemos seleccionar por posiciones usando vectores construidos con :
edades[3:7]

# Y usar posiciones negativas para excluir valores
edades[-(1:4)]

# También podemos seleccionar basándonos em comparaciones. 
edades < 20 # obtenemos un vector de respuestas "este sí, este no"
edades[edades < 20] # y lo usamos, para seleccionar los que cumplen la condición

# o cualquier tipo de condición lógica:
edades[edades %in% c(17, 21)]


