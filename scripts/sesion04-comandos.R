## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 4. Poblaciones, muestras y probabilidad."



set.seed(2019)
N = 158000
poblacion = as.integer(2 * rchisq(N, df = 13), 0)



options(width= 90)
n = 20
(muestra = sample(poblacion, n, replace = TRUE))
options(width= 70)

options(width= 90)
(muestra2 = sample(poblacion, n, replace = TRUE))
mean(muestra2)

options(width= 90)
(muestra3 = sort(poblacion)[1:20])

options(width= 90)
mean(muestra3)

k = 10000
# replicate repite k veces los comandos entre llaves y guarda el resultado
# del último comando en el vector mediasMuestrales
mediasMuestrales = replicate(k, { 
  muestra = sample(poblacion, n, replace = TRUE)
  mean(muestra)
})
head(mediasMuestrales, 10)



poblacion = sample(0:20, 20000, replace = TRUE)

k = 10000
mediasMuestrales = replicate(k, { 
  muestra = sample(poblacion, n, replace = TRUE)
  mean(muestra)
})



Otras poblaciones:
  
#####################################################################
# Uniforme continua
#####################################################################

tamPoblacion = 100000
poblacion = runif(tamPoblacion, min = 0, max = 10)
head(poblacion, 100)


hist(poblacion)
plot(density(poblacion))

mean(poblacion)

Tmuestra = 20

# ¿Cuántas muestras distintas hay?
choose(tamPoblacion, Tmuestra)

# La población es moderadamente grande, pero el espacio de muestras es enorme.

# Vamos a tomar muchas muestras y en cada una calculamos una media muestral.
numMuestras = 100000

# Repetiremos esto varias veces para hacernos una idea.
(muestra = sample(poblacion, size = Tmuestra, replace = TRUE))
mean(muestra)

mediasMuestrales = replicate(numMuestras, {
  muestra = sample(poblacion, size = Tmuestra, replace = TRUE)
  mean(muestra)
})

# ¿Cómo se distibuyen esas medias muestrales?
head(mediasMuestrales)
hist(mediasMuestrales)
hist(mediasMuestrales, breaks = 40, main="")
plot(density(mediasMuestrales, adjust = 1.5), main="")

# ¿Cuál es la media de las me?
(mu = mean(poblacion))
mean(mediasMuestrales)

# ¿Cuál es su desviación típica?
(desvTipPob = sqrt(sum((poblacion - mu)^2) / tamPoblacion))

sd(mediasMuestrales)
desvTipPob / sqrt(Tmuestra)

n = 366 # Número de personas en la sala

# Vamos a repetir el experimento N veces (N salas de n personas)
N = 10000
pruebas = replicate(N, {
  fechas = sort(sample(1:366, n, replace=TRUE)) 
  max(table(fechas)) # si el máximo es mayor que 1 es que 2 fechas coinciden
})
mean(pruebas > 1) # ¿qué proporción de salas tienen coincidencias?



library(kernlab)
data(spam)
spam[1:4, c(1:10, 58)]

library(tidyverse)
spam = spam %>%
  select(order, type) %>% 
  mutate(hasOrder = factor(order > 0, # Creamos el factor hasOrder
                            levels = c(TRUE, FALSE), 
                            labels = c("order", "no order")),
         type = relevel(type, ref = "spam"), # Reordenamos los niveles
         -order) # y elminamos el factor order original

table(spam$hasOrder, spam$type)

options(width = 70)
(ardeida = factor(c("martinete", "garzaReal", "avetorillo", "garzaReal",
                    "cangrejera", "martinete", "martinete"), ))

options(width = 70)
(ardeida = factor(c("martinete", "garzaReal", "avetorillo", "garzaReal",
                  "cangrejera", "martinete", "martinete"), 
levels = c("garzaReal", "martinete",  "cangrejera", "avetorillo")))  

options(width = 70)
gl(n = 3, k = 4, labels = c("piedra", "papel", "tijera"))

gl(n = 3, k=1, length = 30, labels = c("piedra", "papel", "tijera"))

(M = matrix(1:36, nrow=4) )

(M = matrix(1:36, nrow=4, byrow = TRUE) )

dim(M) = c(3, 12)
M

v = rep(c(1, 2, 3), each=6)
cat(paste0("v = c(", paste0(v,collapse = ", "), ")"))

Mv = matrix(v, nrow=3, byrow = TRUE)
(v = c(Mv))
