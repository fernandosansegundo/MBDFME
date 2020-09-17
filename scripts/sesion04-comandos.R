## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 4. Poblaciones, muestras y probabilidad. Variables aleatorias."



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

muestra = sample(0:3, size = 10, replace = TRUE, prob = c(64, 48, 12, 1))

library(viridisLite)
muestra = sample(0:3, size = 1000, replace = TRUE, prob = c(64, 48, 12, 1))
barplot(table(muestra), col=viridis(4))



library(tidyverse)
fhs = read_csv("../datos/framingham.csv")
tablaHyp = prop.table(table(fhs$prevalentHyp))
p = unname(tablaHyp[2])

set.seed(2019)
n = 7
N = 50000
X = replicate(N, {
  pacientes = sample(fhs$prevalentHyp, n, replace = TRUE)
  (exitos = (pacientes == 1))
  sum(exitos)
})
prop.table(table(X))
dbinom(x = 0:n, size = n, prob = p)

dbinom(x = 3, size = 7, prob = p)

signif(dbinom(x = 0:7, size = 7, prob = p), digits = 3)

signif(pbinom(q = 0:7, size = 7, prob = p), digits = 3)

rbinom(n = 25, size = 7, prob = p)

probabilidades = dbinom(x = 0:7, size = 7, prob = p)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:7)
arrows(seq(0.5, 7.5, by = 1), 0, seq(0.5, 7.5, by = 1), prop.table(table(X)), col="red", lwd = 2)

probabilidades = dbinom(x = 0:12, size = 10, prob = 2/3)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:12)

probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)

probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)
par(new = T)
probabilidades[0:25] = 0
probabilidades[36:100] = 0
barplot(probabilidades/4, space = 0, col="blue", names.arg = 0:100)

probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)
par(new = T)
probabilidades[0:25] = 0
probabilidades[36:100] = 0
barplot(probabilidades/4, space = 0, col="blue", names.arg = 0:100)







pnorm(10.5, mean=10, sd=2)

  1 - pnorm(11, mean=10, sd=2)
  pnorm(11, mean = 10, sd = 2, lower.tail = FALSE)



pnorm(12, mean=10, sd=2) - pnorm(7, mean=10, sd=2)


qnorm(p = 1/3, mean = 10, sd=2)




set.seed(2019)
x1 = rnorm(1000)
y1 = rnorm(1000)
ggplot(data.frame(x1, y1)) +
  geom_point(mapping = aes(x1, y1), col="red")
x2 = runif(1000, min = -1, max = 1)
y2 = runif(1000, min = -1, max = 1)
ggplot(data.frame(x2, y2)) +
  geom_point(mapping = aes(x2, y2), col="blue")



set.seed(2019)
pob1 = rnorm(30000, mean = -3, sd = 1)
pob2 = rnorm(30000, mean = 2, sd = 0.5)
pobSuma = 3 * pob1 + 4 * pob2
plot(density(pobSuma, adjust = 1.6), main="", lwd=5, col="red", xlab="")

#########################################################
## Complementos de R
## Operaciones con factores, verbos de dplyr.
#########################################################


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

#############################################
### Verbos de dplyr
#############################################


# dplyr: select.

library(gapminder)
names(gapminder)

gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  head(3)

gapminder %>% 
  select(continent:pop, -year) %>% 
  names()

gapminder %>%  
  select(starts_with("c")) %>% 
    names()

  # dplyr: filter
  
  gapminder %>% 
  filter(country == 'Spain') %>%
  head(4)

# dplyr: filter

gapminder %>% 
filter(year == "1997") %>% 
top_n(3, gdpPercap)

# dplyr: mutate
gapminder %>% 
  mutate(gdp = pop * gdpPercap / 10^6) %>% 
  filter(year == 1982) %>% 
  sample_n(4)

gapminder %>% 
  mutate(gdp = pop * gdpPercap / 10^6) %>% 
  mutate_at("gdp", log10) %>% 
  head(4)
