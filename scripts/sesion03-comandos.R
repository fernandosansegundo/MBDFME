## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 3. Distribuciones. Valores centrales y dispersión."

# Curvas de densidad de auto2, p. 4
require(gridExtra)
require(tidyverse)
require(haven)
auto2 = read_dta("./datos/auto2.dta")
p1 = ggplot(auto2) + 
  geom_density(mapping = aes(price), color="red", fill="lightblue", size=1.5)

p2 = ggplot(auto2) + 
  geom_density(mapping = aes(trunk), color="red", fill="lightblue", size=1.5)

p3 = ggplot(auto2) + 
  geom_density(mapping = aes(weight), color="red", fill="lightblue", size=1.5)

p4 = ggplot(auto2) + 
  geom_density(mapping = aes(gear_ratio), color="red", fill="lightblue", size=1.5)

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Distribución unimodal p. 5
curve(dchisq(x, df=6), from = 0, to = 16, ylab="density", col="red", lwd=4, main="")

# Distribución bimodal p. 5
set.seed(2014)
N = 10000
mu1 = 12
sigma1 = 0.7
mu2 = 14
sigma2 = 0.5
n1 = rnorm(N, mean=mu1, sd=sigma1)
n2 = rnorm(N, mean=mu2, sd=sigma2)
# mezcla de normales
mezcla = c (n1,  n2)
plot(density(mezcla), col="red", lwd=4, main="")


# Ilustrando el concepto de skewness p.5
par(mfrow = c(3, 1))
curve(dchisq(x, df=6), from = 0, to = 16, 
      ylab="density", col="blue", lwd=4, main="Asimétrica a derecha", cex.main=2)  
curve(dnorm, from = -3, to = 3, 
      ylab="density", col="black", lwd=4, main="Simétrica", cex.main=2)  
curve(dchisq(15-x, df=6), from = 0, to = 16, 
      ylab="density", col="red", lwd=4, main="Asimétrica a izquierda", cex.main=2)  
par(mfrow = c(1, 1))



# Media aritmética, p. 9
set.seed(2019)
(muestra = sample(0:100, size = 20, replace = TRUE))
(media = mean(muestra))

# Valores atípicos, p. 10
set.seed(2019)
muestra = sample(0:100, size = 99, replace = TRUE) 
(media = mean(muestra))

muestra2 = c(muestra, 1000)
(media2 = mean(muestra2))

# Mediana, p. 11
set.seed(2019)
(valores = sample(1:100, 17, replace = TRUE))

(ordenados = sort(valores))
(mediana = ordenados[9])

median(valores)

median(muestra)
median(muestra2)

# Valores de posición, p. 13
quantile(mpg$cty, probs = 0.43)

# Summary, p. 13
summary(mpg$cty)

# El gráfico de dispersión de la página 14
library(tidyverse)
set.seed(2019)
n = 400
library(MASS)
muestra1 = mvrnorm(n, mu = 0, Sigma = 1, empirical = TRUE)
min1 = min(muestra1)
max1 = max(muestra1)
muestra1 = c(muestra1[-(1:10)], runif(10, 2 * min1, 2 * max1))
muestra2 = runif(n, 2 * min1, 2 * max1)
muestras = 
  data.frame(x = c(muestra1, muestra2), tipo = gl(2, n))

ggplot(data = muestras) +
  geom_point(mapping = aes(tipo, y = x, color = tipo), 
             position =  position_jitter(w = 0.05, h = 0)) + 
  coord_flip()

# y la versión en curvas de densidad de la página 15
plot(density(muestra1, adjust = 2), col="blue", 
     xlim = c(-12, 12), lwd = 2, main = "", xlab= "")
lines(density(muestra2, adjust = 2), col="red", lwd = 2, sub="")

# Recorrido intercuartílico, p. 16
IQR(mpg$cty)

summary(mpg$cty)

unname(quantile(mpg$cty, probs = c(1/4, 3/4)) + c(-1, 1) * 1.5 * IQR(mpg$cty))



# Boxplot con R básico p. 19
bxp_cty = boxplot(mpg$cty, col="orange")

bxp_cty$out

# Boxplot y violinplot con R básico p. 20
ggplot(mpg) + 
  geom_boxplot(mapping = aes(y = cty), fill="orange") +
  scale_x_discrete(breaks = c())


ggplot(mpg) + 
  geom_violin(mapping = aes(x=0, y = cty)) +
  scale_x_discrete(breaks = c()) +
  geom_boxplot(mapping = aes(y = cty), fill="green") +
  geom_jitter(aes(x=0, y = cty), 
              position = position_jitter(w=0.05, h= 0), col="blue")

# Desviación absoluta mediana p. 21
library(readxl)
accidentes = read_excel("./datos/train_acc_2010.xls")
mad(accidentes$Speed, constant = 1)

# Varianza y desviación típica p. 24
var(mpg$displ)

n = length(mpg$displ)
media = mean(mpg$displ)
sum((mpg$displ - media)^2) / (n - 1 )

sd(mpg$displ)

sqrt(var(mpg$displ))

# Factores p. 26
table(accidentes$TrkType)
prop.table(table(accidentes$TrkType))

# Gráficos para factores, p. 27
library(viridisLite)
barplot(prop.table(table(accidentes$TrkType)), col=viridis(5))
ggplot(accidentes) + 
  geom_bar(mapping = aes(x = TrkType), fill= viridis(5))

# Factores dicotómicos
fhs = read_csv("./datos/framingham.csv")
mean(fhs$male)

#########################################################
## Sección 4. Complementos de R
## Listas, bucles, funciones, datos ausentes, Rmarkdown
#########################################################

(planeta = list(nombre = "Marte", exterior = TRUE, 
                 radio = 3389.5, satelites = list("Fobos", "Deimos")))

planeta[[1]]
planeta$exterior
planeta$satelites[[1]]

planeta[1]
planeta["exterior"]

(l1 = list("A", "B"))
(l2 = list(c("A", "B")))

## (l3 = list(l2, "C"))

l4 = append(l2, "D")
(l4 = c(l2, "D"))

l4[3] = NULL
l4

unlist(l1)

## lista = list(letters[1:3], matrix(1:12, nrow = 3), TRUE)
## unlist(lista)

ifelse(((1:5) < 3), yes = "A",  no = "B")

## valores = numeric(10) # Creamos un vector del tamaño previsto
## for (k in 1:10){
##   sorteo = sample(1:20, 1)
##   print(paste0("k = ", k, ", sorteo = ", sorteo))
##   if (k %in% 5:6){
##     next # saltamos dos valores
##   } else if (sorteo  == 1){
##     print("Resultado del sorteo es 1, fin del bucle")
##     break # paramos si un valor aleatorio es 1
##   }
##   valores[k] = k # se ejecuta cuando no se cumplan las condiciones
## }
## valores

## # Ejemplo de bucle break
## k = 0
## while (k < 4){
##   k = k + 1
##   print(k)
##   if(sample(1:6, 1) == 6){
##     print("Final prematuro")
##     break()
##   }
## }

## # Ejemplo de bucle repeat similar al bucle while previo
## k = 1
## repeat {
##   k = k + 1
##   print(k)
##   if(sample(1:6, 1) == 6){
##     print("Final prematuro")
##     break()
##   }
## }

genPasswd = function(size, upp = TRUE, low = TRUE, nmb = TRUE){

  # El vector pool guarda el juego de caracteres del password
  pool = character() 
  
  # Generamos pool según las opciones
  if(upp) pool = c(pool, LETTERS) 
  if(low) pool = c(pool, letters)
  if(nmb) pool = c(pool, 0:9)
  
  # Sorteamos los símbolos que aparecen en el password
  passwd = sample(pool, size, replace = TRUE) 
  # Y lo reducimos a un string con paste
  paste(passwd, sep = "", collapse = "") 
}

genPasswd(size = 15)

formals(genPasswd)

body(genPasswd)

body(genPasswd) = "No me apetece trabajar..."
genPasswd(12)

x = c(2, 3, -5, NA, 4, 6, NA)
is.na(x)

any(is.na(fhs$glucose))

head(complete.cases(fhs), 17)

mean(fhs$glucose)

mean(fhs$glucose, na.rm = TRUE)
