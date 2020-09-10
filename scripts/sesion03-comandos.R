## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 3. Distribuciones. Valores centrales y dispersión."

# Curvas de densidad de auto2, p. 4
require(gridExtra)
require(tidyverse)
require(haven)
auto2 = read_dta("../datos/auto2.dta")
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



set.seed(2019)
(muestra = sample(0:100, size = 20, replace = TRUE))
(media = mean(muestra))

set.seed(2019)
muestra = sample(0:100, size = 99, replace = TRUE) 
(media = mean(muestra))

muestra2 = c(muestra, 1000)
(media2 = mean(muestra2))

set.seed(2019)
(valores = sample(1:100, 17, replace = TRUE))

(ordenados = sort(valores))
(mediana = ordenados[9])

median(valores)

median(muestra)
median(muestra2)

quantile(mpg$cty, probs = 0.43)

summary(mpg$cty)

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

plot(density(muestra1, adjust = 2), col="blue", 
     xlim = c(-12, 12), lwd = 2, main = "", xlab= "")
lines(density(muestra2, adjust = 2), col="red", lwd = 2, sub="")

IQR(mpg$cty)

summary(mpg$cty)

unname(quantile(mpg$cty, probs = c(1/4, 3/4)) + c(-1, 1) * 1.5 * IQR(mpg$cty))



bxp_cty = boxplot(mpg$cty, col="orange")

bxp_cty$out

ggplot(mpg) + 
  geom_boxplot(mapping = aes(y = cty), fill="orange") +
  scale_x_discrete(breaks = c())

ggplot(mpg) + 
  geom_violin(mapping = aes(x=0, y = cty)) +
  scale_x_discrete(breaks = c()) +
  geom_boxplot(mapping = aes(y = cty), fill="green") +
  geom_jitter(aes(x=0, y = cty), 
              position = position_jitter(w=0.05, h= 0), col="blue")

library(readxl)
accidentes = read_excel("../datos/train_acc_2010.xls")
mad(accidentes$Speed, constant = 1)

var(mpg$displ)

n = length(mpg$displ)
media = mean(mpg$displ)
sum((mpg$displ - media)^2) / (n - 1 )

sd(mpg$displ)

sqrt(var(mpg$displ))

table(accidentes$TrkType)
prop.table(table(accidentes$TrkType))

library(viridisLite)
barplot(prop.table(table(accidentes$TrkType)), col=viridis(5))
ggplot(accidentes) + 
  geom_bar(mapping = aes(x = TrkType), fill= viridis(5))

fhs = read_csv("../datos/framingham.csv")
mean(fhs$male)
