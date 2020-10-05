## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 5. Introducción a la Inferencia Estadística."

## El error es aleatorio porque la muestra es aleatoria.
  
par(mar = c(5.1,4.1,1,2.1)) 
set.seed(2019)
library(tidyverse)
# generamos los datos, 20 muestras (se distinguen por su "tipo")
datos = data.frame(x = rnorm(600), tipo = rep(1:20, each = 30))
# calculamos las medias por muestra con dplyr
medias = datos %>%
  group_by(tipo) %>%
  summarise(medias = mean(x)) %>%
  .$medias 
# Usamos stripchart para dibujar las medias por tipo  
stripchart(x ~ tipo, data = datos, pch = 16, col="blue",
           xlab="Valores de la variable X", ylab = "Número de muestra")
# Añadimos líneas horizontales para ayudar a visualizar
segments(x0 = min(datos$x), y0 = 1:20, x1 = max(datos$x), y1 = 1:20,  col="blue")
# La línea vertical central marca la media
abline(v = 0, lty=2, col= "red", lwd=5)
# Y los puntos naranjas son las medias muestrales
points(x = medias, y = 1:20, col="orange", pch=18, cex=3)

# Código para generar la figura de los intervalos de confianza
par(mar = c(5.1,4.1,1,2.1)) 
set.seed(2018)
library(tidyverse)
# Esta función calcula un intervalo de confianza para cada muestra
# Pronto veremos como funciona
getCI = function(x){
  CI = t.test(x, alternative = "two.sided", mu = 0)$conf.int
  return(CI)
}
# Generamos las muestras
datos = matrix(rt(3000, df = 29), nrow = 100)
# y los correspondientes intervalos
intervalos = t(apply(datos, MARGIN = 1, FUN = getCI))
# los colores dependen de que el intervalo capture la media poblacional
# que en nuestro caso es 0
colores = ifelse(intervalos[,1] * intervalos[,2] < 0, "blue", "red")
# Ahora pintamos los extremos de los intervalos
plot(c(intervalos), rep(1:100,times = 2), col=rep(colores, 2), 
     xlab = "La línea de puntos indica la media poblacional real", ylab="")
# Los segmentos que los conectan
segments(x0 = intervalos[,1], 
         y0 = 1:100, 
         x1 = intervalos[ ,2], 
         y1 = 1:100,
         col=colores)
# Y una línea vertical en la media poblacional
abline(v = 0, lty=2, col= "black", lwd=5)

## Valores críticos con R.
nc = 0.95
alfa = 1 - nc
(zc = qnorm(alfa / 2, lower.tail = FALSE)) # Atención, cola derecha

## Intervalos de confianza con R.
n = 100
barX = 7.34
s = 0.31
(intervalo = barX + c(-1, 1) * zc * s / sqrt(n))

# La distribución t vs Z

k = 1000
xvals = seq(-5, 5, length = k) 
d = data.frame(y = c(dnorm(xvals), dt(xvals, df = 2)), 
                x = xvals,
                dist = factor(rep(c("Normal", "T"), c(k,k)))) 
g = ggplot(d, aes(x = x, y = y))
g = g + geom_line(size = 2, aes(color = dist))
g 

## La distribución $t$ en R.

## Función pt

1 - pt(2.5, df = 17)

# Valores críticos con qt
n = 20
nc = 0.95
alfa = 1 - nc
df = n - 1
(tc = qt(alfa / 2, df, lower.tail = FALSE)) # Atención, cola derecha

# Valores aleatorios con rt
rt(8, df = 19)

# Intervalo de confianza con t
datos = c(0.04, 0.05, 0.03, 0.06, 0.04, 0.06, 0.07, 0.03, 0.06, 0.02)
n = length(datos)
barX = mean(datos)
s = sd(datos)
nc = 0.95
alfa = 1 - nc
tc = qt(1 - alfa/2, df = n - 1)
(intervalo = barX + c(-1, 1) * tc * s / sqrt(n))

## Intervalos de confianza por bootstrap.
# set.seed(2017)
# skewdata = rchisq(100, df = 3) + 5
# write.table(skewdata,file = "./datos/skewdata.csv", row.names = FALSE)
url = "https://raw.githubusercontent.com/fernandosansegundo/MBDFME/master/datos/skewdata.csv"
x = read.table(file = url, header = TRUE)[, 1]
hist(x, freq = FALSE, main=" ", ylim = c(0, 0.30), breaks = 15)
lines(density(x), col = "red")

url = paste0("https://raw.githubusercontent.com/fernandosansegundo",
             "/MBDFME/master/datos/skewdata.csv")
x = read.table(file = url, header = TRUE)[, 1]

# Creamos la "caja" del gráfico.
plot(c(0, 40), c(5,10.5), type="n", xlab="Tamaño muestral", ylab="") 

for (k in seq(5, 40, 1)){ # Este bucle recorre los tamaños muestrales
  a =  numeric(10000) # el vector a almacenará las medias muestrales
  for (i in 1:10000){ # este es el bucle de remuestreo (bootstrap)
  # generamos un remuestreo con reemp. y calculamos su media
    a[i] = mean(sample(x, k, replace=T)) 
    }
  # dibujo del intervalo bootstrap de este tamaño muestral  
  points(c(k,k), quantile(a, c(.025,.975)), type="o", 
         col = "orange", lwd= 3) 
}

# el siguiente bloque de código genera una banda con 
# los intervalos clásicos correspondientes a esas muestras.
xv = seq(5, 40, 0.1) 
yv = mean(x) - qt(0.975, xv) * sqrt(var(x) / xv)
lines(xv, yv, lty = 2, col = "black", lwd = 4)
yv = mean(x) + qt(.975, xv) * sqrt(var(x) / xv)
lines(xv, yv, lty = 2, col = "black", lwd = 4)

# añadimos una línea horizontal en la media
abline(h = mean(x), col="blue", lwd=2) 

## Chi cuadrado: intervalos de confianza para la varianza.

# Estos son los valores muestrales y el nc deseado
varianza = 62 # cuidado si el dato muestral es s y no s^2
n = 7
nc = 0.95
(alfa = 1 - nc)

# Calculamos dos valores críticos de chi cuadrado.
(chi1 = qchisq(alfa / 2, df = n - 1, lower.tail = FALSE)) # cola derecha
(chi2 = qchisq(alfa/2, df = n - 1)) # cola izquierda

# Construimos el intervalo
(intervalo = (n - 1) * varianza / c(chi1, chi2))

# Analizando la normalidad de una población a partir de muestras

# Primero con datos normales

par(mfrow = c(1, 2))
set.seed(2017)
tamMuestra = 500
normales = rnorm(tamMuestra)
altDens = density(normales)
histNoPlot = hist(normales, plot=FALSE)
maxY = max(altDens$y[which.max(altDens$y)], max(histNoPlot$density))
hist(normales, #breaks = cortes, 
     cex.lab=0.7, cex.axis = 0.6,
     ylab = "", xlab="", main="Normal", 
     freq = FALSE, ylim = c(0, maxY), col="#eec591")
title(ylab="Frecuencias", line=2, cex.lab=0.7)
lines(altDens, col="red", lwd=4)

# Y ahora con datos no normales

set.seed(2017)
tamMuestra = 500
noNormales = rchisq(tamMuestra, df = 4)
altDens = density(noNormales)
histNoPlot = hist(noNormales, plot=FALSE)
maxY = max(altDens$y[which.max(altDens$y)], max(histNoPlot$density))
hist(noNormales, #breaks = cortes, 
     cex.lab=0.7, cex.axis = 0.6,
     ylab = "", xlab="", main="No Normal", 
     freq = FALSE, ylim = c(0, maxY), col="#eec591")
title(ylab="Frecuencias", line=2, cex.lab=0.7)
lines(altDens, col="red", lwd=4)
par(mfrow = c(1, 1))

# Boxplots para analizar la simetría.
par(mfrow = c(1, 2))
boxplot(normales, main="Normal", col = "lightblue")
stripchart(normales, method = "jitter", 
           vertical = TRUE, add = TRUE, 
           pch=19, col="blue", cex=0.3)
boxplot(noNormales, main="No normal", col = "lightblue")
stripchart(noNormales, method = "jitter", 
           vertical = TRUE, add = TRUE, 
           pch=19, col="blue", cex=0.3)
par(mfrow = c(1, 1))

# Violinplot
par(mfrow = c(1, 2))
library(vioplot)
vioplot(normales, col = "lightblue")
stripchart(normales, method = "jitter", 
           vertical = TRUE, add = TRUE, 
           pch=19, col="blue", cex=0.3)

title("Normal")
vioplot(noNormales, col = "lightblue")
stripchart(noNormales, method = "jitter", 
           vertical = TRUE, add = TRUE, 
           pch=19, col="blue", cex=0.3)
title("No normal")
par(mfrow = c(1, 1))

# Violinplot con ggplot
ggplot(diamonds, aes(x = cut, y = depth, color=cut)) + 
  geom_violin(show.legend = FALSE) + 
  geom_boxplot(width=0.2, show.legend = FALSE)

## QQplots.

par(mfrow = c(1, 2))
qqnorm(normales, main="Normal")
qqline(normales, lwd=2, col="red")
qqnorm(noNormales, main="No normal")
qqline(noNormales, lwd=2, col="red")
par(mfrow = c(1, 1))

# Precaución al analizar la normalidad con muestras pequeñas

# Densidades...

par(mfrow=c(4, 4))
for(i in 1:16){
  plot(density(rnorm(15)), lwd=3, col="red", main="", xlab = "", ylab = "")
}
par(mfrow=c(1, 1))

# ... y boxplots

par(mfrow=c(3, 3))
for(i in 1:9){
  muestra = rnorm(15)
  boxplot(muestra, main="", xlab = "", ylab = "")
  stripchart(muestra, method = "jitter", add = TRUE, vertical = TRUE)
}
par(mfrow=c(1, 1))

#####################################################################
# Contraste de Hipótesis
#####################################################################

# p-valor usando el TCL

digits = 30

# Simulamos una muestra de una población normal como la del ejemplo.
set.seed(2017)
library(MASS)
muestra = mvrnorm(n = 100, mu = 2.65, Sigma = 0.5^2, empirical = TRUE)

mu0 = 2.5
(n = length(muestra))
(xBar = mean(muestra))
(s = sd(muestra))
# El valor del estadístico es: 
(z = (xBar - mu0) / (s / sqrt(n)))
# Y el p-valor es:
(pValor = 1 - pnorm(z))

###    
# p-valor por remuestreo

# En el ejemplo de las baterías de vehículos, supongamos 
# que la Hipótesis nula tiene razón. 
# Concretamente,  suponemos que mu <= 2.5. Entonces podemos simular 
# la toma de muestras de la población de baterías del nuevo método. 
# Vamos a usar replicate para hacer esto y ver qué fracción de 
# esas muestras están de acuerdo con H0.
# Estimamos p(H0 | datos muestrales)

numMuestras = 100000
mediasRemuestras = replicate(n = numMuestras, {
  remuestra = sample(muestra, 100, replace = TRUE)
  mediaRemuestra = mean(remuestra)
})
# Hemos obtenido muchas medias muestrales. Las primeras son:
head(mediasRemuestras)
# ¿Qué proporción de estas medias muestrales está de acuerdo con H0?
# Es fácil de obtener:
sum(mediasRemuestras <= mu0) / numMuestras
# Esta es otra manera de medir el p-valor y como ves, nos 
# da una respuesta muy parecida al TCL.
# hist(mediasRemuestras)

###    
# t de Student: contrastes sobre la media 
# con muestras pequeñas en variables normales.

n = 21
barX = 3.6
s = 0.6
mu0 = 4
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = pt(estadistico, df = n - 1)

# La función t.test

library(tidyverse)
(testCty = t.test(mpg$cty, mu = 16, 
                  alternative = "two.sided", conf.level = 0.95))

testCty$p.value

testCty$conf.int

testDispl = t.test(mpg$displ,  mu = 3.4, 
                    alternative = "greater", conf.level = 0.95)
testDispl$conf.int

## Contraste sobre la desviación típica.

n = 15
sigma0 = 0.5
s = 0.7
estadistico = (n - 1) * s^2 / sigma0^2
pValor = pchisq(estadistico, df = n - 1, lower.tail = FALSE)

# Usando TeachingDemos
require(TeachingDemos)
(varTestCty = sigma.test(mpg$cty, sigmasq = 16, 
           alternative = "greater", conf.level = 0.95))

# Gráfica de las curvas de potencia

alfa = 0.01
s = 0.5
deltas = seq(0, s, length.out=1000)
n0 = 100
powers = power.t.test(sd = s, n=n0, sig.level =alfa, delta = deltas, 
                      type = "one.sample", alternative = "two.sided", strict=FALSE)$power
plot(deltas, powers,pch=20,col="blue",lwd=0.6,ylab="Potencia",xlab=expression(delta==mu-mu[0]),font.lab=2,cex.axis=1.5,cex.lab=1.3)

## Tamaño muestral y potencia del contraste.

power.t.test(delta = 0.1, sd = 0.5, sig.level = 0.05,
             power = 0.80, type="one.sample", alternative="one.sided")

## Significación estadística vs relevancia científica. 

n = 50
barX = 13.05
s = 0.6
mu0 = 13
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = 2 * pt(abs(estadistico), df = n - 1, lower.tail = FALSE)

# ahora con una muestra mucho más grande:

n = 5000
barX = 13.05
s = 0.6
mu0 = 13
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = 2 * pt(abs(estadistico), df = n - 1, lower.tail = FALSE)

## Relevancia y la d de Cohen.

dCohen = (barX - mu0) / s

## Simulando contrastes múltiples con R.

set.seed(2019)
nTests = 20 # Haremos 20 contrastes
# y este vector los 20 p-valores
pValores = numeric(nTests)
# Ahora hacemos los contrastes y guardamos los p-valores
for(i in 1:nTests){
  muestra = c(rnorm(15))
  pValores[i] = t.test(muestra, alternative = "two.sided", mu = 0)$p.value
}
# ¿Cuál es el p-valor más pequeño?
min(pValores)

#########################################################
## Complementos de R
## 
#########################################################

## Familia apply.

# apply 

options(width = 80)
set.seed(2019)
M = matrix(rnorm(100 * 5), ncol = 5)
head(M, 3)
apply(M, MARGIN = 2, 
      FUN = function(x)t.test(x, alternative = "two.sided")$conf.int)

# lapply

L = list(A = iris, B = matrix(1:12, nrow = 3), 
         C = table(mpg$cyl), D = iris)
lapply(L, FUN = dim)

# sapply

(muestras = sapply(4:8, function(k)rbinom(n = 4, size = k, prob = 1/3)))

# tapply

tapply(mpg$cty, INDEX = mpg$class, FUN = mean)

# aggregate

aggregate(cty ~ class, data = mpg, FUN = mean)

# con dplyr (preferible!)

mpg %>% 
  group_by(class) %>% 
  summarize(mean(cty))

##
## Datos limpios (tidy data).

# Ejemplo de datos no limpios 1

head(anscombe, 3)

# Ejemplo de datos no limpios 2

head(table2, 4)

# Ejemplo de gather.

library(datasets)
head(USArrests)

USArrests %>% 
  gather("Murder", "Assault", "Rape", 
  key = "Felony", 
  value = "ratePer100K") %>% 
  sample_n(4)

# Ejemplo de spread.

head(table2, 4)

table2 %>% 
  spread(key = "type", value = "count")

# Ejemplo de separate.

set.seed(2019)
codigo = paste0(sample(1:5, 6, replace = TRUE), "/", sample(LETTERS[1:2], 6, replace = TRUE))
(datos = data.frame(x = sample(1:10, 6, replace = TRUE), codigo))

datos %>% 
  separate("codigo", into = c("Numero", "Letra"))

# Ejemplo de pivot_longer.

head(USArrests)

USArrests %>% 
  pivot_longer(cols = c(Murder, Assault, Rape), 
               names_to = "felony", 
               values_to = "ratePer100K") %>% 
  sample_n(4)

# Ejemplo de pivot_wider. 

head(table2, 4)

table2 %>% 
  pivot_wider(names_from = type, values_from = count)
