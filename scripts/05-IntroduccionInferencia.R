## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "75%"----
par(mar = c(5.1,4.1,1,2.1)) 
set.seed(2019)
library(tidyverse)
datos = data.frame(x = rnorm(600), tipo = rep(1:20, each = 30))
medias = datos %>%
  group_by(tipo) %>%
  summarise(medias = mean(x)) %>%
  .$medias 
stripchart(x ~ tipo, data = datos, pch = 16, col="blue",
           xlab="Valores de la variable X", ylab = "Número de muestra")
segments(x0 = min(datos$x), y0 = 1:20, x1 = max(datos$x), y1 = 1:20,  col="blue")
abline(v = 0, lty=2, col= "red", lwd=5)
points(x = medias, y = 1:20, col="orange", pch=18, cex=3)


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
par(mar = c(5.1,4.1,1,2.1)) 
set.seed(2018)
library(tidyverse)
getCI = function(x){
  CI = t.test(x, alternative = "two.sided", mu = 0)$conf.int
  return(CI)
  }
datos = matrix(rt(3000, df = 29), nrow = 100)
intervalos = t(apply(datos, MARGIN = 1, FUN = getCI))
colores = ifelse(intervalos[,1] * intervalos[,2] < 0, "blue", "red")
plot(c(intervalos), rep(1:100,times = 2), col=rep(colores, 2), 
     xlab = "La línea de puntos indica la media poblacional real", ylab="")
segments(x0 = intervalos[,1], y0 = 1:100, x1 = intervalos[ ,2], y1 = 1:100,
         col=colores)
abline(v = 0, lty=2, col= "black", lwd=5)




## ------------------------------------------------------------------------
nc = 0.95
alfa = 1 - nc
(zc = qnorm(alfa / 2, lower.tail = FALSE)) # Atención, cola derecha


## ------------------------------------------------------------------------
n = 100
barX = 7.34
s = 0.31
(intervalo = barX + c(-1, 1) * zc * s / sqrt(n))


## ----echo=FALSE, message=FALSE-------------------------------------------
set.seed(2017)
n = 120
barX = 4.43
s = 0.31
library(MASS)
x = data.frame(x = mvrnorm(n, mu = barX, Sigma = s^2,empirical = TRUE)[,1])
write.table(x, "../datos/06-IntervConfNormalGrande.csv", row.names = FALSE, col.names = TRUE)



## ----echo=FALSE, eval=FALSE----------------------------------------------
## # La distribución t con manipulate
## require(manipulate)
## k <- 1000
## xvals <- seq(-5, 5, length = k)
## myplot <- function(df){
##   d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
##                   x = xvals,
##                   dist = factor(rep(c("Normal", "T"), c(k,k))))
##   g <- ggplot(d, aes(x = x, y = y))
##   g <- g + geom_line(size = 2, aes(color = dist))
##   g }
## manipulate(myplot(mu), mu = slider(1, 50, step = 1))




## ------------------------------------------------------------------------
1 - pt(2.5, df = 17)


## ------------------------------------------------------------------------
n = 20
nc = 0.95
alfa = 1 - nc
df = n - 1
(tc = qt(alfa / 2, df, lower.tail = FALSE)) # Atención, cola derecha


## ------------------------------------------------------------------------
rt(8, df = 19)


## ------------------------------------------------------------------------
datos = c(0.04, 0.05, 0.03, 0.06, 0.04, 0.06, 0.07, 0.03, 0.06, 0.02)
n = length(datos)
barX = mean(datos)
s = sd(datos)
nc = 0.95
alfa = 1 - nc
tc = qt(1 - alfa/2, df = n - 1)
(intervalo = barX + c(-1, 1) * tc * s / sqrt(n))


## ----echo=FALSE, fig.height=3--------------------------------------------
set.seed(2017)
# skewdata = rchisq(100, df = 3) + 5
# write.table(skewdata,file = "../datos/skewdata.csv", row.names = FALSE)
x = read.table(file = "http://www.postdata-statistics.com/docs/skewdata.csv", header = TRUE)[, 1]
hist(x, freq = FALSE, main=" ", ylim = c(0, 0.30), breaks = 15)
lines(density(x), col = "red")


## ------------------------------------------------------------------------
x = read.table(file = "http://www.postdata-statistics.com/docs/skewdata.csv", 
               header = TRUE)[, 1]


## ----bootstrap, echo=FALSE, message=FALSE, fig.align='center', out.width = "60%"----
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






## ----echo=FALSE, eval=FALSE----------------------------------------------
## require(manipulate)
## myplot <- function(dof){
##   curve(dchisq(x, df = dof), lwd=3, col="red", xlim=c(0, 3*dof))
##  }
## manipulate(myplot(dof), dof = slider(1, 30, step = 1))




## ------------------------------------------------------------------------
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


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "65%"----
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


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
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


## ----echo=FALSE, fig.align='center', message = FALSE, results='hide', warning=FALSE, out.width = "80%"----
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


## ----echo=FALSE, fig.align='center', message = FALSE, results='hide', warning=FALSE, out.width = "70%"----
par(mfrow = c(1, 2))
qqnorm(normales, main="Normal")
qqline(normales, lwd=2, col="red")
qqnorm(noNormales, main="No normal")
qqline(noNormales, lwd=2, col="red")
par(mfrow = c(1, 1))


## ----echo=FALSE, fig.align='center', message = FALSE, results='hide', warning=FALSE, out.width = "85%"----
par(mfrow=c(4, 4))
for(i in 1:16){
  plot(density(rnorm(15)), lwd=3, col="red", main="", xlab = "", ylab = "")
}
par(mfrow=c(1, 1))


## ----echo=FALSE, fig.align='center', out.width="11cm", message=FALSE, warning=FALSE----
par(mfrow=c(3, 3))
for(i in 1:9){
  muestra = rnorm(15)
  boxplot(muestra, main="", xlab = "", ylab = "")
  stripchart(muestra, method = "jitter", add = TRUE, vertical = TRUE)
}
par(mfrow=c(1, 1))




## ----echo=FALSE, eval=FALSE----------------------------------------------
## #####################################################################
## #####################################################################
## # p-valor usando el TCL
## #####################################################################
## #####################################################################
## 
## digits = 30
## 
## # Simulamos una muestra de una población normal como la del ejemplo.
## set.seed(2017)
## library(MASS)
## muestra = mvrnorm(n = 100, mu = 2.65, Sigma = 0.5^2, empirical = TRUE)
## 
## mu0 = 2.5
## (n = length(muestra))
## (xBar = mean(muestra))
## (s = sd(muestra))
## # El valor del estadístico es:
## (z = (xBar - mu0) / (s / sqrt(n)))
## # Y el p-valor es:
## (pValor = 1 - pnorm(z))
## 
## #####################################################################
## #####################################################################
## # p-valor por remuestreo
## #####################################################################
## #####################################################################
## 
## 
## # En el ejemplo de los canguros, supongamos que la Hipótesis nula tiene razón.
## # Concretamente,  suponemos que mu <= 2.5. Entonces podemos simular la
## # toma de muestras de la población de canguros tratados. Vamos a usar replicate
## # para hacer esto y ver qué fracción de esas muestras están de acuerdo con H0.
## # Estimamos p(H0 | datos muestrales)
## 
## numMuestras = 100000
## mediasRemuestras = replicate(n = numMuestras, {
##   remuestra = sample(muestra, 100, replace = TRUE)
##   mediaRemuestra = mean(remuestra)
## })
## # Hemos obtenido muchas medias muestrales. Las primeras son:
## head(mediasRemuestras)
## # ¿Qué proporción de estas medias muestrales está de acuerdo con H0?
## # Es fácil de obtener:
## sum(mediasRemuestras <= mu0) / numMuestras
## # Esta es otra manera de medir el p-valor y como ves, nos da una respuesta muy parecida al TCL.
## # hist(mediasRemuestras)








## ----echo=FALSE----------------------------------------------------------
n = 21
barX = 3.6
s = 0.6
mu0 = 4
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = pt(estadistico, df = n - 1)


## ------------------------------------------------------------------------
library(tidyverse)
(testCty = t.test(mpg$cty, mu = 16, 
                  alternative = "two.sided", conf.level = 0.95))


## ------------------------------------------------------------------------
testCty$p.value


## ------------------------------------------------------------------------
testCty$conf.int


## ------------------------------------------------------------------------
testDispl = t.test(mpg$displ,  mu = 3.4, 
                    alternative = "greater", conf.level = 0.95)
testDispl$conf.int


## ----echo=FALSE----------------------------------------------------------
n = 15
sigma0 = 0.5
s = 0.7
estadistico = (n - 1) * s^2 / sigma0^2
pValor = pchisq(estadistico, df = n - 1, lower.tail = FALSE)




## ------------------------------------------------------------------------
require(TeachingDemos)
(varTestCty = sigma.test(mpg$cty, sigmasq = 16, 
           alternative = "greater", conf.level = 0.95))


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "50%"----
require(asbio)
alfa = 0.01
s = 0.5
deltas = seq(0, s, length.out=1000)
n0 = 100

powers = power.z.test(sigma=s,n=n0,alpha=alfa,effect=deltas,test="one.tail",strict=FALSE)$power

plot(deltas, powers,pch=20,col="blue",lwd=0.6,ylab="Potencia",xlab=expression(delta==mu-mu[0]),font.lab=2,cex.axis=1.5,cex.lab=1.3)



## ------------------------------------------------------------------------
power.t.test(delta = 0.1, sd = 0.5, sig.level = 0.05,
             power = 0.80, type="one.sample", alternative="one.sided")





## ----echo=FALSE----------------------------------------------------------
n = 50
barX = 13.05
s = 0.6
mu0 = 13
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = 2 * pt(abs(estadistico), df = n - 1, lower.tail = FALSE)


## ----echo=FALSE----------------------------------------------------------
n = 5000
barX = 13.05
s = 0.6
mu0 = 13
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = 2 * pt(abs(estadistico), df = n - 1, lower.tail = FALSE)


## ----echo=FALSE----------------------------------------------------------
dCohen = (barX - mu0) / s


## ------------------------------------------------------------------------
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





## ------------------------------------------------------------------------
(planeta = list(nombre = "Marte", exterior = TRUE, 
                 radio = 3389.5, satelites = list("Fobos", "Deimos")))


## ------------------------------------------------------------------------
planeta[[1]]
planeta$exterior
planeta$satelites[[1]]


## ------------------------------------------------------------------------
planeta[1]
planeta["exterior"]


## ------------------------------------------------------------------------
(l1 = list("A", "B"))
(l2 = list(c("A", "B")))


## ----eval=FALSE----------------------------------------------------------
## (l3 = list(l2, "C"))


## ------------------------------------------------------------------------
l4 = append(l2, "D")
(l4 = c(l2, "D"))


## ------------------------------------------------------------------------
l4[3] = NULL
l4


## ------------------------------------------------------------------------
unlist(l1)


## ----echo=FALSE, eval=FALSE----------------------------------------------
## lista = list(letters[1:3], matrix(1:12, nrow = 3), TRUE)
## unlist(lista)


## ------------------------------------------------------------------------
ifelse(((1:5) < 3), yes = "A",  no = "B")


## ----eval=FALSE----------------------------------------------------------
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


## ----eval=FALSE, echo=FALSE----------------------------------------------
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


## ----eval=FALSE, echo=FALSE----------------------------------------------
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

