## ----echo=FALSE, eval=FALSE----------------------------------------------
## output:
##   beamer_presentation:
##     keep_tex: true
## classoption: "handout"


## ----set-options, echo=FALSE---------------------------------------------
options(width = 60)
library(knitr)
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "40%"-----
include_graphics("../fig/03-fig01-inferenciaPoblacionMuestra.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "30%", size="small"----
set.seed(2019)
N = 158000
poblacion = as.integer(2 * rchisq(N, df = 13), 0)


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "40%", size="small"----
# summary(poblacion)
hist(poblacion, main="", col="orange")
abline(v = mean(poblacion), lty=2, lwd=5, col="blue")


## ----echo=-c(1,4)--------------------------------------------------------
options(width= 90)
n = 20
(muestra = sample(poblacion, n, replace = TRUE))
options(width= 70)


## ----echo = -1-----------------------------------------------------------
options(width= 90)
(muestra2 = sample(poblacion, n, replace = TRUE))
mean(muestra2)


## ----echo = -1-----------------------------------------------------------
options(width= 90)
(muestra3 = sort(poblacion)[1:20])


## ----echo = -1-----------------------------------------------------------
options(width= 90)
mean(muestra3)


## ------------------------------------------------------------------------
k = 10000
# replicate repite k veces los comandos entre llaves y guarda el resultado
# del último comando en el vector mediasMuestrales
mediasMuestrales = replicate(k, { 
  muestra = sample(poblacion, n, replace = TRUE)
  mean(muestra)
})
head(mediasMuestrales, 10)


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "65%", size="small"----
hist(mediasMuestrales, breaks = 40, main="", 
     col="peachpuff", probability = TRUE, xlim=range(poblacion))
lines(density(mediasMuestrales), lwd=4, col="red")
lines(density(poblacion), lwd=4, col="blue")
abline(v = mean(poblacion), lty=2, lwd=5, col="blue")


## ------------------------------------------------------------------------
poblacion = sample(0:20, 20000, replace = TRUE)


## ------------------------------------------------------------------------
k = 10000
mediasMuestrales = replicate(k, { 
  muestra = sample(poblacion, n, replace = TRUE)
  mean(muestra)
})


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "45%", size="small"----
hist(mediasMuestrales, breaks = 40, main="", 
     col="peachpuff", probability = TRUE, xlim=range(poblacion))
lines(density(mediasMuestrales, adjust = 1.5), lwd=4, col="red")
lines(x = c(0, 0, 20, 20), c(0, 1/20, 1/20, 0), lwd=4, col="blue")
abline(v = mean(poblacion), lty=2, lwd=5, col="blue")


## ----eval=FALSE----------------------------------------------------------
## Otras poblaciones:
## 
## #####################################################################
## # Uniforme continua
## #####################################################################
## 
## tamPoblacion = 100000
## poblacion = runif(tamPoblacion, min = 0, max = 10)
## head(poblacion, 100)
## 
## 
## hist(poblacion)
## plot(density(poblacion))


## ----eval=FALSE----------------------------------------------------------
## mean(poblacion)
## 
## Tmuestra = 20
## 
## # ¿Cuántas muestras distintas hay?
## choose(tamPoblacion, Tmuestra)
## 
## # La población es moderadamente grande, pero el espacio de muestras es enorme.


## ----eval=FALSE----------------------------------------------------------
## # Vamos a tomar muchas muestras y en cada una calculamos una media muestral.
## numMuestras = 100000
## 
## # Repetiremos esto varias veces para hacernos una idea.
## (muestra = sample(poblacion, size = Tmuestra, replace = TRUE))
## mean(muestra)
## 
## mediasMuestrales = replicate(numMuestras, {
##   muestra = sample(poblacion, size = Tmuestra, replace = TRUE)
##   mean(muestra)
## })
## 
## # ¿Cómo se distibuyen esas medias muestrales?
## head(mediasMuestrales)
## hist(mediasMuestrales)
## hist(mediasMuestrales, breaks = 40, main="")
## plot(density(mediasMuestrales, adjust = 1.5), main="")


## ----eval=FALSE----------------------------------------------------------
## # ¿Cuál es la media de las me?
## (mu = mean(poblacion))
## mean(mediasMuestrales)
## 
## # ¿Cuál es su desviación típica?
## (desvTipPob = sqrt(sum((poblacion - mu)^2) / tamPoblacion))
## 
## sd(mediasMuestrales)
## desvTipPob / sqrt(Tmuestra)


## ------------------------------------------------------------------------
n = 366 # Número de personas en la sala

# Vamos a repetir el experimento N veces (N salas de n personas)
N = 10000
pruebas = replicate(N, {
  fechas = sort(sample(1:366, n, replace=TRUE)) 
  max(table(fechas)) # si el máximo es mayor que 1 es que 2 fechas coinciden
})
mean(pruebas > 1) # ¿qué proporción de salas tienen coincidencias?


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "30%"-----
include_graphics("../fig/03-fig02-DiagramaVennInterseccionSucesos.png")

