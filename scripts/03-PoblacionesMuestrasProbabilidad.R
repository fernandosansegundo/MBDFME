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


## ----eval=FALSE----------------------------------------------------------
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

