## ----message=FALSE, warning = FALSE, fig.align='center', out.width = "70%"----
library(tidyverse)
plot(mpg$hwy, mpg$cty, pch = 19, col = "blue", xlab = "hwy", ylab = "cty")


## ----message=FALSE, fig.align='center', out.width = "65%", results= "hold"----
library(tidyverse)
plt = ggplot(mpg) +
  geom_point(aes(hwy, cty), col = "darkgreen")
plt


## ----message=FALSE, fig.align='center', out.width = "50%"----------------
boxplot(cty ~ class, data = mpg, col= heat.colors(7), 
        las=2, cex.axis=0.75, xlab = "")
stripchart(cty ~ class, data = mpg, method = "jitter", 
           vertical = TRUE, pch = 19, col = "red", cex=0.3, add = TRUE)


## ----message=FALSE, fig.align='center', out.width = "80%"----------------
ggplot(mpg) +
  geom_density(aes(x = cty, color = class))


## ----message=FALSE, fig.align='center', out.width = "50%"----------------
library(lattice)
mpg$class = reorder(mpg$class, mpg$hwy, FUN = mean)
dotplot(class ~ cty, data = mpg, lwd= 2)


## ----message=FALSE, echo = -c(2, 6), fig.align='center', out.width = "60%"----
Tabla = table(mpg$year, mpg$class)
mosaicplot(Tabla, col=terrain.colors(nlevels(mpg$class)), las = 1)


## ----message=FALSE, fig.align='center', out.width = "60%"----------------
library(GGally)
ggpairs(iris, progress = FALSE, lower = list(combo = wrap("facethist", 
                                                          binwidth = 0.25)))


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "60%"----
set.seed(2017)
margins = par("mar")
par(mfrow = c(1, 3), mar = c(5, 2, 4, 2))
n = 100
x = sort(runif(n))
y1 = 3 + 2 * x + 0.3 * rnorm(n)
plot(x, y1, col="seagreen", pch=19, xlab="", ylab="")
y2 =  x * (1 - x) + 0.01 * rnorm(n)
plot(x, y2, col="seagreen", pch=19, xlab="", ylab="")
y3 =  2 * rnorm(n)
plot(x, y3, col="seagreen", pch=19, xlab="", ylab="")
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "55%"----
set.seed(2017)
n = 100
x = sort(runif(n))
y1 = 3 + 2 * x + 0.3 * rnorm(n)
plot(x, y1, col="seagreen", pch=19, xlab="", ylab="")
abline(lm(y1 ~ x), lwd= 3, col="blue")


## ----message=FALSE, fig.align='center', out.width = "70%"----------------
plt


## ------------------------------------------------------------------------
modelo = lm(cty ~ hwy, data = mpg)
modelo$coefficients


## ------------------------------------------------------------------------
b0 = modelo$coefficients[1]
b1 = modelo$coefficients[2]


## ----message=FALSE, fig.align='center', out.width = "40%"----------------
plt + 
  geom_abline(intercept = b0, slope = b1, color="blue", size = 1.5)
plt


## ------------------------------------------------------------------------
newHwy = 24.5
(ctyEstimado = b0 + b1 * newHwy)


## ------------------------------------------------------------------------
predict(modelo, newdata = data.frame(hwy = 24.5))




## ----message=FALSE, echo=FALSE, fig.align='center', out.width = "70%"----
set.seed(2017)
margins = par("mar")
par(mfrow = c(1, 3), mar = c(5, 2, 4, 2))
n = 100
x = sort(runif(n))
y1 = 3 + 2 * x + 0.3 * rnorm(n)
plot(x, y1, col="seagreen", pch=19, xlab="", ylab="")
abline(lm(y1 ~ x), col="red", lwd=2)
y2 =  x * (1 - x) + 0.01 * rnorm(n)
plot(x, y2, col="seagreen", pch=19, xlab="", ylab="")
abline(lm(y2 ~ x), col="red", lwd=2)
y3 =  2 * rnorm(n)
plot(x, y3, col="seagreen", pch=19, xlab="", ylab="")
abline(lm(y3 ~ x), col="red", lwd=2)
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))


## ------------------------------------------------------------------------
cor(mpg$hwy, mpg$cty)


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
margins = par("mar")
par(mfrow = c(1, 3), mar = c(5, 2, 4, 2))

set.seed(2017)
n = 100
x = sort(runif(n))
y = 3 + 2 * x + 0.3 * rnorm(n)
plot(x, y, col="seagreen", pch=19, xlab="", ylab="", cex=0.5)
abline(lm(y ~ x), col="red", lwd=2)
mtext(text = paste0("r = ", signif(cor(x, y), 2),
                    collapse = ""), line = 2, side = 1, cex=0.75)

set.seed(2017)
n = 150
x = sort(runif(n, min = -1, max = 1))
y = x +  2 * sin(20 * x)/10 + rnorm(n)/50
plot(x, y, col="seagreen", pch=19, xlab="", ylab="", cex=0.5)
abline(lm(y ~ x), col="red", lwd=2)
mtext(text = paste0("r = ", signif(cor(x, y), 2),
                    collapse = ""), line = 2, side = 1, cex=0.75)

set.seed(2017)
n = 150
x = sort(runif(n, min = -1, max = 1))
y = floor(3 * x) + rnorm(n)/20
plot(x, y, col="seagreen", pch=19, xlab="", ylab="", cex=0.5)
abline(lm(y ~ x), col="red", lwd=2)
mtext(text = paste0("r = ", signif(cor(x, y), 2),
                    collapse = ""), line = 2, side = 1, cex=0.75)
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
# muestras = list(A1 = 1, A2 = 2, A3 = 3, A4 = 4)
par(mfrow = c(2, 2))
anscombeModels = sapply(1:4, function(k){
  anscombe %>% 
    select(ends_with(as.character(k))) %>% 
    rename(x = 1, y = 2) %>% 
    do(
      lm(y ~ x, data = .) %>% 
        (function(m){
          plot(m$model[,2:1], col="seagreen", pch=19, xlab="", ylab="", cex=1.5)
          cffs = coefficients(m)
          abline(a = cffs[1], b = cffs[2], col="red", lwd = 4)
          c(coefficients(m),cor(m$model)[1, 2])
        }) %>%
        as.data.frame)
  })
par(mfrow = c(1, 1))
b0 =  signif(anscombeModels[[1]][1], 3)
b1 =  signif(anscombeModels[[1]][2], 3)
r =  signif(anscombeModels[[1]][3], 3)


## ------------------------------------------------------------------------
modelo = lm(hwy ~ cty, data = mpg)


## ------------------------------------------------------------------------
(R2 = cor(mpg$hwy, mpg$cty)^2)




## ----message=FALSE, fig.align='center', out.width = "60%"----------------
set.seed(2019); colores = rainbow(5)
plot(x=c(0, 1), y=c(-1, 7), type = "n", xlab="x", ylab="y")
for(k in 1:5){
  x = runif(30) 
  y = 4 - 2 * x + rnorm(30, mean = 0, sd = 1)
  points(x, y, col=alpha(colores[k], 0.8), pch="·", cex=2)
  abline(lm(y ~ x), col="blue", lwd=5)
}
abline(a = 4, b = -2, lwd=8, lty = 1, col="red")


## ------------------------------------------------------------------------
set.seed(2019); 
beta0 = 4; beta1 = -2; n = 30
x = runif(n) 
y = beta0 + beta1 * x + rnorm(n, mean = 0, sd = 1)


## ----message=FALSE, fig.align='center', out.width = "40%"----------------
modelo = lm(y ~ x)
plot(x, y, col=alpha("blue", 0.8), pch=19)
abline(modelo, col="blue", lwd=5)


## ------------------------------------------------------------------------
(sumModelo = summary(modelo))


## ------------------------------------------------------------------------
sumModelo$sigma


## ------------------------------------------------------------------------
sqrt(sum(modelo$residuals^2)/(modelo$df))


## ------------------------------------------------------------------------
confint(modelo)


## ------------------------------------------------------------------------
tc = qt(1 - 0.025, df = n - 2) # valor crítico de la t de Student, df = n- 2
# Busca el siguiente valor en la salida de summary(lm)
(seB1 = sumModelo$sigma / sqrt(sum((x - mean(x))^2))) 
# Y ahora el intervalo
(intervalo = coefficients(modelo)[2] + c(-1, 1) * tc * seB1)


## ------------------------------------------------------------------------
sumModelo$coefficients


## ----echo=FALSE, results='hide'------------------------------------------
(tValue = coefficients(modelo)[2] /  seB1)
(pValor = 2 * pt(abs(tValue), df = n - 2, lower.tail = FALSE))


## ------------------------------------------------------------------------
nuevoX = data.frame(x = 1/2)
predict(modelo, newdata = nuevoX, interval = "confidence")
predict(modelo, newdata = nuevoX, interval = "prediction")


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
# Vamos a hacer intervalos para muchos valores de x
newXs = data.frame(x=seq(min(x), max(x), length.out=50))
# Calculamos los extremos de ambas bandas con predict
preBand = predict(modelo, int="prediction", newdata=newXs)
confBand = predict(modelo, int="confidence", newdata=newXs)
# Creamos el gráfico
plot(x, y, ylim= range(y, preBand,na.rm=T), pch=19, lwd=5, col="black")
# Estas son las funciones que deibuan las bandas
matlines(newXs$x, preBand[, -1], lty=c(2,2), col="red", lwd=5)
matlines(newXs$x, confBand[, -1], lty=c(4,4), col="darkgreen", lwd=5)
# Añadimos la recta de regresión
abline(modelo, lwd= 5, col= "blue")


## ----eval=FALSE, echo=FALSE----------------------------------------------
## library(gvlma)
## gvlma(modelo)


## ----message=FALSE, fig.align='center', out.width = "40%"----------------
plotModelo = plot(modelo, which = 1, pch=19, lwd= 4)
segments(x0 = c(2.3, 2.3), y0 = c(1, -1), x1 = c(3.7, 3.7), y1 = c(3, -2), 
         lty=3, lwd=4, col="green")


## ----message=FALSE, fig.align='center', out.width = "70%"----------------
plotModelo = plot(modelo, which = 2, pch=19)


## ----message=FALSE, fig.align='center', out.width = "60%"----------------
plotModelo = plot(modelo, which = 3, pch=19)


## ----message=FALSE, fig.align='center', out.width = "60%"----------------
set.seed(2019)
n=100
x = sort(signif(runif(n, min = 0, max = 1), digits=2) )  
y = 1 - (x/2) + rnorm(n, sd = 0.01*(1 + 50 * x))
par(mfrow=c(2, 2))
plot(x, y)
abline(lm(y ~x), col="red", lwd=2)
plot(lm(y ~x), which = 1:3)
par(mfrow=c(1, 1))




## ----echo = FALSE, message=FALSE, fig.align='center', out.width = "65%"----
par(mfrow = c(1, 2))
set.seed(2019)
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  
plot(lm(y ~ x), which = 5, pch=19)
par(mfrow = c(1, 1))


## ------------------------------------------------------------------------
  set.seed(2019)
  n <- 100
  x <- c(10, rnorm(n))
  y <- c(10, c(rnorm(n)))
  modelo = lm(y ~x)


## ------------------------------------------------------------------------
head(hatvalues(modelo))


## ------------------------------------------------------------------------
head(anscombe, 3)


## ------------------------------------------------------------------------
library(tidyverse)
head(table2, 4)




## ----echo = FALSE--------------------------------------------------------
library(datasets)
head(USArrests)


## ------------------------------------------------------------------------
USArrests %>% 
  gather("Murder", "Assault", "Rape", key = "Felony", value = "ratePer100K") %>% 
  head(5)


## ----echo = FALSE--------------------------------------------------------
head(table2, 4)


## ------------------------------------------------------------------------
table2 %>% 
  spread(key = "type", value = "count")


## ----echo=FALSE----------------------------------------------------------
set.seed(2019)
codigo = paste0(sample(1:5, 6, replace = TRUE), "/", sample(LETTERS[1:2], 6, replace = TRUE))
(datos = data.frame(x = sample(1:10, 6, replace = TRUE), codigo))


## ------------------------------------------------------------------------
datos %>% 
  separate("codigo", into = c("Numero", "Letra"))

