## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
set.seed(2017)
n = 100
x = sort(runif(n))
y1 = 3 + 2 * x + 0.3 * rnorm(n) # para que la muestra sea la misma que antes
y =  x * (1 - x) + 0.01 * rnorm(n)
plot(x, y, col="seagreen", pch=19, xlab="", ylab="")


## ----echo=2--------------------------------------------------------------
modeloParabola1 = lm(y ~ I(x) + I(x^2))
modeloParabola = lm(y ~ poly(x, 2))
xParabola = seq(min(x), max(x), length.out = 100)
yParabola = predict(modeloParabola, 
                    newdata = data.frame(x = xParabola ))



## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "45%"----
plot(x, y, col="seagreen", pch=19, xlab="", ylab="")
lines(xParabola, yParabola, lwd= 3, col= "red")
yP = predict(modeloParabola, newdata = data.frame(x = 0.7))
points(0.7, yP, pch = 19, cex=3.5, col="orange")




## ----message=FALSE, fig.align='center', out.width = "60%"----------------
library(tidyverse)
datos = data.frame(x, y)
ggplot(datos) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y), method="lm", formula = y ~ poly(x, 2))


## ----message=FALSE, fig.align='center', out.width = "55%"----------------
ggplot(mpg) + 
  geom_point(aes(displ, cty)) + 
  geom_smooth(aes(displ, cty))





## ----message=FALSE, fig.align='center', out.width = "60%"----------------
childData = data.frame(
wgt = c(64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68), 
hgt = c(57, 59, 49, 62, 51, 50, 55, 48, 42, 42, 61, 57), 
age = c(8, 10, 6, 11, 8, 7, 10, 9, 10, 6, 12, 9))
ggplot(childData) +
      geom_point(aes(age, wgt)) + 
      geom_smooth(aes(age, wgt), method="lm")


## ----echo=FALSE----------------------------------------------------------
  modelo1 = lm(wgt ~ age, data = childData)
  summary(modelo1)


## ------------------------------------------------------------------------
    modelo2 = lm(wgt ~ age + hgt, data = childData)
    summary(modelo2)




## ------------------------------------------------------------------------
modelo2$coefficients


## ------------------------------------------------------------------------
modelo_yx2 =  lm(wgt ~ hgt, data = childData)
modelo_x1x2 =  lm(age ~ hgt, data = childData)


## ------------------------------------------------------------------------
sum(residuals(modelo_yx2) * residuals(modelo_x1x2)) / sum(residuals(modelo_x1x2)^2)


## ------------------------------------------------------------------------
nuevosDatos = data.frame(age = c(9, 9, 9), hgt = c(52, 53, 54))
(pesosPredichos = predict(modelo2, newdata = nuevosDatos))


## ------------------------------------------------------------------------
diff(pesosPredichos)


## ------------------------------------------------------------------------
confint(modelo2)


## ------------------------------------------------------------------------
anova(modelo2)


## ----echo=FALSE, eval=FALSE----------------------------------------------
## aov(modelo1)
## (summ2 = summary(modelo2))
## (aov2 = anova(modelo2))
## 
## modelo3 = lm(formula = wgt ~ hgt + age, data = childData)
## (aov3 = anova(modelo3))
## 
## sum(aov2$`Sum Sq`[1:2]) / sum(aov2$`Sum Sq`)
## summ2$r.squared


## ----message=FALSE, fig.align='center', out.width = "60%"----------------
ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Length, color=Species))


## ------------------------------------------------------------------------
modelo = lm(Sepal.Length ~ Species, iris)


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "60%"----
library(latex2exp)
boxplot(Sepal.Length ~ Species, data = iris, ylim = c(0, 8), lwd = 1)
medias = aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)[,2]
arrows(x0 = 1:3, y0 = c(0, medias[1], medias[1]), 
       x1 = 1:3, y1 = medias, col= c("red", "darkgreen", "blue"), lwd= 5)
abline( h = medias[1], col="red", lty = 2, lwd = 4 )
text(x=1, y= medias[1]/2, label=TeX("$\\beta_0 = \\mu_1$"), col="red", cex = 2)
text(x=2, y= medias[1] - 0.5, label=TeX("$\\beta_1 = \\mu_2 - \\mu_1$"), col="darkgreen", cex = 2)
text(x=3.1, y= medias[1] - 0.5, label=TeX("$\\beta_2 = \\mu_3 - \\mu_1$"), col="blue", cex = 2)


## ------------------------------------------------------------------------
(medias = aggregate(Sepal.Length ~ Species, iris, FUN = mean)[,2])


## ------------------------------------------------------------------------
c(medias[1], medias[2] - medias[1], medias[3] - medias[1])


## ------------------------------------------------------------------------
modelo = lm(Sepal.Length ~ Species, iris)
(coefs = modelo$coefficients)


## ------------------------------------------------------------------------
(sumModelo = summary(modelo))


## ------------------------------------------------------------------------
options(width = 80)
set.seed(2019)
M = matrix(rnorm(100 * 5), ncol = 5)
head(M)
apply(M, MARGIN = 2, 
      FUN = function(x)t.test(x, alternative = "two.sided")$conf.int)


## ------------------------------------------------------------------------
L = list(A = iris, B = matrix(1:12, nrow = 3), 
         C = table(mpg$cyl), D = iris)
lapply(L, FUN = dim)


## ------------------------------------------------------------------------
(muestras = sapply(4:8, function(k)rbinom(n = 4, size = k, prob = 1/3 )))


## ------------------------------------------------------------------------
tapply(mpg$cty, INDEX = mpg$class, FUN = mean)


## ------------------------------------------------------------------------
aggregate(cty ~ class, data = mpg, FUN = mean)


## ------------------------------------------------------------------------
mpg %>% group_by(class) %>% summarize(mean(cty))


## ----message = FALSE-----------------------------------------------------
library(lubridate)
fechaTexto = "21-07-1969 02:56UTC"
(fecha = dmy_hm(fechaTexto))


## ------------------------------------------------------------------------
day(fecha)
month(fecha)
hour(fecha)


## ------------------------------------------------------------------------
fecha + days(100)


## ------------------------------------------------------------------------
fecha + hours(1000)  + minutes(42)


## ------------------------------------------------------------------------
  fecha2 = dmy_hm("29-10-1969 00:00")
  fecha2 - fecha

