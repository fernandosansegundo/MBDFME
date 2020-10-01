## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 6. Introducción a los modelos. Regresión lineal simple.

########################################################################
########################################################################
########################################################################
########################################################################

# Diagrama de dispersión
library(tidyverse)
plot(mpg$hwy, mpg$cty, pch = 19, col = "blue", xlab = "hwy", ylab = "cty")

# con ggplot
library(tidyverse)
plt = ggplot(mpg) +
  geom_point(aes(hwy, cty), col = "darkgreen")
plt

# Una variable continua $X$ y un factor $F$.
boxplot(cty ~ class, data = mpg, col= heat.colors(7), 
        las=2, cex.axis=0.75, xlab = "")
stripchart(cty ~ class, data = mpg, method = "jitter", 
           vertical = TRUE, pch = 19, col = "red", cex=0.3, add = TRUE)

# curvas de densidad por grupos
ggplot(mpg) +
  geom_density(aes(x = cty, color = class))

# Invirtiendo los papeles de $X$ y $F$
library(lattice)
mpg$class = reorder(mpg$class, mpg$hwy, FUN = mean)
dotplot(class ~ cty, data = mpg, lwd= 2)

# Dos factores., mosaicplot
Tabla = table(mpg$year, mpg$class)
mosaicplot(Tabla, col=terrain.colors(nlevels(mpg$class)), las = 1)

# La función `table` para dos factores
table(mpg$cyl, mpg$class, dnn = c("Cilindros", "Tipo"))

# Matrices de gráficos de correlación.
library(GGally)
ggpairs(iris, progress = FALSE, 
        lower = list(combo = wrap("facethist", binwidth = 0.25)))

# Ejemplos de relaciones *ruidosas*.
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

# La recta de regresión.
set.seed(2017)
n = 100
x = sort(runif(n))
y1 = 3 + 2 * x + 0.3 * rnorm(n)
plot(x, y1, col="seagreen", pch=19, xlab="", ylab="")
abline(lm(y1 ~ x), lwd= 3, col="blue")

# La recta de regresión con R.
plt

# Construyendo el modelo
modelo = lm(cty ~ hwy, data = mpg)
modelo$coefficients

# Coeficientes del modelo
b0 = modelo$coefficients[1]
b1 = modelo$coefficients[2]

# Añadimos la recta al diagrama
plt + 
  geom_abline(intercept = b0, slope = b1, color="blue", size = 1.5)
plt

# Predicción con el modelo lineal.

newHwy = 24.5
(ctyEstimado = b0 + b1 * newHwy)

# Usando predict
predict(modelo, newdata = data.frame(hwy = 24.5))

# Bondad del ajuste (goodness of fit).

# La mejor recta puede ser muy mala.
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

# Coeficiente de correlación.
cor(mpg$hwy, mpg$cty)

# Ejemplos de coeficientes de correlación.
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

#  El cuarteto de Anscombe.

par(mfrow = c(2, 2))
anscombeModels = sapply(1:4, function(k){
  anscombe %>% 
    select(ends_with(as.character(k))) %>% 
    rename(x = 1, y = 2) %>% 
    do(
      lm(y ~ x, data = .) %>% 
        (function(m){
          plot(m$model[,2:1], col="seagreen", 
               pch=19, xlab="", ylab="", cex=1.5)
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

# porcentaje de variabilidad total explicado por el modelo
modelo = lm(hwy ~ cty, data = mpg)

(R2 = cor(mpg$hwy, mpg$cty)^2)

# Simulación de muestras, recta muestral y recta poblacional.
set.seed(2019); colores = rainbow(5)
plot(x=c(0, 1), y=c(-1, 7), type = "n", xlab="x", ylab="y")
for(k in 1:5){
  x = runif(30) 
  y = 4 - 2 * x + rnorm(30, mean = 0, sd = 1)
  points(x, y, col=alpha(colores[k], 0.8), pch="·", cex=2)
  abline(lm(y ~ x), col="blue", lwd=5)
}
abline(a = 4, b = -2, lwd=8, lty = 1, col="red")

# Ejemplo extendido de cálculo con R.
set.seed(2019); 
beta0 = 4; beta1 = -2; n = 30
x = runif(n) 
y = beta0 + beta1 * x + rnorm(n, mean = 0, sd = 1)
datos = data.frame(x, y)

# Diagrama de dispersión
modelo = lm(y ~ x, data = datos)
plot(x, y, col=alpha("blue", 0.8), pch=19)
abline(modelo, col="blue", lwd=5)

# summary para un modelo
(sumModelo = summary(modelo))

sumModelo$sigma
# sqrt(sum(modelo$residuals^2)/(modelo$df)) # comprobación

# Intervalo de confianza de los coeficientes del modelo
confint(modelo)
# Vamos a comprobar a mano el de beta_1
# valor crítico de la t de Student, df = n- 2
tc = qt(1 - 0.025, df = n - 2) 
# Busca el siguiente valor en la salida de summary(lm)
(seB1 = sumModelo$sigma / sqrt(sum((x - mean(x))^2))) 
# Y ahora el intervalo
(intervalo = coefficients(modelo)[2] + c(-1, 1) * tc * seB1)

# Y contraste sobre si beta1 = 0
sumModelo$coefficients

# Calculo "a mano" de ese contraste
(tValue = coefficients(modelo)[2] /  seB1)
(pValor = 2 * pt(abs(tValue), df = n - 2, lower.tail = FALSE))

# Intervalos de confianza y predicción para valores de $Y$.  
nuevoX = data.frame(x = 1/2)
predict(modelo, newdata = nuevoX, interval = "confidence")
predict(modelo, newdata = nuevoX, interval = "prediction")

######
# Bandas de confianza y predicción con R base

# Vamos a hacer intervalos para muchos valores de x
newXs = data.frame(x=seq(min(x), max(x), length.out=50))
# Calculamos los extremos de ambas bandas con predict
preBand = predict(modelo, int="prediction", newdata=newXs)
confBand = predict(modelo, int="confidence", newdata=newXs)
# Creamos el gráfico
plot(x, y, ylim= range(y, preBand,na.rm=T), pch=19, lwd=5, col="black")
# Estas son las funciones que dibujan las bandas
matlines(newXs$x, preBand[, -1], lty=c(2,2), col="red", lwd=5)
matlines(newXs$x, confBand[, -1], lty=c(4,4), col="darkgreen", lwd=5)
# Añadimos la recta de regresión
abline(modelo, lwd= 5, col= "blue")

######
# Bandas de confianza y predicción con ggplot
newXs = newXs  %>%  
  bind_cols(as_tibble(preBand))
ggplot(data = newXs) +
  geom_smooth(mapping = aes(x,  fit), method = "lm", se=FALSE)  +
  geom_ribbon(aes(x, ymin=lwr, ymax = upr), fill= "lightblue2") +
  geom_point(data = datos, mapping = aes(x, y)) +
  geom_smooth(data = datos, aes(x, y), 
              method=lm, se=TRUE, fill= "gold3")

# Gráficos para diagnóstico en modelos de regresión lineal simple.

 # residuos frente a valores predichos
plotModelo = plot(modelo, which = 1, pch=19, lwd= 12)
segments(x0 = c(2.3, 2.3), y0 = c(1, -1), x1 = c(3.7, 3.7), y1 = c(3, -2), 
         lty=3, lwd=12, col="blue")

# normalidad de los residuo
plotModelo = plot(modelo, which = 2, pch=19)
qqline(modelo$residuals, col="blue")   

# Gráfico scale-location. 
plotModelo = plot(modelo, which = 3, pch=19)

# Gráficos diagnosticos, datos con varianza no homogénea
par(mfrow = c(2, 2))
set.seed(2019)
n = 100
x = sort(signif(runif(n, min = 0, max = 1), digits=2) )  
y = 1 - (x/2) + rnorm(n, sd = 0.01*(1 + 50 * x))
plot(x, y)
abline(lm(y ~ x), col="red", lwd=2)
plot(lm(y ~ x), which = 1:3)
par(mfrow=c(1, 1))

# Análisis del brazo de palanca (*leverage*) con R.

# Ejemplo de Brian Caffo
par(mfrow = c(1, 2))
set.seed(2019)
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  
plot(lm(y ~ x), which = 5, pch=19)
par(mfrow = c(1, 1))

# Hatvalues.
set.seed(2019)
n <- 100
x <- c(10, rnorm(n))
y <- c(10, c(rnorm(n)))
modelo = lm(y ~ x)

head(hatvalues(modelo))

# Extendiendo el modelo lineal..

# Regresión simple (una v explicativa), más allá de las rectas..

set.seed(2017)
n = 100
x = sort(runif(n))
y1 = 3 + 2 * x + 0.3 * rnorm(n) # para que la muestra sea la misma que antes
y =  x * (1 - x) + 0.01 * rnorm(n)
plot(x, y, col="seagreen", pch=19, xlab="", ylab="")

modeloParabola = lm(y ~ poly(x, 2))

# El modelo ya no es una recta. Predict para dibujar el modelo.

plot(x, y, col="seagreen", pch=19, xlab="", ylab="")
xParabola = seq(min(x), max(x), length.out = 100)
yParabola = predict(modeloParabola, 
                      newdata = data.frame(x = xParabola ))
lines(xParabola, yParabola, lwd= 3, col= "red")
yP = predict(modeloParabola, newdata = data.frame(x = 0.7))
points(0.7, yP, pch = 19, cex=3.5, col="orange")

# Dibujo con ggplot.
library(tidyverse)
datos = data.frame(x, y)
ggplot(datos) + 
  geom_point(aes(x, y)) + 
  geom_smooth(aes(x, y), method="lm", formula = y ~ poly(x, 2))

# Ajuste de curvas exponenciales, logarítmicas, etc.
ggplot(mpg, aes(displ, cty)) + 
  geom_point() + 
  geom_smooth()

# Regresión multivariable.

childData = data.frame(
wgt = c(64, 71, 53, 67, 55, 58, 77, 57, 56, 51, 76, 68), 
hgt = c(57, 59, 49, 62, 51, 50, 55, 48, 42, 42, 61, 57), 
age = c(8, 10, 6, 11, 8, 7, 10, 9, 10, 6, 12, 9)) 
ggplot(childData, mapping = aes(age, wgt)) +
      geom_point() + 
      geom_smooth(method="lm")

# Modelo de regresión lineal con una de las variables.
modelo1 = lm(wgt ~ age, data = childData)
summary(modelo1)

# Modelo con las dos variables. 
modelo2 = lm(wgt ~ age + hgt, data = childData)
summary(modelo2)

# Comprobación con R en los datos del ejemplo. Identidad Anova.

modelo2$coefficients

modelo_yx2 =  lm(wgt ~ hgt, data = childData)
modelo_x1x2 =  lm(age ~ hgt, data = childData)

sum(residuals(modelo_yx2) * residuals(modelo_x1x2)) /     sum(residuals(modelo_x1x2)^2)

# Interpretación de los coeficientes.
nuevosDatos = data.frame(age = c(9, 9, 9), hgt = c(52, 53, 54))
(pesosPredichos = predict(modelo2, newdata = nuevosDatos))

diff(pesosPredichos)

# Intervalos de confianza en modelos con 2 variables
confint(modelo2)

# Selección de modelos. 
anova(modelo2)

# Cuidado con el orden en los términos del modelo
aov(modelo1)
(summ2 = summary(modelo2))
(aov2 = anova(modelo2))

modelo3 = lm(formula = wgt ~ hgt + age, data = childData)
(summ3 = summary(modelo3))
(aov3 = anova(modelo3))

sum(aov2$`Sum Sq`[1:2]) / sum(aov2$`Sum Sq`)
summ2$r.squared
summ3$r.squared

# Modelos lineales con factores. Anova.
ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Length, color=Species))

# El modelo lineal para Anova
modelo = lm(Sepal.Length ~ Species, iris)

# Representación gráfica de los coeficientes del modelo.

require(latex2exp)
boxplot(Sepal.Length ~ Species, data = iris, ylim = c(0, 8), lwd = 1)
medias = aggregate(Sepal.Length ~ Species, data = iris, FUN = mean)[,2]
arrows(x0 = 1:3, y0 = c(0, medias[1], medias[1]), 
       x1 = 1:3, y1 = medias, col= c("red", "darkgreen", "blue"), lwd= 5)
abline( h = medias[1], col="red", lty = 2, lwd = 4 )
text(x=1, y= medias[1]/2, label=TeX("$\\beta_0 = \\mu_1$"), col="red", cex = 2)
text(x=2, y= medias[1] - 0.5, label=TeX("$\\beta_1 = \\mu_2 - \\mu_1$"), col="darkgreen", cex = 2)
text(x=3.1, y= medias[1] - 0.5, label=TeX("$\\beta_2 = \\mu_3 - \\mu_1$"), col="blue", cex = 2)

# Estimando los coeficientes del modelo. 

(medias = aggregate(Sepal.Length ~ Species, iris, FUN = mean)[,2])

c(medias[1], medias[2] - medias[1], medias[3] - medias[1])

# comparamos con los coeficientes del modelo con lm
modelo = lm(Sepal.Length ~ Species, iris)
(coefs = modelo$coefficients)

# summary para el modelo
(sumModelo = summary(modelo))

# Contraste de igualdad de medias en dos grupos usando $t$
# Caso de homogeneidad de varianzas

# Fabricamos dos muestras. Hacemos "trampa" porque sabemos las medias 
set.seed(2020)
muestra1 = rnorm(30, mean = 2, sd = 0.4)
muestra2 = rnorm(30, mean = 2.5, sd = 0.4)
# Fíjate en que las varianzas son iguales. Ahora hacemos el contraste
t.test(muestra1, muestra2, alternative = "two.sided", var.equal = TRUE)

# Anova vs t.test 

# Creamos el factor
tipo = gl(n = 2, k = 30)
datos = data.frame(x = c(muestra1, muestra2), tipo)

# Ajustamos el modelo
modelo = lm(x ~ tipo, data = datos)
summary(modelo)

# Otros tipos de comparaciones entre dos grupos más allá de Anova.

# Caso de varianzas distintas

# Fabricamos las dos muestras. Fíjate en las varianzas
muestra1 = rnorm(30, mean = 2, sd = 0.1)
muestra2 = rnorm(30, mean = 2.5, sd = 0.6)
# Hagamos un contraste unilateral, con varianzas distintas
t.test(muestra1, muestra2, alternative = "less", var.equal = FALSE)

# Muestras emparejadas

# Fabricamos las dos muestras. Fíjate en las varianzas
antes = rnorm(25, mean = 36.5, sd = 0.6)
despues = rnorm(25, mean = 37, sd = 0.8)
# Hagamos un contraste unilateral, con varianzas distintas
t.test(antes, despues, alternative = "less", paired = TRUE)

# Contraste de igualdad de varianzas con var.test`

# Fabricamos las dos muestras (con varianzas dis
muestra1 = rnorm(30, mean = 2, sd = 0.1)
muestra2 = rnorm(30, mean = 2.5, sd = 0.6)
# Y contrastamos la igualdad de esas varianzas
var.test(muestra1, muestra2)

# Modelos lineales generalizados (glm). Regresión Logística. 

# Descripción del problema.
CHDdata = read.table("./datos/CHDAGE.txt", header = TRUE)
head(CHDdata)

summary(CHDdata)

# Diagrama de dispersión para regresión logística
ggplot(CHDdata) + 
  geom_point(aes(x = AGE, y = CHD, size=2), 
             show.legend=FALSE, position = position_jitter(w = 0, h = 0.02)) +
  geom_hline(yintercept = 0:1, linetype = "dashed", color = "blue", size = 2)

# Construcción del modelo.. Franjas de edad
(AGEbreaks = c(20, seq(from = 30, to = 60, by = 5), 70))

CHDdata$AgeGroup = cut(CHDdata$AGE, breaks = AGEbreaks, right = FALSE)

# tabla de contingencia de CHD frente al grupo de edad
options(width = 80)
(tabla1 = as.matrix(table(CHDdata$CHD, CHDdata$AgeGroup)))

(sumaColumnas = colSums(tabla1))

(probs = signif(tabla1[2, ] / sumaColumnas, 2))

# ...para pensar en términos de probabilidades. 
(probs = signif(tabla1[2, ] / sumaColumnas, 2))

# Representación gráfica de las probabilidades por franja de edad

midpoints = AGEbreaks[-length(AGEbreaks)] + c(5, rep(2.5, 6), 5)
probsdf = data.frame(midpoints, probs)
ggplot(CHDdata) + 
  geom_point(aes(x = AGE, y = CHD, size=4), 
             show.legend=FALSE, 
             position = position_jitter(w = 0, h = 0.02)) +
  geom_hline(yintercept = 0:1, 
             linetype = "dashed", 
             color="blue", 
             size=2) + 
  geom_point(data = probsdf, 
             mapping = aes(x = midpoints, 
                           y = probs, 
                           size=4, col="red"),
             show.legend=FALSE)

# El modelo es una curva con forma de s.

glmCHD = glm(CHD ~ AGE, family = binomial(link = "logit"), CHDdata)
summGlmCHD = summary(glmCHD)
curvaX = data.frame(AGE = seq(20, 70, length.out = 101))
curvaY = predict(glmCHD, newdata = curvaX, type = "response")
curvaDf = data.frame(AGE = curvaX$AGE, CHD = curvaY)
ggplot(CHDdata) + 
geom_point(aes(x = AGE, y = CHD, size=4), 
           show.legend=FALSE, position = position_jitter(w = 0, h = 0.02)) +
  geom_hline(yintercept = 0:1, linetype = "dashed", color="blue", size=2) + 
  geom_line(data = curvaDf, aes(x = AGE, y = CHD, col= "red", size=3), 
            show.legend=FALSE)

# Ajustando un modelo logístico con R. Función glm.
(glmCHD = glm(CHD ~ AGE, family = binomial, data = CHDdata))

coefficients(glmCHD)

# Usando el modelo logístico para predecir.
edadPredecir = data.frame(AGE = 32)
(probCHD = predict(glmCHD, newdata = edadPredecir, type = 'response'))

# Predicción en escala de log-odds
predict(glmCHD, newdata = edadPredecir, type = 'link')
coefficients(glmCHD)[1] + coefficients(glmCHD)[2] * 32

# Interpretación de coeficientes en regresión logística. 

edades = data.frame(AGE = 50:55)
probabilidades = predict(glmCHD, newdata = edades, type = 'response')
diff(probabilidades)
logOdds = predict(glmCHD, newdata = edades, type = 'link')
diff(logOdds)
coefficients(glmCHD)[2]

########################################################################
########################################################################
########################################################################
########################################################################

#########################################################
## Complementos de R
## 
#########################################################

# Strings y regex con `stringr`

# Número de caracteres de un string
palabras = c("Wahrheit", "ist", "Feuer", "und", 
             "Wahrheit", "reden", "heißt", "leuchten", "und",
             "brennen")
str_count(palabras)

# str_c

str_c(palabras, collapse = " ")
str_c("20",1:9, sep = "0")

# Expresiones regulares

library(lubridate)
fechaTexto = "01-01-2020"
fecha = dmy(fechaTexto)
meses = fecha + months(0:11)

# Localizando las aparciones de patrones con regex

meses = "enero, febrero, marzo, abril, mayo, junio, julio,     agosto, septiembre, octubre, noviembre, diciembre"
patron = "bre"
require(htmlwidgets)
str_view_all(meses, patron) # requiere instalar htmlwidgets

patron2 = "[aeiou]bre"
str_view_all(meses, patron2)

patron3 = "[b-df-hj-np-tv-z]{2}"
str_view_all(meses, patron3)

# Usando las expresiones regulares con funciones de stringr

patron3 = "[b-df-hj-np-tv-z]{2}"
str_replace_all(meses, patron3, "##")

patron4 = "(\\w)(\\w)"
# str_view_all(meses, patron4) # Ejecuta para ver como funciona
str_replace_all(meses, patron4, "\\2\\1")

words %>%  str_detect(pattern = "(s){2}") %>%  words[.]

# Fechas en R. Lubridate.

library(lubridate)
fechaTexto = "21-07-1969 02:56UTC"
(fecha = dmy_hm(fechaTexto))

# Acceso a las componentes de una fecha y operaciones con fechas. 
day(fecha)
month(fecha)
hour(fecha)

fecha + days(100)

fecha + hours(1000)  + minutes(42)

fecha2 = dmy_hm("29-10-1969 00:00")
fecha2 - fecha
