## ------------------------------------------------------------------------
muestra = sample(0:3, size = 10, replace = TRUE, prob = c(64, 48, 12, 1))


## ----echo=FALSE, eval=FALSE, comment=NULL--------------------------------
library(viridisLite)
muestra = sample(0:3, size = 1000, replace = TRUE, prob = c(64, 48, 12, 1))
barplot(table(muestra), col=viridis(4))




## ----message=FALSE, echo=FALSE-------------------------------------------
library(tidyverse)
fhs = read_csv("../datos/framingham.csv")
tablaHyp = prop.table(table(fhs$prevalentHyp))
p = unname(tablaHyp[2])


## ----echo=FALSE, results='hide'------------------------------------------
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


## ------------------------------------------------------------------------
dbinom(x = 3, size = 7, prob = p)


## ------------------------------------------------------------------------
signif(dbinom(x = 0:7, size = 7, prob = p), digits = 3)


## ------------------------------------------------------------------------
signif(pbinom(q = 0:7, size = 7, prob = p), digits = 3)


## ------------------------------------------------------------------------
rbinom(n = 25, size = 7, prob = p)


## ----message=FALSE, fig.align='center', out.width = "50%", echo = -3-----
probabilidades = dbinom(x = 0:7, size = 7, prob = p)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:7)
arrows(seq(0.5, 7.5, by = 1), 0, seq(0.5, 7.5, by = 1), prop.table(table(X)), col="red", lwd = 2)


## ----message=FALSE, fig.align='center', out.width = "35%", echo = FALSE----
probabilidades = dbinom(x = 0:12, size = 10, prob = 2/3)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:12)


## ----message=FALSE, fig.align='center', out.width = "60%", echo = FALSE----
probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)


## ----probBinomial, message=FALSE, fig.align='center', out.width = "40%", echo = FALSE----
probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)
par(new = T)
probabilidades[0:25] = 0
probabilidades[36:100] = 0
barplot(probabilidades/4, space = 0, col="blue", names.arg = 0:100)


## ----message=FALSE, fig.align='center', out.width = "40%", echo = FALSE----
probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)
par(new = T)
probabilidades[0:25] = 0
probabilidades[36:100] = 0
barplot(probabilidades/4, space = 0, col="blue", names.arg = 0:100)








## ------------------------------------------------------------------------
pnorm(10.5, mean=10, sd=2)


## ----echo = TRUE, results='hide', purl=TRUE------------------------------
  1 - pnorm(11, mean=10, sd=2)
  pnorm(11, mean = 10, sd = 2, lower.tail = FALSE)




## ------------------------------------------------------------------------
pnorm(12, mean=10, sd=2) - pnorm(7, mean=10, sd=2)



## ----purl=-2-------------------------------------------------------------
qnorm(p = 1/3, mean = 10, sd=2)





## ----echo=-(1:3), eval=FALSE, comment=NULL-------------------------------
set.seed(2019)
x1 = rnorm(1000)
y1 = rnorm(1000)
ggplot(data.frame(x1, y1)) +
  geom_point(mapping = aes(x1, y1), col="red")

## ----echo=FALSE, eval=FALSE, comment=NULL--------------------------------
x2 = runif(1000, min = -1, max = 1)
y2 = runif(1000, min = -1, max = 1)
ggplot(data.frame(x2, y2)) +
  geom_point(mapping = aes(x2, y2), col="blue")




## ----echo = TRUE, fig.align='center', out.width = "30%"------------------
set.seed(2019)
pob1 = rnorm(30000, mean = -3, sd = 1)
pob2 = rnorm(30000, mean = 2, sd = 0.5)
pobSuma = 3 * pob1 + 4 * pob2
plot(density(pobSuma, adjust = 1.6), main="", lwd=5, col="red", xlab="")


## ------------------------------------------------------------------------
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


## ------------------------------------------------------------------------
genPasswd(size = 15)


## ----eval=FALSE, comment = NULL------------------------------------------
formals(genPasswd)


## ------------------------------------------------------------------------
body(genPasswd)


## ----eval=FALSE, comment=NULL--------------------------------------------
body(genPasswd) = "No me apetece trabajar..."
genPasswd(12)


## ------------------------------------------------------------------------
x = c(2, 3, -5, NA, 4, 6, NA)
is.na(x)


## ------------------------------------------------------------------------
any(is.na(fhs$glucose))


## ------------------------------------------------------------------------
head(complete.cases(fhs), 17)


## ------------------------------------------------------------------------
mean(fhs$glucose)


## ------------------------------------------------------------------------
mean(fhs$glucose, na.rm = TRUE)

