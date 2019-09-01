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


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "60%"-----
include_graphics("../fig/02-fig00-XKCDfileExtensions.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "80%"-----
include_graphics("../fig/02-fig01-FicherosCsv.png")
# clase = read.table("../datos/sesion02-ejemploCsv.csv", header = TRUE, sep=",")


## ----size="small"--------------------------------------------------------
movies = read.csv(file = "../datos/movies.csv", header = TRUE)


## ----size="tiny"---------------------------------------------------------
movies[7, ]


## ----size="scriptsize"---------------------------------------------------
tail(movies$Year, 20) # se muestran las 20 últimas


## ----eval=FALSE----------------------------------------------------------
## movies$Genre[movies$Year == 2010]


## ----size="scriptsize", echo=-1, message=FALSE---------------------------
library(tidyverse)
movies %>% 
  filter(Year == 2010) %>% 
  select(Genre) %>% 
  .[1:20, ]     # ¿Qué hace esta última operación?


## ----size="small", message=FALSE, warning=FALSE--------------------------
library(tidyverse)
movies2 = read_csv("../datos/movies.csv")


## ----echo=-1, results='hold', size="small"-------------------------------
set.seed(2019)
datos = 
  data.frame(A = sample(1:100, 10), B = sample(LETTERS, 10), C = rnorm(10))
head(datos, 2)
write_csv(datos, path = "../datos/sesion02-guardarCsv.csv")


## ------------------------------------------------------------------------
library(readxl)
accidentes = read_excel("../datos/train_acc_2010.xls")


## ------------------------------------------------------------------------
library(haven)


## ----size="scriptsize", echo=-1------------------------------------------
options(width=190)
library(haven)
planetas = read_spss("../datos/CH10_Planet_distances_and_y.SAV")
head(planetas, 3) # Veamos las tres primeras filas.


## ----size="scriptsize", echo=-1------------------------------------------
options(width=150)
transport = read_sas("../datos/transport.sas7bdat")
head(transport, 3) 


## ----size="tiny"---------------------------------------------------------
auto2 = read_dta("../datos/auto2.dta")
head(auto2, 3) 


## ----size="small"--------------------------------------------------------
save("accidentes", "planetas", file = "../datos/accidentes_planetas.RData")


## ------------------------------------------------------------------------
rm(planetas)


## ----size="scriptsize"---------------------------------------------------
load(file = "../datos/accidentes_planetas.RData")
head(planetas, 3)


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "80%"----
library(tidyverse)
include_graphics("../fig/02-fig02-fhs.png")
fhs = read_csv("../datos/framingham.csv")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "20%"-----
# include_graphics("../fig/02-fig03-DiscretoContinuo.png")


## ----comment=NULL, echo=-1, size="small"---------------------------------
options(width = 100)
table(mpg$cty)


## ----eval=FALSE----------------------------------------------------------
## mpg %>%
##   count(cty)


## ----echo = -1, size="small"---------------------------------------------
options(width = 70)
signif(prop.table(table(mpg$cty)), 2)


## ----eval=FALSE, size="small"--------------------------------------------
##     mpg %>%
##       count(cty) %>%
##         mutate(cty, freq = n / sum(n), n=NULL) # NULL aquí es como un select


## ------------------------------------------------------------------------
sum(prop.table(table(mpg$cty)))


## ----echo=-1-------------------------------------------------------------
options(width = 60)
cumsum(table(mpg$cty))


## ----eval=FALSE----------------------------------------------------------
## table(fhs$totChol)


## ----size = "scriptsize"-------------------------------------------------
cholLevels = cut(fhs$totChol, breaks = 10)
head(cholLevels)


## ----size="small"--------------------------------------------------------
table(cholLevels)


## ----echo=TRUE, message=FALSE, fig.align='center', out.width = "40%"-----
cortes = seq(min(mpg$cty), max(mpg$cty), length.out = 11)
hist(mpg$cty, breaks = cortes, col="orange", main="")


## ----echo=TRUE, message=FALSE, fig.align='center', out.width = "40%"-----
ggplot(data = mpg) + 
  geom_histogram(mapping = aes(cty), breaks = cortes, 
                 fill = "orange", color="black")


## ----echo=TRUE, message=FALSE, fig.align='center', out.width = "50%"-----
plot(density(mpg$cty), col="red", main="", lwd = 3)


## ----echo=FALSE, eval=FALSE, message=FALSE, fig.align='center', out.width = "50%"----
## plot(density(auto2$rep78, na.rm = TRUE), col="red", main="", lwd = 3)


## ----message=FALSE, fig.align='center', out.width = "50%", size="small"----
ggplot(mpg) + 
  geom_density(mapping = aes(cty), color="red", fill="lightblue", size=1.5)


## ----echo=TRUE, message=FALSE, fig.align='center', out.width = "50%"-----
hist(x = fhs$sysBP, breaks=150, probability = TRUE, main="")
lines(density(fhs$sysBP), col="red", lwd=4)


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "70%"----
library(gridExtra)
library(tidyverse)

p1 = ggplot(auto2) + 
  geom_density(mapping = aes(price), color="red", fill="lightblue", size=1.5)

p2 = ggplot(auto2) + 
  geom_density(mapping = aes(trunk), color="red", fill="lightblue", size=1.5)

p3 = ggplot(auto2) + 
  geom_density(mapping = aes(weight), color="red", fill="lightblue", size=1.5)

p4 = ggplot(auto2) + 
  geom_density(mapping = aes(gear_ratio), color="red", fill="lightblue", size=1.5)

grid.arrange(p1, p2, p3, p4, nrow = 2)


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "40%"----
curve(dchisq(x, df=6), from = 0, to = 16, ylab="density", col="red", lwd=4, main="")


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "40%"----
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



## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "80%"----
par(mfrow = c(3, 1))
curve(dchisq(x, df=6), from = 0, to = 16, 
      ylab="density", col="blue", lwd=4, main="Asimétrica a derecha", cex.main=2)  
curve(dnorm, from = -3, to = 3, 
      ylab="density", col="black", lwd=4, main="Simétrica", cex.main=2)  
curve(dchisq(15-x, df=6), from = 0, to = 16, 
      ylab="density", col="red", lwd=4, main="Asimétrica a izquierda", cex.main=2)  
par(mfrow = c(1, 1))


## ----echo=FALSE, message=FALSE, fig.align='center', out.width = "90%"----
include_graphics("../fig/02-fig04-ProbabilidadIntervaloVariableDiscreta.png")


## ----echo = -1, size="scriptsize"----------------------------------------
set.seed(2019)
(muestra = sample(0:100, size = 20, replace = TRUE))
(media = mean(muestra))


## ----echo = -1-----------------------------------------------------------
set.seed(2019)
muestra = sample(0:100, size = 99, replace = TRUE) 
(media = mean(muestra))


## ------------------------------------------------------------------------
muestra2 = c(muestra, 1000)
(media2 = mean(muestra2))


## ----echo  =-1-----------------------------------------------------------
set.seed(2019)
(valores = sample(1:100, 17, replace = TRUE))


## ----results='hold'------------------------------------------------------
(ordenados = sort(valores))
(mediana = ordenados[9])


## ------------------------------------------------------------------------
median(valores)


## ------------------------------------------------------------------------
median(muestra)
median(muestra2)


## ----size="small"--------------------------------------------------------
quantile(mpg$cty, probs = 0.43)


## ----size="small"--------------------------------------------------------
summary(mpg$cty)


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width = "70%"----
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


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width = "70%"----
plot(density(muestra1, adjust = 2), col="blue", 
     xlim = c(-12, 12), lwd = 2, main = "", xlab= "")
lines(density(muestra2, adjust = 2), col="red", lwd = 2, sub="")


## ------------------------------------------------------------------------
IQR(mpg$cty)


## ----size="small"--------------------------------------------------------
summary(mpg$cty)


## ----size="small"--------------------------------------------------------
unname(quantile(mpg$cty, probs = c(1/4, 3/4)) + c(-1, 1) * 1.5 * IQR(mpg$cty))


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "60%"-----
include_graphics("../fig/02-fig05-BoxPlotEstructura.png")


## ----echo=TRUE, comment=NULL, fig.align='center', out.width = "50%", size="small"----
bxp_cty = boxplot(mpg$cty, col="orange")


## ----size="small"--------------------------------------------------------
bxp_cty$out


## ----echo=TRUE, comment=NULL, fig.align='center', out.width = "60%", size="small"----
ggplot(mpg) + 
  geom_boxplot(mapping = aes(y = cty), fill="orange") +
  scale_x_discrete(breaks = c())


## ----echo=TRUE, comment=NULL, fig.align='center', out.width = "40%", size="tiny"----
ggplot(mpg) + 
  geom_violin(mapping = aes(x=0, y = cty)) +
  scale_x_discrete(breaks = c()) +
  geom_boxplot(mapping = aes(y = cty), fill="green") +
  geom_jitter(aes(x=0, y = cty), 
              position = position_jitter(w=0.05, h= 0), col="blue")


## ----size="small"--------------------------------------------------------
mad(accidentes$Speed, constant = 1)


## ------------------------------------------------------------------------
var(mpg$displ)


## ------------------------------------------------------------------------
n = length(mpg$displ)
media = mean(mpg$displ)
sum((mpg$displ - media)^2) / (n - 1 )


## ------------------------------------------------------------------------
sd(mpg$displ)


## ------------------------------------------------------------------------
sqrt(var(mpg$displ))


## ----size="small"--------------------------------------------------------
table(accidentes$TrkType)
prop.table(table(accidentes$TrkType))


## ----echo=TRUE, message=FALSE, fig.align='center', out.width = "30%", size="scriptsize"----
library(viridisLite)
barplot(prop.table(table(accidentes$TrkType)), col=viridis(5))
ggplot(accidentes) + 
  geom_bar(mapping = aes(x = TrkType), fill= viridis(5))


## ------------------------------------------------------------------------
mean(fhs$male)

