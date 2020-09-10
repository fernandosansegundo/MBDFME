## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 2: Tipos de Variables y Análisis Exploratorio"



# Ficheros csv con R, Lectura ficheros movies
movies = read.csv(file = "../datos/movies.csv", header = TRUE)

# Repaso de operaciones con data.frames.
movies[7, ]

tail(movies$Year, 20) # se muestran las 20 últimas

movies$Genre[movies$Year == 2010]

# Seleccionar usando el tidyverse (dplyr)
library(tidyverse)
movies %>% 
  filter(Year == 2010) %>% 
  select(Genre) %>% 
  .[1:20, ]     # ¿Qué hace esta última operación?

# Usando readr para leer y escribir ficheros csv.
movies2 = read_csv("../datos/movies.csv")

# crear ficheros csv a partir de una tabla
set.seed(2019)
datos = 
  data.frame(A = sample(1:100, 10), B = sample(LETTERS, 10), C = rnorm(10))
head(datos, 2)
write_csv(datos, path = "../datos/sesion02-guardarCsv.csv")

# Ficheros Excel
library(readxl)
accidentes = read_excel("../datos/train_acc_2010.xls")

# Ficheros de otros programas estadísticos.
library(haven)

options(width=190)
library(haven)
planetas = read_spss("../datos/CH10_Planet_distances_and_y.SAV")
head(planetas, 3) # Veamos las tres primeras filas.

options(width=150)
transport = read_sas("../datos/transport.sas7bdat")
head(transport, 3) 

auto2 = read_dta("../datos/auto2.dta")
head(auto2, 3) 

# Ficheros RData.
save("accidentes", "planetas", file = "../datos/accidentes_planetas.RData")

rm(planetas)

load(file = "../datos/accidentes_planetas.RData")
head(planetas, 3)




# Datos de enfermedades coronarias en Framingham
fhs = read_csv("../datos/framingham.csv")

# variable discreta, tabla de frecuencia absoluta
options(width = 100)
table(mpg$cty)

mpg %>%
  count(cty)

# y tabla de frecuencia relativa
options(width = 70)
signif(prop.table(table(mpg$cty)), 2)

      mpg %>% 
        count(cty) %>%
          mutate(cty, relFreq = prop.table(n), n=NULL) 
          # NULL aquí elimina la columna n

# suma frecuencias relativas
sum(prop.table(table(mpg$cty)))

# Frecuencias acumuladas.
options(width = 60)
cumsum(table(mpg$cty))

# Error:  tabla de frecuencia de una v. continua 
table(fhs$totChol)

# Creando intervalos con cut
cholLevels = cut(fhs$totChol, breaks = 10)
head(cholLevels)

table(cholLevels)

# Histograma con R basico (seleccionando los cortes)
cortes = seq(min(mpg$cty), max(mpg$cty), length.out = 11)
hist(mpg$cty, breaks = cortes, col="orange", main="")

# Histograma con ggplot2 (los mismos cortes)
ggplot(data = mpg) + 
  geom_histogram(mapping = aes(cty), breaks = cortes, 
                 fill = "orange", color="black")

# Curvas de densidad, R básico
plot(density(mpg$cty), col="red", main="", lwd = 3)



# Curvas de densidad con ggplot
ggplot(mpg) + 
  geom_density(mapping = aes(cty), color="red", fill="lightblue", size=1.5)

# Relación entre curvas de densidad e histogramas.
hist(x = fhs$sysBP, breaks=150, probability = TRUE, main="")
lines(density(fhs$sysBP), col="red", lwd=4)





