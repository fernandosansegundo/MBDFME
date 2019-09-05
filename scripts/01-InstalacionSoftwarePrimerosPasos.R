## ----eval = FALSE, comment = NULL----------------------------------------
2 + 3
15 - 7
4 * 6
13/5
1/3 + 1/5
sqrt(25)
sqrt(26)
sin(pi)
sin(3.14)


## ----eval = FALSE, comment = NULL,, comment = NULL-----------------------
1.224606e-16


## ----eval = FALSE, comment = NULL,, comment = NULL-----------------------
3 + 


## ----eval = FALSE, comment = NULL,, comment = NULL-----------------------
4/*3
2/0
sqrt(-4)


## ----eval = FALSE, comment = NULL,, comment = NULL-----------------------
Sqrt(4)




## ----eval = FALSE, comment = NULL----------------------------------------
a = 2




## ----eval = FALSE, comment = NULL----------------------------------------
a + 1


## ----results='hide'------------------------------------------------------
a = 2
b = 3
c = a + b
a = b * c
b = (c - a)^2
c = a * b


## ----comment=NULL--------------------------------------------------------
(c = a + b)


## ----eval = FALSE, comment = NULL----------------------------------------
a = 2


## ----eval = FALSE, comment = NULL----------------------------------------
a <- 2








## ----results='hide'------------------------------------------------------
a = 4












## ------------------------------------------------------------------------
head(iris)


## ----eval = FALSE, comment = NULL----------------------------------------
View(iris)


## ------------------------------------------------------------------------
head(iris)


## ------------------------------------------------------------------------
dim(iris)


## ------------------------------------------------------------------------
iris[2, 3]


## ------------------------------------------------------------------------
iris[2, 3] = 7
head(iris)


## ----eval = FALSE, comment = NULL----------------------------------------
iris[ , 3]

## ----echo=FALSE----------------------------------------------------------
head(iris[ , 3], 38)


## ----eval = FALSE, comment = NULL----------------------------------------
iris$Petal.Length

## ----echo=FALSE----------------------------------------------------------
head(iris$Petal.Length, 38)


## ------------------------------------------------------------------------
iris[2, ]


## ------------------------------------------------------------------------
iris[49:52, c(1, 3, 5)]


## ----eval = FALSE, comment = NULL----------------------------------------
iris[iris$Sepal.Width > 2, ]

## ----echo=FALSE----------------------------------------------------------
head(iris[iris$Sepal.Width > 2, ])




## ----message=FALSE-------------------------------------------------------
library(tidyverse)


## ----echo=FALSE, eval = FALSE, comment = NULL----------------------------
library(nycflights13)
View(flights)


## ------------------------------------------------------------------------
library(nycflights13)


## ----echo=FALSE, eval = FALSE, comment = NULL----------------------------
library(nycflights13)
str(flights)
library(tidyverse)
ggplot(data = flights) + 
  geom_boxplot(mapping = aes(origin, distance, color = origin)) 


## ----message=FALSE, size="scriptsize"------------------------------------
iris %>%
  select(c('Petal.Length', 'Petal.Width'))  %>%
  filter(Petal.Width > 2.3) 


## ----eval = FALSE, comment = NULL----------------------------------------
library(gapminder)
View(gapminder)




## ----echo=-1, comment=NULL, fig.align='center', out.width = "65%", size="scriptsize"----
library(gapminder)
gapminder %>% 
  filter(year == 2007) %>% # Hasta aquí dplyr, ahora entra en acción ggplot
ggplot() +
  geom_point(mapping = aes(x = lifeExp, log(gdpPercap, 10), 
                           color = continent, size = pop)) +
  scale_size(range = c(.1, 24), name="Population (M)")

