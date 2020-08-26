# Ejercicio 1  
2 + 3
15 - 7
4 * 6
13/5
1/3 + 1/5
sqrt(25)
sqrt(26)
sin(pi)
sin(3.14)

# Ejercicio 2
3 + 

4/*3
2/0
sqrt(-4)

Sqrt(4)

# Variables y asignaciones
a = 2

a + 1

# Ejercicio 3
a = 2
b = 3
c = a + b
a = b * c
b = (c - a)^2
c = a * b

(c = a + b)

a = 2

a <- 2

# Tablas (Data Frames)
head(iris)

dim(iris)

# Selección de elementos de una tabla
iris[2, 3]

iris[2, 3] = 7
head(iris)

iris[ , 3]
head(iris[ , 3], 38)

iris$Petal.Length
head(iris$Petal.Length, 38)

iris[2, ]

iris[49:52, c(1, 3, 5)]

iris[iris$Sepal.Width > 2, ]
head(iris[iris$Sepal.Width > 2, ])

# Instalación y carga de librerías
library(tidyverse)

library(nycflights13)

# Un primer encuentro con dplyr
iris %>%
  select(c('Petal.Length', 'Petal.Width'))  %>%
  filter(Petal.Width > 2.3) 

# Y un primer encuentro con ggplot
library(gapminder)
gapminder %>% 
  filter(year == 2007) %>% # Hasta aquí dplyr, ahora entra en acción ggplot
ggplot() +
  geom_point(mapping = aes(x = lifeExp, log(gdpPercap, 10), 
                           color = continent, size = pop)) +
  scale_size(range = c(.1, 24), name="Population (M)")
