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


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "70%"-----
include_graphics("../fig/01-fig00.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "50%"-----
include_graphics("../fig/Data_Science_VD.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "80%"-----
include_graphics("../fig/DataScienceProgLanguageSurvey2017.png")


## ----echo=FALSE, eval= FALSE, comment=NULL, fig.align='center', out.width = "80%"----
# Un ejemplo de https://plot.ly/r/choropleth-maps/

library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total.exports, text = ~hover, locations = ~code,
    color = ~total.exports, colors = 'Purples'
  ) %>%
  colorbar(title = "Millions USD") %>%
  layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "55%"-----
include_graphics("../fig/01-fig01.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "55%"-----
include_graphics("../fig/01-fig02.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "55%"-----
include_graphics("../fig/Tut02-01.png")


## ----eval=FALSE----------------------------------------------------------
## 2 + 3
## 15 - 7
## 4 * 6
## 13/5
## 1/3 + 1/5
## sqrt(25)
## sqrt(26)
## sin(pi)
## sin(3.14)


## ----eval=FALSE----------------------------------------------------------
## 1.224606e-16


## ----eval=FALSE----------------------------------------------------------
## 3 +


## ----eval=FALSE----------------------------------------------------------
## 4/*3
## 2/0
## sqrt(-4)


## ----eval=FALSE----------------------------------------------------------
## Sqrt(4)


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "45%"-----
include_graphics("../fig/Tut02-01.png")


## ----eval=FALSE----------------------------------------------------------
## a = 2


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "30%"-----
include_graphics("../fig/01-fig09-RStudio-EntornoValoresVariables.png")


## ----eval=FALSE----------------------------------------------------------
## a + 1


## ----results='hide'------------------------------------------------------
a = 2
b = 3
c = a + b
a = b * c
b = (c - a)^2
c = a * b


## ----comment=NULL--------------------------------------------------------
(c = a + b)


## ----eval=FALSE----------------------------------------------------------
## a = 2


## ----eval=FALSE----------------------------------------------------------
## a <- 2


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "25%"-----
include_graphics("../fig/01-fig10-RStudio-EntornoLimpieza.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "25%"-----
include_graphics("../fig/01-fig11-RStudio-EntornoLimpieza.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "55%"-----
include_graphics("../fig/01-InterfazRstudio-Editor.png")


## ----results='hide'------------------------------------------------------
a = 4


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "35%"-----
include_graphics("../fig/01-fig12-RStudio-Editor.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "35%"-----
include_graphics("../fig/01-fig13-RStudio-EditorVariosComandos.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "35%"-----
include_graphics("../fig/01-fig14-RStudio-EditorVariosComandos.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "30%"-----
include_graphics("../fig/01-fig15-RStudio-ElegirWorkDir.png")


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "60%"-----
include_graphics("../fig/01-fig17-RStudio-PrimerScriptAbierto.png")


## ------------------------------------------------------------------------
head(iris)


## ----eval=FALSE----------------------------------------------------------
## View(iris)


## ------------------------------------------------------------------------
head(iris)


## ------------------------------------------------------------------------
dim(iris)


## ------------------------------------------------------------------------
iris[2, 3]


## ------------------------------------------------------------------------
iris[2, 3] = 7
head(iris)


## ----eval=FALSE----------------------------------------------------------
## iris[ , 3]

## ----echo=FALSE----------------------------------------------------------
head(iris[ , 3], 38)


## ----eval=FALSE----------------------------------------------------------
## iris$Petal.Length

## ----echo=FALSE----------------------------------------------------------
head(iris$Petal.Length, 38)


## ------------------------------------------------------------------------
iris[2, ]


## ------------------------------------------------------------------------
iris[49:52, c(1, 3, 5)]


## ----eval=FALSE----------------------------------------------------------
## iris[iris$Sepal.Width > 2, ]

## ----echo=FALSE----------------------------------------------------------
head(iris[iris$Sepal.Width > 2, ])


## ----echo=FALSE, comment=NULL, fig.align='center', out.width = "45%"-----
include_graphics("../fig/01-fig16-RStudio-InstalarLibreria.png")


## ----message=FALSE-------------------------------------------------------
library(tidyverse)


## ----echo=FALSE, eval = FALSE--------------------------------------------
## library(nycflights13)
## View(flights)


## ------------------------------------------------------------------------
library(nycflights13)


## ----echo=FALSE, eval = FALSE--------------------------------------------
## library(nycflights13)
## str(flights)
## library(tidyverse)
## ggplot(data = flights) +
##   geom_boxplot(mapping = aes(origin, distance, color = origin))


## ----message=FALSE, size="scriptsize"------------------------------------
iris %>%
  select(c('Petal.Length', 'Petal.Width'))  %>%
  filter(Petal.Width > 2.3) 


## ----eval=FALSE----------------------------------------------------------
## library(gapminder)
## View(gapminder)


## ----eval=FALSE----------------------------------------------------------
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))


## ----echo=-1, comment=NULL, fig.align='center', out.width = "65%", , size="scriptsize"----
library(gapminder)
gapminder %>% 
  filter(year == 2007) %>% # Hasta aquí dplyr, ahora entra en acción ggplot
ggplot() +
  geom_point(mapping = aes(x = lifeExp, log(gdpPercap, 10), 
                           color = continent, size = pop)) +
  scale_size(range = c(.1, 24), name="Population (M)")

