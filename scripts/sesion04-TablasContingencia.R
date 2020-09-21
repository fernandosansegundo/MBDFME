########################################################
# www.postdata-statistics.com
# POSTDATA. Introducción a la Estadística
# Capitulo 3.  
# Ejemplos clase. 
########################################################

rm(list=ls())

(tablaContingencia = matrix(c(192, 4, 158, 9646), nrow=2))

colnames(tablaContingencia) = c("ENF","SANO" )
rownames(tablaContingencia) = c("Prueba+", "Prueba-" )
tablaContingencia


sum(tablaContingencia)

# A?adimos m?rgenes
(tablaContingenciaAmpliada = addmargins(tablaContingencia))

# Intersecciones
(tablaContingenciaRel = tablaContingenciaAmpliada / sum(tablaContingencia))

# Dividimos cada fila por su suma (usando prop.table):
(tablaMarginalFilas = prop.table(tablaContingencia, margin = 1))
addmargins(tablaMarginalFilas) # Condicionadas por prueba+ / prueba-


# Dividimos cada columna por su suma:
(tablaMarginalColumnas = prop.table(tablaContingencia, margin = 2))
addmargins(tablaMarginalColumnas)  # Condicionadas por sano / enfermo



