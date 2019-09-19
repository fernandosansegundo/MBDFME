#############################################################
# Fundamentos Matemáticos de la Estadística. Curso 2019-2020.
# sesion03-02
#############################################################

##########################
# Tablas de contingencia
##########################

# Vamos a empezar creando una tabla como matriz aunque normalmente
# el punto de partida será el resultado de table.

(tablaContingencia = matrix(c(192, 4, 158, 9646), nrow=2))

# Ponemos nombres a las filas y columnas
colnames(tablaContingencia) = c("ENF","SANO" )
rownames(tablaContingencia) = c("Prueba+", "Prueba-" )
tablaContingencia

# Vamos a guardar el total en una variable

(totalTabla = sum(tablaContingencia))

# Añadimos márgenes
(tablaContingenciaAmpliada = addmargins(tablaContingencia))

# Dividiendo por el total obtenemos probabilidad de intersecciones
(tablaContingenciaRel = tablaContingenciaAmpliada / totalTabla)

# Dividiendo las filas por sus sumas (usando prop.table)
# obtenemos probabilidades condicionadas por positivo o negativo.
(tablaMarginalFilas = prop.table(tablaContingencia, margin = 1))
addmargins(tablaMarginalFilas) 


# Dividiendo las columnas por sus sumas (usando prop.table)
# obtenemos probabilidades condicionadas por enfermo o sano.
(tablaMarginalColumnas = prop.table(tablaContingencia, margin = 2))
addmargins(tablaMarginalColumnas)  # Condicionadas por sano / enfermo



