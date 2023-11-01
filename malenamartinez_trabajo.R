######################
### TRABAJO 1 EN R ###
######################
#para cargar los datos he usado una función llamada "read.delim"
#introducimos los datos a estudio en una variable llamada "datos"
datos <- read.table("datos-trabajoR.txt",sep="\t",header=TRUE)
#nota: sep="\t", header=TRUE sirve para que reconozca los encabezados de las tablas

#comprobamos que los datos se han cargado bien en la variable 
datos 
class(datos)

#compuebo que se ha formado bien
datos

#######################################################
##1.Carga los datos y exáminalos en R. Emplea las funciones head(),
#summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos
#tratamientos?

head (datos) #aparece el encabezado
summary (datos)# con esta función obtenemos: el mínimo, el 1st quintal, 
#la mediana, la media, el 3rd quintal y el máximo (calculado de tratamiento, veriable 1 y variable2)
dim (datos) #sirve para obtener las dimensiones de la matriz o para modificarlas. Las dimesiones de mi metriz son 50 3
str (datos) #sirve para ver la estructura de la matriz 
##respuesta: hay 5 tratamietos para dos variables

#########################################################
##2. Haz un boxplot para nuestros datos. Uno para cada variable.
#Colorea a Variable 1 y a Variable 2 de forma diferente (guarda esos
#colores para las siguientes gráficas)

##boxplot de "datos"
boxplot(datos, col=c("gray","blue","pink") ,main="boxplot de datos")

##bloxplot para variable1
boxplot(Variable1 ~ Tratamiento,data= datos, col="gray", main= "Boxplot_Variable1")

##boxplot para variable2
boxplot(Variable2 ~ Tratamiento,data= datos, col="gray", main= "Boxplot_Variable2")

################################################################
##3.Haz un gráfico de dispersión con las dos variables. Cada
#tratamiento debe de ir de un color distinto. ¡Como en la siguiente
#imagen! Pista: usa col=datos$Tratamiento
plot(datos$Variable1, datos$Variable2,
	main= "scatterplot con dos variables",
	xlab = "Variable1",
	ylab = "Variable2", 
	col = datos$Tratamiento,
) 

###################################################################
##4.Ponle leyenda al gráfico del apartado anterior. En el margen inferior
#derecho. Pista: investiga sobre legend()
leyenda_datos <- c(datos)
legend ("bottomright",legend = c("Tto1", "Tto2","Tto3","Tto4","Tto5"),fill = c("black", "pink", "green", "aquamarine", "blue"))


#########################################################################
##5.Haz un histograma para cada variable. Recuerda mantener los
#colores.
#histograma para Variable1
hist(datos$Variable1, col= c("gray", "blue"),main="Histograma_Variable1")
#histograma para Variable2
hist(datos$Variable2, col= c("gray","pink"), main="Histograma_Variable2")

#####################################################################
##6.Haz un factor en la columna tratamiento y guárdalo en una
#variable. Pista: factor(factor$Tratamiento)

#definimos que "Tratamientos" va a ser un factor 
factor (datos$Tratamiento)

#guardamos el factor "Tratamientos" en una variable llamada "factor_tto"
factor_tto <- factor (datos$Tratamiento)

#comprobamos que se ha guardado correctamente
factor_tto

#######################################################################
##7.Calcula la media y la desviación estándar para cada tratamiento. Recomendación: es
#más fácil si usas aggregate() o tapply().

#cálculo de la media para Tratamientos con la Variable1
aggregate (Tratamiento ~ Variable1, datos,mean) 
#calculo de la media para Tratamientos con la Variable2
aggregate (Tratamiento ~ Variable2, datos, mean)


#cálculo de la desviación estandar con la Variable1
#nota: para calcular la desviación estandar uso la función sd(), sin argumentos
aggregate (Tratamiento ~ Variable1, datos, sd) 
#nota: aparecen valores faltantes, por lo que se puede a usar la función de la sd() "na.rm=TRUE"
#para que la calculadora excluya los valores faltantes

#cálculo de la desviación estandar con la Variable2
aggregate (Tratamiento ~ Variable2, datos, sd) 

########################################################################
##8.Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si
usas table() con el factor
table (factor_tto)
##respuesta: cada tratamiento tiene 10 elementos

##########################################################################
##9.Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una
variable diferente

#introduzco los datos de Tratamiento1 en la variable "tto_1"
tto_1 <- datos[datos$Tratamiento == "1", ]
#compruebo que se ha guardado bien
tto_1

#introduzco los datos de Tratamiento4 en la variable "tto_4"
tto_4 <- datos[datos$Tratamiento == "4",]
#compruebo que se han guardado bien
tto_4

###########################################################################
##10.Nuestra hipótesis nula es que las medias de tratamiento 1 y tratamiento 4 para la
#Variable 1 son iguales. ¿Puedes comprobarlo? Para ello, necesitarás comprobar
#primero si los datos se distribuyen de forma normal. En función del resultado de la
#prueba de normalidad, ¿qué test usarías? ** En general, asumimos que las muestras
#son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo a tus
#resultados

##Comprobación de distribución normal: 
#para comprobar que los datos presentan una distribución normal, empleamos el estudio shapiro
#nota: establecemos un valor de significancia de α=0.05

#realizo el estudio saphiro con los datos de tto_1
shapiro_tto_1 <- shapiro.test(tto_1$Variable1)
#compruebo el resultado del test con la tto_1
shapiro_tto_1

#realizo el estudio saphiro con los datos de tto_4
shapiro_tto_4 <- shapiro.test(tto_4$Variable1)
#compruebo el resultado del test con tto_4
shapiro_tto_4

#analizo los resultados de shapiro_tto_1 y shapiro_tto_4
#El resultado del p-value de shapiro_tto_1 y del p-value de shapiro_tto_4 son 
#ambos mayores que el valor de significancia 0.05, por lo que determinamos que 
#siguen una distrubución normal y no hay evidencia para rechazar la hipotesis nula todavia

#calculamos la varianza y las media de tto_1 y tto_4 de sus variables 1 para comprobar si son iguales
#media y varianza de tto_1 (usamos la función mean() para la media, y var() para la varianza)
mean (tto_1$Variable1)
var (tto_1$Variable1)

#media y varianza de tto_4
mean (tto_4$Variable1)
var (tto_4$Variable1)

##Test estadístico: 
#analisis de los resultados
#las varianzas de las dos tratamientos referido a la Variable1 no son iguales, 
#por lo que deberíamos calcular una Prueba t para varianzas desiguales o Prueba t de Welch
resultado_t <- t.test(tto_1$Variable1, tto_4$Variable1, var.equal = FALSE)

#comprobamos el resultado 
resultado_t

##Interpretación de los resultados de la Prueba t de Welch:
#para interpretar el resultado se deben analizar los valores de t y p 
#el valor de t es negativo pero grande, lo que indica que las medias están muy distanciadas
#el valor de p-value es más pequeño que 0.05 (2.576e-07), lo que indica que hay evidencia estadística
#de rechazar la hipotesis nula, que determinaba que las medias de los tratamientos 1 y 4 
#referido a la Variable1 son iguales. 

##Conclusión:
#Los resultados del test estadístico Prueba t del Welch indican que las medias de la
#Variable1 para el tratamiento1 y tratamiento4 no son iguales, por lo que se rechaza la 
#hipotesis nula



r