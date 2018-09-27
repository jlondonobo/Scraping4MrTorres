library(stringr)
library(magrittr)
# Se leen unicamente nombres de documentos y autores
setwd("/Users/puchu/Documents/EAFIT Completo/Semestre Actual/Semillero de Investigación/Proyectos Daniel/Data Scarping")
datos <- read.csv("datos_borradores_banrep.csv")
datos <- datos[,c(1,2)]

#Se separan los autores en columnas diferentes
autores <- str_split_fixed(datos$Authors,";",n = Inf)

datos <- datos[1]

tab_in_out <- c()
for(i in 1:nrow(autores)){

# Se halla la cantidad de columnas que tienen datos para cada fila
n <- sum(autores[i,] != "")

# Se hallan todas las combinaciones posibles de estos datos
# Utilizo el if pues cuando hay solo un autor (n=1) no se pueden hacer combinaciones de 2.
# Lo que hace el codigo es crear un vector con el autor y un espacio blanco.
d <- if(n>1){
  combn(autores[i,1:n],2)
} else {c(autores[i,1],"")}

# Las combinaciones se convierten en una matriz para poder trasponerla 
# añado el if para los casos en que solo hay un autor pues ncol y nrow no se pueden utilizar
# para vectores [solo funcionan en matrices]. NROW y NCOL si funcionan para vectores.
d <- if(n>1){
matrix(d, nrow = nrow(d), ncol = ncol(d))
} else {matrix(d, NROW(d), NCOL(d))}

# Se traspone la matriz para que quede en 2 columnas
d <- t(d)
e <- matrix(datos[i,1], nrow = nrow(d), ncol = 1)

# Se combinan la columna de titulos y la columna de investigadores
pi <- cbind(e,d)

# Se pone el nombre a la cada columna
colnames(pi)<-c("Nombre","Input", "Output")

# Se combinan las columnas de cada paper
tab_in_out <- rbind(tab_in_out, pi)
}



