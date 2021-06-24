# Corregir cosas
setwd("/Users/puchu/Documents/EAFIT Completo/Semestre Actual/Semillero de Investigación/Proyectos Daniel/Data Scarping/Correcciones")

#Librerias
library(magrittr)
library(stringr)
library(MASS)
library(corrplot)
library(ggplot2)
library(reshape2) #para melt
library(grDevices)
library("scales")


#Matriz de correlación de JEL E
datos <- read.csv("tab_jel.csv", stringsAsFactors = F)%>%
  .[,c(1,2)]

# Solo deja la categoría en cada fila
datos$source <- substr(datos$source,1,1)
datos$target <- substr(datos$target,1,1)
tbl <- table(datos$source,datos$target) #Crea tabla de contingencia

corr_matrix<- cor(tbl) #Guarda matriz de correlación

corrplot(corr_matrix, method = "circle", "upper", tl.col = "black") #Grafica Mat. Corr

rm(list=ls()) #Borra todos los environments
#------------------------------------------------------------------#
#---------------- Transición JEL areas (Trimestral)----------------#

#Importar datos
datos <- read.csv("datos_borradores_banrep.csv", stringsAsFactors = F)

#limpiar datos para obtener necesarios
jel<- datos[,c("Date", "JEL")]
jel$Date <- substring(jel$Date,1,7)

#Se separan los autores en columnas diferentes
jels <- str_split_fixed(jel$JEL,";",n = Inf)
jels <- substr(jels,1,1)

jel <- jel[,1]

jel_data <- cbind(Date=jel, jels)%>%
  as.data.frame()

#Se agregan datos trimestrales
jel_data <- subset(jel_data, substring(jel_data$Date,6,7)%in%c("03","06","09","12"))
jel_data$date<- ifelse(substring(jel_data$Date,6,7)%in%c("01","02","03"), "03",
                        ifelse(substring(jel_data$Date,6,7)%in%c("04","05","06"),"06",
                               ifelse(substring(jel_data$Date,6,7)%in%c("07","08","09"),"09",
                                      ifelse(substring(jel_data$Date,6,7)%in%c("10","11","12"),"12","0"))))
#Se pegan nuevas etiquetas creadas en date con años
jel_data$Date <- substr(jel_data$Date,1,5)%>%
  as.character()
jel_data$date <- as.character(jel_data$date)

jel_data$fecha<-paste0(jel_data$Date,jel_data$date, sep="")
jel_data$Date <- jel_data$fecha
jel_data <- jel_data[,c(-12,-13)]

  
# Se empieza a hacer el chorizo para hacer ftable despues
long_list<- list()
for(i in 2:11){
  long_list[[i-1]] <- jel_data[,c(1,i)]
}

long_list <- rbind.data.frame(long_list)%>%
  as.data.frame()
colnames(long_list)[seq(1,20,2)] <- "Date"
colnames(long_list)[seq(2,20,2)] <- "JEL"

long_list<- rbind(long_list[,c(1,2)],long_list[,c(3,4)],long_list[,c(5,6)],
           long_list[,c(7,8)],long_list[,c(9,10)],long_list[,c(11,12)],
           long_list[,c(13,14)],long_list[,c(15,16)],long_list[,c(17,18)],
           long_list[,c(19,20)])
# Termina el chorizo

#Se discriminan los JEL que se quieren utilizar
Jel_utiles <- c("E", "C", "G", "F", "J", "H", "D")

periodos <- c()
for(i in 1995:2018){
  date <- paste0(i,"-12")
  periodos <- c(periodos, date)
}

#Se organiza tabla para despues imprimir

table_long_list <- ftable(long_list)%>%
  prop.table(.,1)
table_long_list <- table_long_list
table_long_list <- as.data.frame(table_long_list)
table_long_list <- subset(table_long_list, table_long_list[,2]%in%Jel_utiles)


#Se estiliza la gráfica
ggplot(data = table_long_list, aes(x=Date,y=Freq, group = JEL, colour=JEL)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.ticks.x=element_blank(),axis.title.x = element_blank())+
                     scale_x_discrete(breaks = periodos) +
  ylab("Frecuencia Relativa")

rm(list=ls())
#------------------------------------------------------------------#
#-------- Transición JEL areas (Anual, solo para E, C, G) Caso1-----------#

#Importar datos
datos <- read.csv("datos_borradores_banrep.csv", stringsAsFactors = F)

#limpiar tabla de datos
jel<- datos[,c("Date", "JEL")]

jel$Date <- substring(jel$Date,1,4)

jel_separ <- str_split_fixed(jel$JEL,";", n=Inf)%>%
  substring(.,1,1)

ppal <- cbind(jel$Date,jel_separ)

#Se crea la longtable
ppal_long <- c()
for(i in 2:11){
  k <- ppal[,c(1,i)]
  ppal_long <- rbind(ppal_long,k)
}

ppal_long <- as.data.frame(ppal_long)
colnames(ppal_long) <- c("Date", "JEL")

#Se eliminan JEL innecesarios
Jel_utiles <- c("E","C","G")

ppal_long <- subset(ppal_long,ppal_long$JEL!="")

tab_ppal_long <- ftable(ppal_long)%>%
  #prop.table(.,1)%>%
  as.data.frame()
tab_ppal_long$variat <- c("")
for(i in 2:525){
  tab_ppal_long$variat[i]<-(tab_ppal_long$Freq[i]-tab_ppal_long$Freq[i-1])/tab_ppal_long$Freq[i-1]
}

tab_ppal_long$variat <- ifelse(tab_ppal_long$variat=="NaN",0,tab_ppal_long$variat)
# MACHETAZO INCOMING
tab_ppal_long$variat <- ifelse(tab_ppal_long$variat=="Inf",2,tab_ppal_long$variat) 
tab_ppal_long <- subset(tab_ppal_long,tab_ppal_long$Date!=1994)

#Para machetazo
tab_ppal_long <- subset(tab_ppal_long,tab_ppal_long$JEL%in%Jel_utiles)
tab_ppal_long$variat <- as.numeric(tab_ppal_long$variat)




ggplot(data=tab_ppal_long, aes(x=Date,y=variat,group=JEL,colour=JEL, fill=JEL)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank()) +
  scale_x_discrete(breaks=c(1994:2018)) +
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3))+
  ylab("Frecuencia Relativa") + geom_hline(yintercept =0, color="black", size = 0.3)
rm(list=ls())
#------------------------------------------------------------------#
#------ Transición JEL areas (Anual, solo para E, C, G) Caso2------#

#Importar datos
datos <- read.csv("datos_borradores_banrep.csv", stringsAsFactors = F)

#limpiar tabla de datos
jel<- datos[,c("Date", "JEL")]

jel$Date <- substring(jel$Date,1,4)

jel_separ <- str_split_fixed(jel$JEL,";", n=Inf)%>%
  substring(.,1,1)

ppal <- cbind(jel$Date,jel_separ)

#Se crea la longtable
ppal_long <- c()
for(i in 2:11){
  k <- ppal[,c(1,i)]
  ppal_long <- rbind(ppal_long,k)
}

ppal_long <- as.data.frame(ppal_long)
colnames(ppal_long) <- c("Date", "JEL")

#Se eliminan JEL innecesarios
Jel_utiles <- c("E","C","G")

#A diferencia del caso anterior, acá solo se tienen en cuenta estos tres JEL
# para la frecuencia
ppal_long <- subset(ppal_long,ppal_long$JEL%in%Jel_utiles)

tab_ppal_long <- ftable(ppal_long)%>%
  prop.table(.,1)%>%
  as.data.frame()

tab_ppal_long <- subset(tab_ppal_long,tab_ppal_long$JEL%in%Jel_utiles)



ggplot(data=tab_ppal_long, aes(x=Date,y=Freq,group=JEL,colour=JEL, fill=JEL)) +
  geom_area(alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank()) +
  scale_x_discrete(breaks=c(1994:2018)) +
  ylab("Frecuencia Relativa")
rm(list=ls())

#------------------------------------------------------------------#
#------------------ Comparación cada JEL con PIB-------------------#
setwd("/Users/puchu/Documents/EAFIT Completo/Semestre Actual/Semillero de Investigación/Proyectos Daniel/Data Scarping/Correcciones")
library(tibble)

# Importar datos
datos_jel <- read.csv("datos_borradores_banrep.csv", stringsAsFactors = F)
pib <- read.csv2("pib.csv")
infla <- read.csv2("Inflacion.csv")

# Homogeneizar datos

pib <- add_column(pib, type="PIB", .after = 1)
colnames(pib)[3] <- "Val"
infla <- add_column(infla, type="INF", .after = 1)
colnames(infla)[3] <- "Val"



datos_jel<- datos_jel[,c("Date", "JEL")]
datos_jel$Date <- substr(datos_jel$Date,1,7)

jel <- str_split_fixed(datos_jel$JEL,";",Inf)
jel <- substr(jel,1,1)

jel <- cbind(Date=datos_jel$Date,jel)%>%
  as.data.frame()




jel$mes<- ifelse(substring(jel$Date,6,7)%in%c("01","02","03"), "03",
                       ifelse(substring(jel$Date,6,7)%in%c("04","05","06"),"06",
                              ifelse(substring(jel$Date,6,7)%in%c("07","08","09"),"09",
                                     ifelse(substring(jel$Date,6,7)%in%c("10","11","12"),"12","0"))))


jel$Date <- paste0(substr(jel$Date,1,4),"-",jel$mes)
jel <- jel[,-12]
colnames(jel) <- "Date"

jel_long <-c()
for(i in 2:11){
  m <- jel[,c(1,i)]
  jel_long <- rbind(jel_long,m)
}

jel_long <- subset(jel_long,jel_long[,2]!="")
jel_long <- as.data.frame(jel_long)

tab_jel_long <- ftable(jel_long)%>%
  as.data.frame()
colnames(tab_jel_long)[c(2,3)] <- c("type","Val")
tab_jel_long$Camb <- c("")
for(i in 2:2058){
  tab_jel_long$Camb[i]<-tab_jel_long$Val[i]-tab_jel_long$Val[i-1]
}
tab_jel_long <- tab_jel_long[c(-1, -99,-197,-295,-393,-491,-589,-687,-785,-883,-981,-1079,-1177,-1275),]
tab_jel_long$Val <- tab_jel_long$Camb
tab_jel_long <- tab_jel_long[,-4]
tab_jel_long$Val <- as.numeric(tab_jel_long$Val)

#Ya se tienen todas las tablas en el formato fecha - valor. En caso de JEL es diferente
# porque tambien tenemos tipo de JEL. Ahora discriminamos con un solo tipo de JEL.

Jel_utiles <- c("E","C", "G","F","J","H")

periodos <- c()
for(i in seq(1995,2018,2)){
  date <- paste0(i,"-12")
  periodos <- c(periodos, date)
}

# Comienza un loop que va a sacar los gráficos para cada uno de estos temas
for(i in Jel_utiles){
  fake <- subset(tab_jel_long, tab_jel_long$type==i)
  fake$Val <- (fake$Val)/2
  base_grafica <- rbind(pib,infla,fake)
  print(ggplot(data = base_grafica, aes(x=Date, y=Val, group=type, colour=type)) + 
          geom_line()+
          scale_y_continuous(breaks = c(seq(-10,10,2)))+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          scale_x_discrete(breaks = periodos)+
          ylab("Cambios relativos (PIB, INF)")+
          xlab("")+
          labs(colour="Variable")+
          scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Cambios absolutos (JEL)"))
          )
}



rm(list=ls())

#------------------------------------------------------------------#
#---------------------- Total de Publicaciones --------------------#
datos <- read.csv("datos_borradores_banrep.csv", stringsAsFactors = F)
datos$year <- substr(datos$Date,1,4)
datos$year <- as.numeric(datos$year)

tab_year <- table(datos$year)%>%
  as.data.frame()
tab_year$Var1 <- as.numeric(as.character(tab_year$Var1))
  

plot <- ggplot(data=tab_year, aes(x=Var1, y=Freq, group=1)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot + labs(y="Documentos Publicados", x="") + 
  geom_vline(xintercept = c(2005,2017), color="red", linetype="dashed") +
  scale_x_continuous(breaks=c(1994:2018))
  
rm(list=ls())


#------------------------------------------------------------------#
#----------------- Evolución Publicaciones por tema ---------------#

#Se quieren para C, E y G
datos <- read.csv("datos_borradores_banrep.csv", stringsAsFactors = F)

jel_clave <- c("E","C","G")
datos <- datos[,c("JEL", "Date")]
datos$Date <- substr(datos$Date,1,4)%>%
  as.numeric()
datos$JEL <- substr(datos$JEL,1,2)
  
datos <- subset(datos, substr(datos$JEL,1,1)%in%jel_clave)

table(datos$JEL)
use_c <- c("C2","C3","C5")
use_e <- c("E3","E4","E5")
use_g <- c("G1","G2","G3")
tfrec <- ftable(datos)%>%
  as.data.frame()%>%
  subset(.,tfrec$JEL%in%c(use_c,use_e,use_g))

list <- list(use_c,use_e,use_g)



# Se crea una nuvea tabla con frecuencias acumuladas
freq_cum <- c()
for (i in c(use_c,use_e,use_g)) {
  fk <- tfrec
  fk <- subset(fk,fk$JEL==i)
  fk$Freq <- cumsum(fk$Freq)
  freq_cum <- rbind(freq_cum,fk)
}



#Se crean las gráficas
for(i in list){
  k <- i
  fake <- subset(freq_cum,freq_cum$JEL%in%k)
  plot <- ggplot(data = fake, aes(x=Date, y=Freq,group=JEL, colour=JEL))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab("") +
    scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140))+
    ylab("Publicaciones")
  print(plot)
}













