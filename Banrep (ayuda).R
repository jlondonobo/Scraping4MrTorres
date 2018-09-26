#------------------- Web Scraping Banrep ------------------------------------#
library(rvest)
library(magrittr)
setwd("/Users/puchu/Documents/EAFIT Completo/Semestre Actual/Semillero de Investigación/Proyectos Daniel/Data Scarping")

#Numero de documentos que se quiere revisar (n)
#n <- 1059


#---------------

list_docum <- c()
# Se hace un loop para tomar todas las páginas y crear listado con todos los documentos
for (i in seq(0,1059,20)) {
  url <- paste0("http://repositorio.banrep.gov.co/handle/20.500.12134/5018/browse?rpp=20&sort_by=2&type=dateissued&offset=",i,"&etal=-1&order=DESC")
  web <- read_html(url)
  docum_pag <- web%>%
    html_nodes(".artifact-title a")%>%
    html_attr("href")
  links <- paste0("http://repositorio.banrep.gov.co",docum_pag)
  list_docum <- c(list_docum,links )
}

#Se borran las variables innecesarias (paso innecesario)
rm(docum_pag)
rm(links)
rm(url)
rm(i)
rm(web)
  

invg_name<- c() 
author <- c()
code <- c()
date <- c()
resumen <- c()
abstract <- c()
jel <- c()
keywords <- c()
p_claves <- c()

# Estos son especiales por el codigo de resumen y abstract (necesarios)
# NO TOCAR
res <- c()
abs <- c()

for(j in list_docum){
  #El paso url <- j se puede obviar, pero lo hago por mantener el orden
  url <- j
  web <- read_html(url)
  #Scraping de nombres
  nom <- web%>%
    html_nodes(".first-page-header")%>%
    html_text()
  invg_name <- c(invg_name, nom)
  
  #Scraping de autores
  aut <- web%>%
    html_nodes(".simple-item-view-authors a")%>%
    html_text()%>%
    paste(., collapse = ";")
  author<-c(author,aut)
  
  #Scraping del código
  co <- web%>%
    html_nodes(".simple-item-view-relation-isversionof div")%>%
    html_text()
  code <- c(code, co)
  
  #Scraping de fecha
  fe <- web%>%
    html_nodes(".simple-item-view-date")%>%
    html_text()%>%
    .[1]%>%
    gsub("\nFecha de publicación","",.)
  
  # Esta parte añade un -01 en caso de que el docu no tenga dia de pub.
  # esto se hace para que el as.Date lo pueda entender.
  if(nchar(fe)==7){
    fe <- paste0(fe,"-01")
  } else{}
  #fe <- as.Date(fe, format = "%Y-%m-%d")
  
  date <- c(date, fe)
  
  #Scraping del Abstract y resumen
  
  b <- c()
  for (l in 2:4){
    p <-  web%>%
      html_nodes(paste0(".simple-item-view-description:nth-child(",l,") h5"))%>%
      html_text()
    if(!is.null(p) & length(p)!=0){
      b[l] <- p
    } else {b[l] <- 0}
    
    if(b[l]=="Resumen"){
      res <- web%>%
        html_nodes(paste0(".simple-item-view-description:nth-child(",l,") div"))%>%
        html_text()
    } else {""}
    if(b[l]=="Abstract"){
      abs <- web%>%
        html_nodes(paste0(".simple-item-view-description:nth-child(",l,") div"))%>%
        html_text()
    } else {}
  }
  
  # Esta sección dice "Si el vector de nombres no tiene alguno de los dos
  # es porque este no existe.
  if(!("Resumen" %in% b)){
    res <- NA
  } else {""}
  if(!("Abstract" %in% b)){
    abs <- NA
  } else {""}
  
  abstract <- c(abstract, abs)
  resumen <- c(resumen, res)
  
    
    
  
#Scraping de JEl --------------------------- REVISAR -----------------

# En esta parte se hace un loop que lee todos los headers y los elimina si
# no existen o son de length = 0. Despues toma la posición del header y lee
# el argumento (a) del nth-child que tenga como header "Códigos JEL"
  a<-c()
  for(j in 2:7){
    r <- web%>%
      html_nodes(paste0(".jelspa:nth-child(",j,") h5"))%>%
      html_text()
    if(!is.null(r) & length(r)!=0){
      a[j] <- r
    } else {a[j] <- 0}
    if(a[j]=="Códigos JEL"){
      JEL <- web%>%
              html_nodes(paste0(".jelspa:nth-child(",j,") a"))%>%
              html_text()
    } else {""}
    if(a[j]=="Palabras clave"){
      pal <- web%>%
        html_nodes(paste0(".jelspa:nth-child(",j,") a"))%>%
        html_text()
    } else {""}
    
    if(a[j]=="Keywords"){
      key <- web%>%
        html_nodes(paste0(".jelspa:nth-child(",j,") a"))%>%
        html_text()
    } else {""}
  }
  
  if(!("Keywords" %in% a)){
    key <- NA
  } else {""}
  
#Creo que en key hay un error pues solo va a arrojar el resultado de evaluar
# corregir a[7]
  
# Esta parte hace arreglos menores. Se guarda solo el código JEl, se
# eliminan espacios a la derecha y luego se convierte el vecto en un solo
# string separado por un ";"
  
  JEL <- substring(JEL, 1,3)%>%
    trimws(which = "right")%>%
    paste(collapse = ";")
  jel <- c(jel, JEL)
  
  pal <- paste(pal, collapse = ";")
  p_claves <- c(p_claves, pal)
  
  key <- paste(key, collapse = ";")
  keywords <- c(keywords, key)

  
#-----------------------------------------------------------------------  
#Finaliza el loop con el }
}

# Se eliminan variables residuos por estética
rm(res,abs,aut,co,fe,j,JEL,key,nom,pal,r,a,n,url,list_docum,web,b)

#Se crea un data frame para luego escribirlo.
tabla <- data.frame(Title=invg_name,
                    Authors = author,
                    Date = date,
                    Codigo = code,
                    JEL = jel,
                    "Palabras Claves" = p_claves,
                    Keywords = keywords,
                    Resumen = resumen,
                    Abstract = abstract)
write.csv(tabla, file = "datos_borradores_banrep.csv", row.names = F)

# NOTA IMPORTANTE a las fechas que no tienen "dia" se les asignó el valor
# para día de 01

