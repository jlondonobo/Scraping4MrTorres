#----------------------------------------------------------------------------#
#-------------------| SCRAPING BANREP'S ARTICLES |---------------------------#
#----------------------------------------------------------------------------#

library(rvest)        #web-scraping
library(tidyverse)    #manipulacion de datos

# Extrayendo links de documentos ------------------------------------------

#Parametros
n <- 1059 #numero de documentos a extraer

#Extrayendo url de cada articulo
list_doc <- c()
for (i in seq(0,n,20)) {
  #1. conectando con pagina web
  url <- paste0("http://repositorio.banrep.gov.co/handle/20.500.12134/5018/browse?rpp=20&sort_by=2&type=dateissued&offset=",i,"&etal=-1&order=DESC")
  web <- read_html(url)
  #2. extrayendo url de los 20 articulos en la pagina
  docum_pag <- web%>%
    html_nodes(".artifact-title a")%>%
    html_attr("href")
  #3. creando urls completas y uniendo a lista de todas las urls
  links <- paste0("http://repositorio.banrep.gov.co", docum_pag)
  list_doc <- c(list_doc,links )
  #4. pasando a proxima pagina
}

#Removiendo variables
rm(docum_pag, links, url, i, web)

invg_name<- c() 
author <- c()
code <- c()
date <- c()
resumen <- c()
abstract <- c()
jel <- c()
keywords <- c()
p_claves <- c()

#Resumen y abstract
res <- c()
abs <- c()

for(article_url in list_doc){
  #1. conectando con pagina web
  web <- read_html(article_url)
  #2. extrayendo elementos
  #titulo
  nom <- web%>%
    html_nodes(".first-page-header")%>%
    html_text()
  invg_name <- c(invg_name, nom)
  
  #autores
  aut <- web%>%
    html_nodes(".simple-item-view-authors a")%>%
    html_text()%>%
    paste(., collapse = ";")
  author<-c(author,aut)
  
  #codigo
  co <- web%>%
    html_nodes(".simple-item-view-relation-isversionof div")%>%
    html_text()
  code <- c(code, co)
  
  #fecha de publicacion
  fe <- web%>%
    html_nodes(".simple-item-view-date")%>%
    html_text()%>%
    .[1]%>%
    gsub("\nFecha de publicación","",.)
  
    #si fecha esta en formato %Y-%m llenamos dia con primero del mes
    if(nchar(fe)==7){
      fe <- paste0(fe,"-01")
    }
  
  date <- c(date, fe)
  
  #resumen y abstract
  #-- al momento de escribir este codigo no sabia hacer seleccion usando xpath
  #-- este cofigo puede hacerse muchisimo mas corto usando xpath
  
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
rm(res,abs,aut,co,fe,j,JEL,key,nom,pal,r,a,n,url,list_doc,web,b)

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

write.csv(tabla, file = here('data', 'raw', 'articles_metadata.csv'), row.names = F)


