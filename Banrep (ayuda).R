#------------------- Web Scraping Banrep ------------------------------------#
library(rvest)
library(magrittr)
setwd("/Users/puchu/Documents/EAFIT Completo/Semestre Actual/Semillero de Investigación/Proyectos Daniel/Data Scraping")

#Numero de documentos que se quiere revisar (n)
n <- 21


#---------------

list_docum <- c()
# Se hace un loop para tomar todas las páginas y crear listado con todos los documentos
for (i in seq(0,(n-20),20)) {
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
  list_docum
  
invg_name<- c() 
author <- c()
code <- c()
date <- c()
resumen <- c()
abstract <- c()
jel <- c()
keywords <- c()
p_claves <- c()
pos_1 <- c()
pos_2 <- c()
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
  # Nota: El siguiente codigo analiza las posiciones de la siguiente manera:
  # 1. Si en la posicion 1 el titulo es "Abstract", el resumen no existe.
  # 2. Si el abstract está en la posición 2, el resumen está en la 1.
  # 3. Si el abstract no existe, debe haber un resumen en la posición 1.
  
  pos_1 <-web%>%
    html_nodes(".hidden-xs+ .table h5")%>%
    html_text() 
  pos_2<- web%>%
    html_nodes(".simple-item-view-description:nth-child(4) h5")%>%
    html_text()
  
  if(pos_1=="Abstract"){
    abs <- web%>%
      html_nodes(".hidden-xs+ .table div")%>%
      html_text()
    res <- NA
  } else if (pos_2=="Abstract"){
    abs <- web%>%
      html_nodes(".simple-item-view-description:nth-child(4) div")%>%
      html_text()
    res <- web%>%
      html_nodes(".hidden-xs+ .table div")%>%
      html_text()
  }  else {
    abs <- NA
    res <-  web%>%
      html_nodes(".hidden-xs+ .table div")%>%
      html_text()
  }
  
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
    } else {key <- NA}
  }
  
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
rm(res,abs,aut,co,fe,j,JEL,key,nom,pal,r,a,n,url,list_docum,web, pos_1, pos_2)

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


