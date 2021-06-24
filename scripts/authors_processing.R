#-------------------------------------------#
#--------------| SETUP |--------------------#
#-------------------------------------------#

library(stringr)     #manipulacion de texto
library(tidyverse)   #manipulacion de dats
library(here)        #acceder a direcciones locales

#Importando titulo y nombres de autores
articles_metadata <- read.csv(here('data', 'raw', 'articles_metadata.csv'))%>%
  select(1,2)

#Convirtiendo a formato tidy
#-- implica separar autores cada uno en una fila (duplicando titulo)
articles_dup <- articles_metadata %>%
  separate_rows(Authors, sep = ';')

#Obteniendo todas las combinaciones de autores
combinaciones <- articles_dup %>%
  group_by(Title) %>%
  filter(n() > 1) %>%
  split(.$Title) %>%
  map(., 2) %>%
  map(~combn(.x, m = 2)) %>%
  map(~t(.x)) %>%
  map_dfr(as_tibble)

#Renombrando y anadiendo columnas
combinaciones <- combinaciones%>%
  rename(source=1, target=2)%>%
  mutate(weight=1, type = "Undirected")

#Escribiendo archivo a csv
write.csv(combinaciones, here('data', 'processed', 'authors_nodes.csv'), row.names = F)


