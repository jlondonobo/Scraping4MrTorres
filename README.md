# Scraping4MrTorres

## Objetivo
Este proyecto nació como una idea del departamento de macroeconomía de la Universidad EAFIT para comprender las áreas de interés y el sentimiento de los investigadores del Banco de la República (Colombia) a través del tiempo. Para lograr este propósito creamos una base de datos con información de más de 1000 artículos publicados por investigadores del Banco de la República entre 1994 y 2018.

## Datos
Capturamos los metadatos de 1059 artículos escritos  entre 1994 y 2018. Nuestra base completa contiene, para cada artículo: *título*, *autor(es)*, *fecha de publicación*, *codigo interno*, *lista de clasificaciones JEL*, *palabras claves*, *resumen* *e idioma del documento*. Los datos están disponibles en `data/articles_metadata.csv`.

## Método
Usamos la librería `rvest` para extraer la información dispuesta en el [repositorio del Banco de la República](https://repositorio.banrep.gov.co/handle/20.500.12134/5018/browse?type=dateissuedhttps://repositorio.banrep.gov.co/handle/20.500.12134/5018/browse?type=dateissued). La metodología exacta está dispuesta en el documento `scripts/scraper_metadata.R`.

## Análisis
Tras obtener los datos hicimos un análisis de texto y un análisis de redes preliminar. Los resultados de este análisis son presentados en la carpeta `results/.`
