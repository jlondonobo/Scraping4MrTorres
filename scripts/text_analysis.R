#----------------------------------------------------------------------------#
#-----------------| BANREP'S ARTICLES TEXT ANALYSIS |------------------------#
#----------------------------------------------------------------------------#

library(quanteda)               #analisis de texto
library(tidyverse)              #manipulacion de datos
library(ggplot2)                #plots
library(here)                   #directorios

### Datos -----------------------------------------------------------------

articles_metadata <- read.csv(here('data', 'raw', 'articles_metadata.csv'), stringsAsFactors = F)%>%
  filter(Resumen!="")

### Document-feature matrix ------------------------------------------------

#Corups (collection of text)
summary_corpus <- corpus(articles_metadata, docid_field = "Title", text_field = "Resumen" )

#Tokens (vectors of words for each document)
tokenized_corpus <- tokens(summary_corpus, remove_punct=T, remove_symbols=T, remove_numbers=T)%>%
  tokens_remove(stopwords("spanish"), min_nchar=3)

#Document-feature matrix (matrix counting word appearances in each document of corpus)
dfm_summaries <- dfm(tokenized_corpus)


### Metricas ----------------------------------------------------------------

#Keyness
keyness_target <- lubridate::year(dfm_summaries@docvars$Date)>2007 #vector de boolean si documento escrito despues de 2007
keyness <- quanteda.textstats::textstat_keyness(dfm_summaries, target = keyness_target) #keyness calculada
attr(keyness, "groups") <- c("2008-2018","1994-2007")

  #Plot keyness (ggplot)
  pdf(here('assets','plots', 'text_analysis', 'keyness_crisis.pdf'), width=10)
  quanteda.textplots::textplot_keyness(keyness) +
    labs(x="Estadístico Chi2",title="Keyness de resumenes",subtitle="Comparación antes y después de la crisis del 2008")
  dev.off()

#Diversidad de lexico
lexdiv <- textstat_lexdiv(dfm_summaries)%>%
  rename(Title=document)

author_lexdiv <- articles_metadata%>%
  separate_rows(Authors, sep = ';')%>%
  select(Authors, Title)%>%
  left_join(lexdiv)%>%
  group_by(Authors)%>%
  summarise(meanTTR=mean(TTR), n_articles=n())%>%
  ungroup()%>%
  arrange(desc(meanTTR))


  #Plot lexdive por autor
  ggplot(top_n(author_lexdiv,5,meanTTR), aes(x=Authors, y=meanTTR)) + geom_bar(stat="identity")

  
  gridExtra::grid.arrange()




