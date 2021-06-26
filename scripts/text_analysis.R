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
own_stopwords <- c("así", "05r", "documento", "cada", "través")
full_stopwords <- c(stopwords("spanish"), own_stopwords)

tokenized_corpus <- tokens(summary_corpus, remove_punct=T, remove_symbols=T, remove_numbers=T)%>%
  tokens_remove(full_stopwords, min_nchar=3)

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
  ungroup()


  #Plot diversidad lexica por autor (max y min)
  max_lexdiv_plot <- ggplot(top_n(author_lexdiv,5,meanTTR), aes(x=reorder(Authors, meanTTR), y=meanTTR)) + geom_bar(stat="identity", fill="#F8766D") +
    ylim(0, 1) +
    coord_flip()+
    labs(x="", y="Diversidad léxica")
  min_lexdiv_plot <- ggplot(top_n(author_lexdiv,-5,meanTTR), aes(x=reorder(Authors, meanTTR), y=meanTTR)) + geom_bar(stat="identity", fill="#00BFC4")+
    ylim(0, 1)+
    coord_flip()+
    labs(x="", y="Diversidad léxica")
  
  pdf(here('assets','plots', 'text_analysis', 'lexical_diversity.pdf'))
  cowplot::plot_grid(max_lexdiv_plot, min_lexdiv_plot, ncol = 1, align="v", labels = c('Max', 'Min'))
  dev.off()
  
#Nube de palabras

#agrupando corpus en solo tres documentos
dfm_summaries@docvars <- dfm_summaries@docvars%>%
  mutate(Periodo = case_when(lubridate::year(Date)<=1999 ~ "Precrisis - 1999",
                             lubridate::year(Date)<=2007 ~ "Precrisis - 2008",
                             lubridate::year(Date)>2007 ~ "Poscrisis - 2008"))
dfm_summaries_grouped <- dfm_group(dfm_summaries, group=Periodo)

  #Plot nube de palabras comparativa
  set.seed(0)
  pdf(here('assets','plots', 'text_analysis', 'wordcloud.pdf'))
  quanteda.textplots::textplot_wordcloud(dfm_summaries_grouped,
                                         min_count = 80,
                                         labeloffset = -0.02,
                                         comparison = T)
  dev.off()


