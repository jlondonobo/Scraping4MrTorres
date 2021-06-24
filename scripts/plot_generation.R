#---------------------------------------------------------------#
#------------------| ASSET GENERATION |-------------------------#
#---------------------------------------------------------------#

#Librerias
library(tidyverse)     #manipulacion de datos
library(stringr)    #manipulacion de strings    #
library(ggplot2)    #graficos generales
library(here)       #para navegar folders

#Determinar tema de ggplot
my_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks.x=element_blank(), axis.title.x = element_blank())

#Importar datos
articles <- read.csv(here('data', 'raw', 'articles_metadata.csv'), stringsAsFactors = F)

articles <- articles%>%
  mutate(Date =as.Date(Date))

# Transición JEL areas (Anual) ---------------------------------------

#Seleccionar solo fecha y codigos
jel_dates<- articles%>%
  select(Date, JEL)

#Separando JEL codes
jel_dates_split <- jel_dates %>%
  separate_rows(JEL, sep = ';')%>%
  mutate(JEL = substr(JEL,1,1))%>%
  mutate(Quarter = lubridate::quarter(Date),
         Year = lubridate::year(Date))%>%
  relocate(Date, Year, Quarter)

#Mantenemos solo codigos JEL de interes
desired_jel <- c("E", "C", "G", "F", "J", "H", "D")
jel_dates_split <- jel_dates_split%>%
  filter(JEL %in% desired_jel)

#Contamos JEL por year
jel_dates_split_plot <- jel_dates_split%>%
  group_by(Year, JEL)%>%
  summarise(Count=n())
  
#Grafica
pdf(here('assets', 'plots', 'general', 'jel-codes_frequency_year.pdf'))

ggplot(jel_dates_split_plot, aes(x=Year,y=Count, colour=JEL)) + geom_line() + 
  my_theme +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(y="Frecuencia", title="Frecuencia de códigos JEL por año")

dev.off()


# Artículos publicados anualmente -----------------------------------------

yearly_articles <- articles%>%
  select(Title, Date)%>%
  mutate(Year = lubridate::year(Date))%>%
  group_by(Year)%>%
  summarise(Count = n())

cuts <- data.frame(Ref = c("Miguel Urrutia", "José D. Uribe", "Juan J. Echavarría"),
                   vals = c(1994, 2005,2017),
                   stringsAsFactors = FALSE)
  
pdf(here('assets','plots', 'general', 'published_articles.pdf'))

ggplot(yearly_articles, aes(x=Year, y=Count)) + geom_line() +
  my_theme +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(y="Numero de documentos", title="Total de artículos publicados por año - Lineas según gerente ") + 
  geom_vline(xintercept = c(1994, 2005,2017), color="red", linetype="dashed") +
  geom_text(cuts, mapping = aes(x = vals,
                          y = -2,
                          label = Ref,
                          hjust = 0,
                          vjust = 2,
                          angle = 90))

dev.off()


# Publicaciones por tema --------------------------------------------------

#Manteniendo solo documentos con codigos E, C, G
jel_dates_split_detail <- jel_dates %>%
  separate_rows(JEL, sep = ';')%>%
  filter(substr(JEL,1,1) %in% c("E","C","G"))%>%
  mutate(SubJEL = substr(JEL,1,2))%>%
  mutate(Quarter = lubridate::quarter(Date),
         Year = lubridate::year(Date))%>%
  relocate(Date, Year, Quarter)%>%
  #Calculating cumsum
  group_by(Year, SubJEL)%>%
  arrange(.by_group = T)%>%
  summarise(Count=n())%>%
  group_by(SubJEL)%>%
  mutate(Cumulative_count = cumsum(Count))

#Selecting subcodes of interest
use_c <- c("C2","C3","C5")
use_e <- c("E3","E4","E5")
use_g <- c("G1","G2","G3")

list <- list(use_c, use_e, use_g)
names(list)<- c("jel_c", "jel_e", "jel_g")

#Se crean las gráficas
for(i in 1:length(list)){
  temp_df <- jel_dates_split_detail%>%
    filter(SubJEL%in%list[[i]])
  
  plot <- ggplot(data = temp_df, aes(x=Year, y=Cumulative_count, colour=SubJEL)) + geom_line()+
    my_theme + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
    labs(y="Número de artículos", title = "Artículos publicados según subcódigo JEL")
  
  ggsave(plot, file=here('assets','plots', 'general', paste0("frequency_", names(list)[i],".pdf")))
}






