library(tidyverse)
library(stringr)
library(ggbeeswarm)

atpMatchesClean=readRDS('atpMatchesClean.rds')


#manipulate data to make visualizations easy 
#winners
player1<-atpMatchesClean %>% 
  select(match_id,winner,contains('1')) %>% 
  mutate(WL=ifelse(winner==1,'W','L'))
names(player1)=str_replace_all(names(player1),'1','')

player2<-atpMatchesClean %>% 
  select(match_id,winner,contains('2')) %>% 
mutate(WL=ifelse(winner==2,'W','L'))
names(player2)=str_replace_all(names(player1),'2','')

atpMatchesWinner<-rbind(player1,player2) %>% arrange(match_id,WL) 
atpMatchesWinner<-atpMatchesWinner %>% filter(str_extract(match_id,'[0-9]{4}') %in% c('2015','2016','2017','2018','2019','2020'))
pdf(file='C:\\Users\\George Zhou\\Desktop\\tennis ML\\Data Exploration\\DataExploration.pdf',onefile = T)
for( i in 3:(ncol(atpMatchesWinner)-1)){
  print(ggplot(atpMatchesWinner) + 
    geom_quasirandom(aes_string(x='WL',y=names(atpMatchesWinner)[i],color='WL'))+
    theme_light()+
    labs(title = str_c(c(names(atpMatchesWinner)[i],' Distribution'),collapse  = ''))+
    theme(legend.position = 'none'))
}
dev.off()




