library(tidyverse)
source('dataCleaning.R')



ptsWon=points_won(atpMatches)
fstSv<-fstSvPct(atpMatches)
fstSvWon<-fstSvWon(atpMatches)
sndSv<-SndSvWon(atpMatches)
acesDF<-acesDf(atpMatches)
winUE<-winsUE(atpMatches)
bp=breakPoints(atpMatches)
winner=winner(atpMatches)
forced=forcedpoints(atpMatches)
unRet=unreturnable(atpMatches)


atpMatchesClean<-ptsWon %>% 
  inner_join(fstSv) %>% 
  inner_join(fstSvWon) %>% 
  inner_join(sndSv) %>% 
  inner_join(acesDF) %>% 
  inner_join(winUE) %>% 
  inner_join(bp) %>% 
  inner_join(netPoints) %>%
  inner_join(forced) %>% 
  inner_join(unRet) %>% 
  inner_join(winner)
saveRDS(atpMatchesClean,'atpMatchesClean.rds')

#use to validate results to check ATP website 
samp<-sample(1:nrow(atpMatchesClean),10)

atpMatchesClean$match_id[samp]


#check for NAs 
for(i in 1:ncol(atpMatchesClean)){
  print(names(atpMatchesClean)[i])
  print(sum(is.na(atpMatchesClean[i])))
}


#duplicate rows
nrow(unique(atpMatchesClean))==nrow(atpMatchesClean)
