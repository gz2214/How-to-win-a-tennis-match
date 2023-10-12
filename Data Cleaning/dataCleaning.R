#data cleaning 
library(tidyverse)
library(stringr)
atpMatches<-read.csv('charting-m-points.csv',stringsAsFactors = F)


#NAs?
mean(is.na(atpMatches)) #this is due to nature of document



#only atp matches that serve as points will count, will look at 2014-2019
atpMatches<-atpMatches %>% filter(str_detect(match_id,'(Laver)|(Davis)')==F,str_extract(match_id,'^[0-9]{4}') %in% as.character(c(2014:2019)))


#filtering out matches which ends in an retirement
retiredMatches<-atpMatches %>% 
                  group_by(match_id) %>% 
                  slice(n()) %>% 
                  filter(str_detect(Notes,fixed('retire',ignore_case = T))==T) %>% 
                  pull(match_id)
#some matche were not finished for whatever reason, filtering out those too
notFinished<-atpMatches %>% 
  filter(!(match_id %in% retiredMatches)) %>% 
  group_by(match_id) %>% group_by(match_id) %>% slice(n()) %>% 
  filter(PtsAfter!='GM') %>% pull(match_id)

atpMatches<-atpMatches %>% 
              filter(!(match_id %in% retiredMatches),!(match_id %in% notFinished))
#how many matches per year
atpMatches %>% 
  select(match_id) %>% 
  unique() %>% 
  mutate(year=str_extract(match_id,'^[0-9]{4}'))%>% 
  pull(year) %>% table() 


saveRDS(atpMatches,'atpMatches.rds') #saving RDS so will be easy to grab later

#winner of match 
winner=function(atpMatches){
  df=atpMatches %>% 
    group_by(match_id) %>% 
    slice(n()) %>% 
    summarise(winner=PtWinner) %>% 
    mutate(loser=ifelse(winner==1,2,1))
  return(df)
}

#points won stat
points_won<-function(atpMatches){
  df=atpMatches %>% 
    count(match_id,PtWinner) %>% 
    pivot_wider(names_from = PtWinner,values_from = n,names_prefix = 'points') %>% 
    mutate(pointsPt1=round(points1/(points1+points2),3)*100,
           pointsPt2=round(points2/(points1+points2),3)*100,
           pointsDiff=pointsPt1-pointsPt2)
  return(df)
}

#serve/return stats data 

#first serve in
fstSvPct=function(atpMatches){
  df=atpMatches %>% 
    group_by(match_id,Svr) %>% 
    summarize(fstSvPct=mean(X1stIn)*100) %>% 
    pivot_wider(names_from = Svr,values_from =fstSvPct,names_prefix = 'firstSvPct') %>% 
    mutate(fstSvPctDiff=firstSvPct1-firstSvPct2)
  return(df)
}
#first serve/Return won
fstSvWon=function(atpMatches){
  df=atpMatches %>% filter(X1stIn==1) %>%
    mutate(fstWon=ifelse(Svr==PtWinner,1,0)) %>%
    group_by(match_id,Svr) %>% 
    summarize(fstSvWon=mean(fstWon)*100) %>% 
    pivot_wider(names_from = Svr,values_from =fstSvWon,names_prefix = 'fstSvWon') %>% 
    mutate(fstSvWonDiff=fstSvWon1-fstSvWon2,
           retFstWon1=100-fstSvWon2,
           retFstWon2=100-fstSvWon1,
           retFstWondiff=retFstWon1-retFstWon2)
  return(df)
}
#2nd serve/return won
SndSvWon=function(atpMatches){
  df=atpMatches %>% filter(!is.na(X2ndIn)) %>%
    mutate(sndWon=ifelse(Svr==PtWinner,1,0)) %>%
    group_by(match_id,Svr) %>% 
    summarize(sndSvWon=mean(sndWon)*100) %>% 
    pivot_wider(names_from = Svr,values_from =sndSvWon,names_prefix = 'sndSvWon') %>% 
    mutate(sndSvWonDiff=sndSvWon1-sndSvWon2,
           retSndWon1=100-sndSvWon2,
           retSndWon2=100-sndSvWon1,
           retSndWondiff=retSndWon1-retSndWon2)
  return(df)
}

#aces/df
acesDf=function(atpMatches){
  df=atpMatches %>% 
    group_by(match_id,Svr) %>% 
    summarize(aces=sum(isAce),df=sum(isDouble)) %>% 
    pivot_wider(names_from = Svr,values_from =c(aces,df),names_sep = "") %>% 
    mutate(acesDiff=aces1-aces2,dfDiff=df1-df2)
}

#winners/unforced errors
winsUE=function(atpMatches){
  df=atpMatches%>%
    mutate(UE1=ifelse(isUnforced==T&PtWinner==2,1,0),
           UE2=ifelse(isUnforced==T&PtWinner==1,1,0),
           win1=ifelse((isRallyWinner==T|isAce==T)&PtWinner==1,1,0),
           win2=ifelse((isRallyWinner==T|isAce==T)&PtWinner==2,1,0)) %>% 
    group_by(match_id) %>% 
    summarize(win1=sum(win1),win2=sum(win2),UE1=sum(UE1),UE2=sum(UE2),winDiff=win1-win2,
              UEdiff=UE1-UE2,WUEratioDiff=(win1/(win1+UE1))-(win2/(UE2+win2)),WUEratio1=win1/(win1+UE1),WUEratio2=win2/(UE2+win2))
  return(df)
}


###break points
breakPoints<-function(atpMatches){
  deuceAD<-'((40)|(AD))-40'
  breakPointsGM<-'(-40)|(40-AD)|(GM)'
  #non next gen games
  df=atpMatches %>% 
    filter(TB.==0) %>%
    filter(str_detect(match_id,'NextGen_')==F) %>% 
    select(match_id,Svr,PtsAfter,PtWinner) %>% 
    filter(str_detect(PtsAfter,deuceAD)==F,str_detect(PtsAfter,breakPointsGM)==T) %>% 
    mutate(bkLst=ifelse(PtsAfter=='GM'&Svr!=PtWinner,1,0)) %>% 
    group_by(match_id,Svr) %>% 
    summarize(bkPtFc=sum(str_detect(PtsAfter,'(-40)|(40-AD)')),bkPtsSave=ifelse(bkPtFc==0,0,bkPtFc-sum(bkLst)),bkSavePct=ifelse(bkPtFc==0,0,100*bkPtsSave/bkPtFc))%>% 
    pivot_wider(names_from = Svr, values_from = c(bkPtFc,bkPtsSave,bkSavePct),names_sep = "") %>% 
    mutate(bkPtsWon1=1-bkSavePct2,bkPtsWon2=1-bkSavePct1,bkWonDiff=-bkSavePct2+bkSavePct1,bkSaveDiff=bkSavePct1-bkSavePct2,bkPtFcDiff=bkPtFc1-bkPtFc2)
  #next gen games  
  df1<-atpMatches %>% 
    filter(TB.==0,str_detect(match_id,'NextGen_')==T) %>% 
    select(match_id,Svr,PtsAfter,PtWinner) %>% 
    group_by(match_id,Svr) %>% 
    summarize(bkPtFc=sum(str_detect(PtsAfter,'(-40)|(40-AD)')==T),bkPtsSave=bkPtFc-sum((Svr!=PtWinner)&PtsAfter=='GM'),bkSavePct=100*bkPtsSave/bkPtFc) %>% 
    pivot_wider(names_from = Svr, values_from = c(bkPtFc,bkPtsSave,bkSavePct),names_sep = "") %>% 
    mutate(bkPtsWon1=100-bkSavePct2,bkPtsWon2=100-bkSavePct1,bkWonDiff=-bkSavePct2+bkSavePct1,bkSaveDiff=bkSavePct1-bkSavePct2,bkPtFcDiff=bkPtFc1-bkPtFc2)  
  df<-rbind(df,df1)
  return(df)
}


#netPoints
netPoints=read.csv('charting-m-stats-NetPoints.csv',stringsAsFactors = F)
netPoints<-netPoints %>% 
  filter(row=='NetPoints') %>% 
  select(match_id,player,net_pts,pts_won) %>% rename(netPts_won=pts_won)
netPoints1<-netPoints %>% 
  group_by(match_id) %>% 
  mutate(n=n()) %>% 
  filter(n%%2==1) %>% 
  select(match_id,player,net_pts,netPts_won) %>% ungroup()


for(i in 1:nrow(netPoints1)){
  if(netPoints1$player[i]==1){
    netPoints<-netPoints %>% add_row(match_id=netPoints1$match_id[i],player=2,net_pts=0,netPts_won=0)
  } else{
    netPoints<-netPoints %>% add_row(match_id=netPoints1$match_id[i],player=1,net_pts=0,netPts_won=0)
  }
}


netPoints<-netPoints %>% 
  mutate(net_ptsPct=ifelse(netPts_won==0,0,100*netPts_won/net_pts)) %>% 
  unique() %>%
  pivot_wider(names_from = player,values_from = c(net_pts,netPts_won,net_ptsPct),names_sep = "") %>% 
  mutate(netPtsDiff=net_ptsPct1-net_ptsPct2)

#forced points
forcedpoints<-function(atpMatches){
  df<-atpMatches %>% 
    group_by(match_id,PtWinner) %>% 
    summarize(forcedPointsWon=sum(isForced)) %>% 
    pivot_wider(names_from = PtWinner,values_from = forcedPointsWon,names_sep = "",names_prefix='forcedPointsWon') %>% 
    mutate(forcedPointsDiff=forcedPointsWon1-forcedPointsWon2)
  return(df)
}

#unreturnable serves

unreturnable<-function(atpMatches){
  df=atpMatches %>% group_by(match_id,Svr) %>% 
    summarise(unRetSvr=sum(isUnret)) %>% 
    pivot_wider(names_from = Svr,values_from=unRetSvr,names_sep="",names_prefix='unRetSvr') %>% 
    mutate(unRetDiff=unRetSvr1-unRetSvr2)
  return(df)
}
