library(tidyverse)
library(leaps)
library(ISLR)
library(MASS)
library(tree)
library(dominanceanalysis)
atpMatchesClean=read_rds('atpMatchesClean.RDS')
#change winner to binary variable 
atpMatchesClean<-atpMatchesClean %>% mutate(winner=ifelse(winner==1,1,0)) %>%
  mutate(winner=factor(winner))

#diff
atpMatchesDiff<-atpMatchesClean %>% 
  dplyr::select(winner,contains('diff'),-c(retFstWondiff,retSndWondiff,pointsDiff,bkSaveDiff)) %>% 
  mutate(winner=factor(winner))

#no diff
linDep<-c('match_id','loser','retFstWon','retSndWon','points','bkPtsSave','netPts_won','bkSave')
atpMatchesNoDiff<-atpMatchesClean %>% 
  dplyr::select(-contains(c('diff',linDep)))

set.seed(10000)

#training index
trainIndex<-sample(1:nrow(atpMatchesDiff),trunc(.8*nrow(atpMatchesDiff)))
#fit models based on method used and data frame used 
models<-function(df,index,method_used){
  train<-df[index,]
  test<-df[-index,]
  regFit<-regsubsets(winner~.,data=train,method = method_used,really.big = T,nvmax=(ncol(df)-1))
  return(regFit)
}
#pick coefficients used 
modelCoefficients<-function(regFit){
  bicResults=data.frame(nVar=1:length(summary(regFit)$cp),BIC=summary(regFit)$bic)
  nCoef<-which(bicResults$BIC==min(bicResults$BIC))
  return(coef(regFit,nCoef))
}
#evaluate based on types of models used 
modelEvaluation<-function(coefs,df,index){
  coefPlus<-str_c(names(coefs[-1]),collapse = '+')
  coefFormula<-str_c('factor(winner)~',coefPlus,collapse = "")
  #fit model
  logModel<-glm(formula(coefFormula),data = df,subset = index,family='binomial')
  ldaModel<-lda(formula(coefFormula),data=df,subset = index)
  qdaModel<-qda(formula(coefFormula),data=df,subset = index)
  treeModel<-tree(formula(coefFormula),data=df,subset = index)
  #predict based on model 
  logPredict<-round(predict(logModel,df[-index,],type='response'))
  ldaPredict<-predict(ldaModel,df[-index,],type='response')$class
  qdaPredict<-predict(qdaModel,df[-index,],type='response')$class
  treePredict<-predict(treeModel,df[-index,],type='class')
  #how many correct 
  logResults<-mean(logPredict==df$winner[-index])
  ldaResults<-mean(ldaPredict==df$winner[-index])
  qdaResults<-mean(qdaPredict==df$winner[-index])
  treeResults<-mean(treePredict==df$winner[-index])
  
  error<-round(1-c(logResults,ldaResults,qdaResults,treeResults),5)
  names(error)<-c('Logistic','LDA','QDA','DecisionTree')
  return(error)
}
#gather coefficients
regFitDiff<-models(atpMatchesDiff,trainIndex,'exhaustive')
regFitNoDiff<-models(atpMatchesNoDiff,trainIndex,'forward')

#find coefficients
coefsDiff<-modelCoefficients(regFitDiff)
coefsNoDiff<-modelCoefficients(regFitNoDiff)

#fit and evaluate models 
coefsDiffResults<-modelEvaluation(coefsDiff,atpMatchesDiff,trainIndex)
coefsNoDiffResults<-modelEvaluation(coefsNoDiff,atpMatchesNoDiff,trainIndex)


coefsDiffResults


#results put together 
results<-rbind(coefsDiffResults,coefsNoDiffResults)
results<-data.frame(nVar=c(8,15),results)
row.names(results)<-c('Difference Only','Individual Stats')
knitr::kable(results)


atpMatchesClean$fstSvPctDiff
#let's look at why serve results are negative
#final model 
atpMatchesClean$fstSvPctDiff
model1<-glm(winner~fstSvPctDiff+winDiff+sndSvWonDiff+unRetDiff+UEdiff+forcedPointsDiff,data=atpMatchesDiff,subset = trainIndex,family = 'binomial')
summary(model1)
pred<-round(predict(model1,atpMatchesClean[-trainIndex,],type = 'response'))
1-mean(pred==atpMatchesNoDiff$winner[-trainIndex])
coefsFinal<-coef(model1)

coefsFinalResults<-modelEvaluation(coefsFinal,atpMatchesDiff,trainIndex)
coefsFinalResults

#variable importance
formDiff<-str_c('winner~',str_c(names(coefsFinal)[-c(1)],collapse = '+'),collapse = "") %>% formula()
modDiff<-glm(formDiff,data=atpMatchesDiff[trainIndex,],family = 'binomial')
dapres<-dominanceAnalysis(modDiff)
x=dapres$contribution.average[[1]]
df<-data.frame(stats=names(x),r2.m=x)
df %>% mutate(stats=fct_reorder(stats,r2.m)) %>% 
  ggplot(aes(x=stats,y=r2.m))+
  geom_col(color='black',fill='red4')+
  coord_flip()+
  ggtitle('Dominance of Tennis Stats')+
  theme_light()
