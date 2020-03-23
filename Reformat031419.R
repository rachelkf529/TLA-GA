library(tidyverse)
library(stringr)
SearchTerm<-read.csv("C:\\Users\\zjiang17\\Box Sync\\zjiang research\\Search Task Research\\October_7056_deleted_setting.csv")
Nrow<-nrow(SearchTerm)
NewSearchTerm<-rep(NA,Nrow)
#Clean the first letter of each row record
for (i in 1: Nrow){
  target<-as.character(SearchTerm[i,2])
  target<-gsub("\\\\," ,",", target)
  target<-gsub("\\\\." ,".", target)
  target<-gsub("\\," ,",", target)
  target<-gsub("\\." ,".", target)
  target<-gsub("\"" ,"'", target);target<-str_trim(target, side = c("both", "left", "right"))
  noChange<-grep("^[A-z0-9,(\']",target, ignore.case=TRUE)
  if(length(noChange)==0){
    indicator<-c(which(grepl("[A-z0-9,(\']",strsplit(target," ")[[1]]))[1])
    target<-paste(c(strsplit(target," ")[[1]])[indicator:length(strsplit(target," ")[[1]])],collapse = ' ')
  }
  NewSearchTerm[i]<-target
} 

BooleanOper<-c('AND','OR','NOT','TX',
'TI',
'AU',
'AR',
'DE',
'MM',
'ZK',
'SU',
'JN',
'SO',
'SE',
'JC',
'JJ',
'AB',
'LN',
'IS',
'IB',
'PS',
'AN',
'XY')
CountMat<-matrix(0,Nrow,length(BooleanOper))

for (i in 1: Nrow){
  tempTerm<-strsplit(NewSearchTerm[i]," ")[[1]]
  for( j in 1:length(BooleanOper)){
    CountMat[i,j]<-length(which(tempTerm==BooleanOper[j]))  
  }
}

colnames(CountMat)<-BooleanOper
CountMat<-as.data.frame(CountMat)
CountMat$Term<-NewSearchTerm
write.csv(CountMat,"C:\\Users\\zjiang17\\Box Sync\\zjiang research\\Search Task Research\\October_7056_deleted_setting_re.csv")

######################################################################################################################
################################################## 04-01-2019  #######################################################
######################################################################################################################

library(tidyverse)
library(stringr)
SearchTerm<-read.csv("C:\\Users\\zjiang17\\Box Sync\\zjiang research\\Search Task Research\\Oct_nonDelete.csv")
Nrow<-nrow(SearchTerm)
NewSearchTerm<-rep(NA,Nrow)
#Clean the first letter of each row record
for (i in 1: Nrow){
  target<-as.character(SearchTerm[i,2])
  target<-gsub("\\\\," ,",", target)
  target<-gsub("\\\\." ,".", target)
  target<-gsub("\\," ,",", target)
  target<-gsub("\\." ,".", target)
  target<-gsub("\"" ,"'", target);target<-str_trim(target, side = c("both", "left", "right"))
  noChange<-grep("^[A-z0-9,(\']",target, ignore.case=TRUE)
  if(length(noChange)==0){
    indicator<-c(which(grepl("[A-z0-9,(\']",strsplit(target," ")[[1]]))[1])
    target<-paste(c(strsplit(target," ")[[1]])[indicator:length(strsplit(target," ")[[1]])],collapse = ' ')
  }
  NewSearchTerm[i]<-target
}
ResultTable<-matrix(0,Nrow,4)
BooleanOper<-c('AND PT','AND LN','AND DT','AND FT')
for (i in 1:Nrow){
  for(j in 1:4){
    if(length(grep(BooleanOper[j],NewSearchTerm[i]))!=0){
      ResultTable[i,j]<-grep(BooleanOper[j],NewSearchTerm[i])
      
    }
    
  }  }

colnames(ResultTable)<-BooleanOper
newDat<-cbind(c(NewSearchTerm),ResultTable)
write.csv(newDat,"C:\\Users\\zjiang17\\Box Sync\\zjiang research\\Search Task Research\\Oct_nonDelete_re.csv")


