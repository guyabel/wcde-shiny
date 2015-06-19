##
##build .RData by scenario
##
rm(list=ls())
gc()
wd<-"C:/Users/gabel/Documents/shiny/wcde"
setwd(wd)
load("label.RData")

#havent run yet

##
##data from samir in df0
##
library("reshape2")
library("dplyr")
library("saves")
iso24 <- geog %>% filter(is171==0) %>% .[["isono"]]

for(sn in 1:7){
  df0<-expand.grid(year=seq(1970,2100,5), isono=geog$isono, ageno=0:21, sexno=0:2, eduno=0:7)
  setwd(paste0(wd,"/df0"))
  for(i in 1:length(ind$name)){
    fn<-paste0("df",sn,ind$name[i],".csv")
    if(fn %in% list.files()){
      df1 <- read.csv(fn, stringsAsFactors=FALSE) %>% distinct() 
      #set past data unreliable to NA (iso24 and isreal)
      if(ind$is195[i]==0 & ind$edu[i]==0)
        df1[df1$year<2010 & df1$isono %in% c(iso24,376), ncol(df1)]<-NA
      if(ind$is195[i]==0 & ind$edu[i]==1)
        df1[df1$year<2010 & df1$isono %in% c(iso24,376) & df1$eduno!=0, ncol(df1)]<-NA
      df0 <- df0 %>% left_join(df1)
      #print(nrow(df0))
      #print(head(subset(df0, isono==4 & ageno<2 & year==1975)))
      #print(head(subset(df0, isono==44 & ageno==0 & year==2045)))
      message(ind$name[i])
    }
  }
  df0 <- df0 %>% arrange(isono, year, sexno, ageno) %>% mutate(year=as.integer(year))
  df0$period<-as.factor(df0$year)
  levels(df0$period)[1:length(yn4)]<-names(yn4)
  df0$area<-as.factor(df0$isono)
  levels(df0$area)<-geog$name[match(as.numeric(levels(df0$area)),geog$isono)]
  df0$age<-as.factor(df0$ageno)
  levels(df0$age)[1:length(age1)]<-names(age1)
  df0$sex<-as.factor(df0$sexno)
  levels(df0$sex)<-sex1
  df0$edu<-as.factor(df0$eduno)
  levels(df0$edu)<-names(edu1)
  df0$bage<-as.factor(df0$ageno)
  levels(df0$bage)[1:length(bage1)]<-names(bage1)
  df0$sage<-as.factor(df0$ageno)
  levels(df0$sage)[1:length(sage1)]<-names(sage1)
  
#   j <- sapply(df0, is.factor)
#   df0[j] <- lapply(df0[j], as.character)
  
  print(head(df0 %>% filter(area=="France")))
  ##
  ##save data
  ##
  #re-run do_label to get pyrmax1...payrmax5 and stsmax1...stsmax5
  setwd(wd)
  unlink(paste0(wd,"/df", sn), recursive = TRUE)
  if(sn==1){
    df1<-df0
    saves(df1, ultra.fast = TRUE)
    rm(df1)
  }
  if(sn==2){
    df2<-df0
    saves(df2, ultra.fast = TRUE)
    rm(df2)
  }
  if(sn==3){
    df3<-df0
    saves(df3, ultra.fast = TRUE)
    rm(df3)
  }
  if(sn==4){
    df4<-df0
    saves(df4, ultra.fast = TRUE)
    rm(df4)
  }
  if(sn==5){
    df5<-df0
    saves(df5, ultra.fast = TRUE)
    rm(df5)
  }
  if(sn==6){
    df6<-df0
    saves(df6, ultra.fast = TRUE)
    rm(df6)
  }
  if(sn==7){
    df7<-df0
    saves(df7, ultra.fast = TRUE)
    rm(df7)
  }
}
