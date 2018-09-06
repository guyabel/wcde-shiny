# setwd("E:/VID/project/wcde/")
rm(list=ls())
library(tidyverse)
library(readxl)

##
##copy about
##
# file.copy(from = "C:/Users/gabel/Dropbox/wicdata/about_details.md", to = "C:/Users/gabel/Documents/shiny/wcde/about_details.md", overwrite = TRUE)

##
##Indicators
##

library("xlsx")
#ind<-read.csv("C:/Users/gabel/Dropbox/wicdata/indicator.csv", stringsAsFactors=FALSE , sep="|")
# ind<-read.xlsx("C:/Users/gabel/Dropbox/wicdata/indicator.xlsx", stringsAsFactors=FALSE , sheetIndex=1)
ind <- read.xlsx("./meta/indicator.xlsx", stringsAsFactors=FALSE , sheetIndex=1) %>%
  tbl_df()

ind0<-as.list(unique(ind$type))
names(ind0)<-unique(ind$type1)


ind1<-as.list(unique(ind$type2[ind$type=="phcs"]))
names(ind1)<-unique(ind$type2[ind$type=="phcs"])
for(i in names(ind1))
  ind1[[i]]<-subset(ind, type2==i)$fullname

ind2<-as.list(unique(ind$type2[ind$type=="demo"]))
names(ind2)<-unique(ind$type2[ind$type=="demo"])
for(i in names(ind2))
  ind2[[i]]<-subset(ind, type2==i)$fullname
ind2$Migration<-c(ind2$Migration, " ")

ind3<-as.list(unique(ind$type2[ind$type=="si"]))
names(ind3)<-unique(ind$type2[ind$type=="si"])
for(i in names(ind3))
  ind3[[i]]<-subset(ind, type2==i)$fullname

ind4<-c(ind1,ind2,ind3)

##
##geography
##
geog <- read_excel("./meta/geography.xlsx") %>%
  tbl_df() %>%
  mutate(isono = as.integer(isono),
         ggarea = ifelse(str_detect(string = ggarea, pattern = "[:digit:]"), 
                         str_pad(string = ggarea, width = 3, pad = "0"),
                         ggarea))

an1 <- geog %>%
  filter(dim == "area") %>%
  pull(name)

rn1 <- geog %>%
  filter(dim == "region") %>%
  pull(name)

nn1 <- geog %>%
  filter(dim == "country") %>%
  pull(name)

nn2 <- as.list(an1[-1])
names(nn2) <- an1[-1]
for(i in an1[-1]){
  nn2[[i]] <- subset(geog, continent==i)$name
}

geo1<-c(list("World","Continent","Region"),nn2)
names(geo1)[2:3]<-geo1[2:3]
geo1[[2]]<-an1[-1]
geo1[[3]]<-rn1

geo2<-list("World","Continent","Region")
names(geo2)<-geo2
geo2[[2]]<-an1[-1]
geo2[[3]]<-rn1

geo3<-geo1
geo3[[3]]<-NULL

nn3<-as.list(an1[-1])
names(nn3)<-an1[-1]
for(i in an1[-1]){
  nn3[[i]]<-subset(geog, continent==i & is185==1)$name
}
geo3<-c(list("World","Continent"),nn3)
names(geo3)[2]<-geo3[2]
geo3[[2]]<-an1[-1]



##
##dimensions
##
dimen <- read.xlsx("./meta/dimension.xlsx", stringsAsFactors=FALSE , sheetIndex=1) %>%
  tbl_df()

sn1<-as.list(subset(dimen, dim=="scenario")$code)
names(sn1)<-subset(dimen, dim=="scenario")$name

yn1<-seq(2015, 2100,5)
yn2<-as.list(yn1)[-length(yn1)]
names(yn2)<-paste0(yn1[-length(yn1)],"-",substr(yn1[-length(yn1)]+5,3,4))

yn3<-seq(1950, 2100,5)
yn4<-as.list(yn3)[-length(yn1)]
names(yn4)<-paste0(yn3[-length(yn1)],"-",substr(yn3[-length(yn1)]+5,3,4))


age1<-as.list(subset(dimen, dim=="age")$code)
names(age1) <- subset(dimen, dim=="age")$name 
sex1<-as.list(0:2)
names(sex1) <- subset(dimen, dim=="sex")$name 
edu1<-as.list(0:10)
names(edu1) <- subset(dimen, dim=="edu")$name 

edu2<-edu1
edu2[4]<-edu2[5]; 
edu2[6]<-edu2[7];
names(edu2)[4]<-names(edu2)[5]
names(edu2)[7]<-gsub("Upper ","",names(edu2)[7])
names(edu2)[6]<-names(edu2)[7]


age2<-list("Five Year Age Groups"=subset(dimen, dim=="age")$name,"Other Age Groups"=subset(dimen, dim=="bage")$name)
age3<-list("Five Year Age Groups"=subset(dimen, dim=="age")$name,"Other Age Groups"=subset(dimen, dim=="sage")$name)

sage1<-as.list(subset(dimen, dim=="sage")$code)
names(sage1) <- subset(dimen, dim=="sage")$name 

bage1<-as.list(subset(dimen, dim=="bage")$code)
names(bage1) <- subset(dimen, dim=="bage")$name 

# age3<-as.list(0:9)
# names(age3) <- subset(dimen, dim=="bage")$name 

  
##
##assumptions
##
assump <- read.xlsx("./meta/assumption.xlsx", stringsAsFactors=FALSE , sheetIndex=1) %>%
  tbl_df()
#assump$country<-NULL
# assump<-assump %>% left_join(geog %>% select(name,isono))
# assump<-assump %>% rename(country=name)
head(assump)

##
##info
##
csvinfo<-
"Source: Wittgenstein Centre for Demography and Global Human Capital (2015). Wittgenstein Centre Data Explorer Version 1.2.
Available at: www.wittgensteincentre.org/dataexplorer \n\n"

pdfinfo<-
"<br> Source: Wittgenstein Centre for Demography and Global Human Capital, (2015). <i> Wittgenstein Centre Data Explorer Version 1.2.</i> <br>
Available at: <a href='www.wittgensteincentre.org/dataexplorer'>www.wittgensteincentre.org/dataexplorer</a> <br><br>"

iiasa4<-paste0("['lightgrey','",
                 rgb(192,0,0, max=255),"','",
                 #rgb(255,102,0, max=255),"','",
                 rgb(255,192,0, max=255),"','",
                 #rgb(153,204,255, max=255),"','",
                 rgb(0,102,255, max=255),"','",
                 rgb(0,0,153, max=255),"']")

iiasa6<-paste0("['lightgrey','",
                  rgb(192,0,0, max=255),"','",
                  rgb(255,102,0, max=255),"','",
                  rgb(255,192,0, max=255),"','",
                  rgb(153,204,255, max=255),"','",
                  rgb(0,102,255, max=255),"','",
                  rgb(0,0,153, max=255),"']")

#iiasacol6<-c("lightgrey", rgb(192,0,0, max=255), rgb(255,102,0, max=255), rgb(255,192,0, max=255), rgb(153,204,255, max=255), rgb(0,102,255, max=255), rgb(0,0,153, max=255))

##
##faq
##
faq <- read.xlsx("./meta/faq.xlsx", stringsAsFactors=FALSE , sheetIndex=1)

##
##saving
##
rm(i)

save.image(file="./label.RData")


