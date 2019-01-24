# setwd("E:/VID/project/wcde/")
rm(list=ls())
library(tidyverse)
library(readxl)
library(fs)
library(magrittr)

##
##copy about
##
dir_delete("./meta")
dir_copy(path = "C:\\Users\\Guy\\Dropbox\\wcde2\\meta-to-wcde", 
         new_path = "./meta")
dir_delete("./md")
dir_copy(path = "C:\\Users\\Guy\\Dropbox\\wcde2\\md-to-wcde", 
         new_path = "./md")
# file.copy(from = "C:/Users/gabel/Dropbox/wicdata/about_details.md", to = "C:/Users/gabel/Documents/shiny/wcde/about_details.md", overwrite = TRUE)

##
##Indicators
##

ind <- read_excel("./meta/indicator.xlsx")

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
  replace_na(list(edu8 = 0)) %>%
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

nn2 <- geog %>%
  filter(dim == "country") %>%
  select(continent, name) %$%
  split(.$name, .$continent)

geo1 <- c( list("World", "Continent", "Region"), nn2)
names(geo1)[2:3]<-geo1[2:3]
geo1[[2]]<-an1[-1]
geo1[[3]]<-rn1

geo2<-list("World","Continent","Region")
names(geo2)<-geo2
geo2[[2]]<-an1[-1]
geo2[[3]]<-rn1

geo3<-geo1
geo3[[3]]<-NULL

nn3 <- geog %>%
  filter(dim == "country", 
         is185 == 1) %>%
  select(continent, name) %$%
  split(.$name, .$continent)

geo3 <- c(list("World","Continent"),nn3)
names(geo3)[2]<-geo3[2]
geo3[[2]]<-an1[-1]



##
## dimensions
##
dimen <- read_excel("./meta/dimension.xlsx")

sn1 <- dimen %>%
  filter(dim == "scenario") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()


yn1<-seq(2015, 2100,5)
yn2<-as.list(yn1)[-length(yn1)]
names(yn2)<-paste0(yn1[-length(yn1)],"-",substr(yn1[-length(yn1)]+5,3,4))

yn3<-seq(1950, 2100,5)
yn4<-as.list(yn3)[-length(yn1)]
names(yn4)<-paste0(yn3[-length(yn1)],"-",substr(yn3[-length(yn1)]+5,3,4))


age1 <- dimen %>%
  filter(dim == "age") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()

sex1 <- dimen %>%
  filter(dim == "sex") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()

edu1 <- dimen %>%
  filter(dim == "edu") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()

edu2 <- dimen %>%
  filter(dim == "edu") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()

edu2<-edu1
edu2[4]<-edu2[5]; 
edu2[6]<-edu2[7];
names(edu2)[4]<-names(edu2)[5]
names(edu2)[7]<-gsub("Upper ","",names(edu2)[7])
names(edu2)[6]<-names(edu2)[7]


edu4 <- edu1[c(1, 2, 4, 6, 7)+1]
edu4[[3]] <- 3:4
edu4[[4]] <- 5:6
edu4[[5]] <- 7:10
names(edu4) <- str_remove_all(string = names(edu4), pattern = "Upper ")
edu4 <- c(edu1[1], edu4)

edu4 <- edu4 %>% 
  map_df(~ data_frame(eduno = .x), .id = "edu_name")

edu6 <- edu1[c(1:7)+1]
edu6[[7]] <- 7:10
edu6 <- c(edu1[1], edu6)

edu6 <- edu6 %>% 
  map_df(~ data_frame(eduno = .x), .id = "edu_name")

edu10 <- edu1 %>% 
  map_df(~ data_frame(eduno = .x), .id = "edu_name") 
  # mutate(eduno = ifelse(edu_name == "Post Secondary", NA, eduno))


age2 <- list("Five Year Age Groups" = dimen %>%
               filter(dim=="age") %>%
               pull(name),
             "Other Age Groups" = dimen %>%
               filter(dim=="bage") %>% 
               pull(name))

age3 <- list("Five Year Age Groups" = dimen %>%
               filter(dim=="age") %>%
               pull(name),
             "Other Age Groups" = dimen %>%
               filter(dim=="sage") %>% 
               pull(name))

sage1 <- dimen %>%
  filter(dim == "sage") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()

bage1 <- dimen %>%
  filter(dim == "bage") %>%
  select(code, name) %>%
  {'names<-'(.$code, .$name)} %>%
  as.list()

# age3<-as.list(0:9)
# names(age3) <- subset(dimen, dim=="bage")$name 

  
##
##assumptions
##
assump <- read_excel("./meta/assumption.xlsx")
#assump$country<-NULL
# assump<-assump %>% left_join(geog %>% select(name,isono))
# assump<-assump %>% rename(country=name)
head(assump)

##
##info
##
csvinfo<-
"Source: Wittgenstein Centre for Demography and Global Human Capital (2018). Wittgenstein Centre Data Explorer Version 2.0
Available at: www.wittgensteincentre.org/dataexplorer \n\n"

pdfinfo<-
"<br> Source: Wittgenstein Centre for Demography and Global Human Capital, (2018). <i> Wittgenstein Centre Data Explorer Version 2.0</i> <br>
Available at: <a href='www.wittgensteincentre.org/dataexplorer'>www.wittgensteincentre.org/dataexplorer</a> <br><br>"

iiasa4 <- paste0("['lightgrey','",
                 rgb(202,0,32, max=255),"','",
                 rgb(244,165,130, max=255),"','",
                 rgb(146,197,222, max=255),"','",
                 rgb(5,113,176, max=255),"']")

iiasa6<-paste0("['lightgrey','",
               rgb(178,24,43, max=255),"','",
               rgb(239,138,98, max=255),"','",
               rgb(253,219,199, max=255),"','",
               rgb(209,229,240, max=255),"','",
               rgb(103,169,207, max=255),"','",
               rgb(33,102,172, max=255),"']")

iiasa8<-paste0("['lightgrey','",
               rgb(178,24,43, max=255),"','",
               rgb(214,96,77, max=255),"','",
               rgb(244,165,130, max=255),"','",
               rgb(253,219,199, max=255),"','",
               rgb(209,229,240, max=255),"','",
               rgb(146,197,222, max=255),"','",
               rgb(67,147,195, max=255),"','",
               rgb(33,102,172, max=255),"']")

iiasa8a <- paste0("['lightgrey','",
               rgb(178,24,43, max=255),"','",
               rgb(214,96,77, max=255),"','",
               rgb(244,165,130, max=255),"','",
               rgb(253,219,199, max=255),"','",
               rgb(209,229,240, max=255),"']")


iiasa8b <- paste0("['",
               rgb(146,197,222, max=255),"','",
               rgb(67,147,195, max=255),"','",
               rgb(33,102,172, max=255),"']")

# iiasa4<-paste0("['lightgrey','",
#                  rgb(192,0,0, max=255),"','",
#                  rgb(255,192,0, max=255),"','",
#                  rgb(0,102,255, max=255),"','",
#                  rgb(0,0,153, max=255),"']")
# 
# iiasa6<-paste0("['lightgrey','",
#                   rgb(192,0,0, max=255),"','",
#                   rgb(255,102,0, max=255),"','",
#                   rgb(255,192,0, max=255),"','",
#                   rgb(153,204,255, max=255),"','",
#                   rgb(0,102,255, max=255),"','",
#                   rgb(0,0,153, max=255),"']")

##
##faq
##
faq <- read_excel("./meta/faq.xlsx")

##
##saving
##
rm(i)

save.image(file="./label.RData")

save(ind0, file="./ind0.RData")

##
## check pdf names
##
fn <- list.files("./pdf") %>%
  str_remove_all(pattern = ".pdf")
fn[!fn %in% geog$name]
fn[!fn %in% geog$name]

