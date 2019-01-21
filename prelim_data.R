##
## convert data from samir for app
##
library(tidyverse)
library(readxl)
library(saves)

dimen <- read_excel("./meta/dimension.xlsx")
ind <- read_excel("./meta/indicator.xlsx") %>%
  filter(ready == 1)

# iso24 <- geog %>% filter(is185==0) %>% .[["isono"]]

##
## label names
##
d1_age <- dimen %>%
  tbl_df() %>%
  filter(dim == "age") %>%
  select(code, name) %>%
  rename(ageno = code, age = name) 

d1_bage <- dimen %>%
  tbl_df() %>%
  filter(dim == "bage") %>%
  select(code, name) %>%
  rename(ageno = code, age = name)

d1_sage <- dimen %>%
  tbl_df() %>%
  filter(dim == "sage") %>%
  select(code, name) %>%
  rename(ageno = code, age = name)

d1_sex <- dimen %>%
  tbl_df() %>%
  filter(dim == "sex") %>%
  select(code, name) %>%
  rename(sexno = code, sex = name)

d1_edu <- dimen %>%
  tbl_df() %>%
  filter(dim == "edu") %>%
  select(code, name) %>%
  rename(eduno = code, edu = name)

# # correct for e0
# for(sn in 1:5){
#   d2 <- read_csv(paste0("C:\\Users\\gabel\\Dropbox\\wcde2\\ToGuy V1\\df",sn,"epop.csv"), 
#                  guess_max = 1e6) %>%
#     mutate(edu = ifelse(age<=15 & edu == "e1", "e0", edu))
#   write_csv(x = d2, paste0("C:\\Users\\gabel\\Dropbox\\wcde2\\ToGuy V1\\df",sn,"epop.csv"))
# }
  
#sn=1; i = 1
for(sn in 1:5){
  for(i in 1:length(ind$name)){
    fn <- paste0("ToGuy V5/df",sn,ind$name[i],".csv")
    zf <- unz(description = "C:\\Users\\Guy\\Dropbox\\wcde2\\ToGuy V5.zip", filename = fn)
    
    d1a <- d1_age
    if(ind$bage[i] == 1)
      d1a <- d1_bage
    if(ind$sage[i] == 1)
      d1a <- d1_sage
        
    d1 <- read_csv(zf, guess_max = 1e6, col_types = cols()) %>% 
      distinct() %>%
      select(isono, year, sexno, ageno, eduno, everything()) %>%
      arrange(year, isono, sexno, ageno, eduno)
      
      # #set past data unreliable to NA (iso24 and isreal)
      # if(ind$is201[i]==0 & ind$edu[i]==0)
      #   df1[df1$year<2015 & df1$isono %in% c(iso24,376), ncol(df1)]<-NA
      # if(ind$is201[i]==0 & ind$edu[i]==1)
      #   df1[df1$year<2015 & df1$isono %in% c(iso24,376) & df1$eduno!=0, ncol(df1)]<-NA
      
    d2 <- d1 %>%
      distinct() %>%
      drop_na() %>%
      spread(key = isono, value = names(d1)[ncol(d1)]) %>%
      left_join(d1a, by = "ageno") %>%
      left_join(d1_sex, by = "sexno") %>%
      left_join(d1_edu, by = "eduno") %>%
      mutate(period = paste(year, year+5, sep = "-")) 
    message(paste0("scenario: ", sn, "  indicator: ", ind$name[i]))
    
    # save for ultra fast loading  
    unlink(x = paste0("df", sn, "/",ind$name[i]), recursive = TRUE)
    if(sn == 1){
      setwd("./df1/")
      assign(ind$name[i], d2)
      saves(list = ind$name[i], file = ind$name[i], overwrite = TRUE, ultra.fast = TRUE)
      rm(list = ind$name[i])
      setwd('..')
    }
    if(sn == 2){
      setwd("./df2/")
      assign(ind$name[i], d2)
      saves(list = ind$name[i], file = ind$name[i], overwrite = TRUE, ultra.fast = TRUE)
      rm(list = ind$name[i])
      setwd('..')
    }
    if(sn == 3){
      setwd("./df3/")
      assign(ind$name[i], d2)
      saves(list = ind$name[i], file = ind$name[i], overwrite = TRUE, ultra.fast = TRUE)
      rm(list = ind$name[i])
      setwd('..')
    }
    if(sn == 4){
      setwd("./df4/")
      assign(ind$name[i], d2)
      saves(list = ind$name[i], file = ind$name[i], overwrite = TRUE, ultra.fast = TRUE)
      rm(list = ind$name[i])
      setwd('..')
    }
    if(sn == 5){
      setwd("./df5/")
      assign(ind$name[i], d2)
      saves(list = ind$name[i], file = ind$name[i], overwrite = TRUE, ultra.fast = TRUE)
      rm(list = ind$name[i])
      setwd('..')
    }
  }
}
