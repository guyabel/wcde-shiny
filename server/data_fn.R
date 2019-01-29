# this can work in data choice and map choices
age_choice0 <- function(d){
  ch <- NULL
  if(sum(d$age, d$bage, d$sage) == 0)
    ch <- age1[1]
  
  if(d$age == 1)
    ch <- age1
  if(d$name == "asfr")
    ch <- age1[5:11]
  if(d$name %in% c("prop", "mys"))
    ch <- age1[-(1:4)]

  if(d$bage == 1)
    ch <- bage1[-1]
  if(d$name %in% c("bprop", "bmys"))
    ch <- bage1[-(1:3)]

  if(d$sage == 1)
    ch <- sage1
  return(ch)
}


sex_choice0 <- function(d){
  ch <- sex1
  if(d$sex == 0)
    ch <- sex1[1]
  if(d$name %in% c("assr","eassr",  "e0", "ryl15", "pryl15")) #"prop", "bprop",
    ch <- sex1[-1]
  if(d$name %in% c("tfr", "etfr", "asfr", "macb", "easfr"))
    ch <- sex1[-c(1:2)]
  return(ch)
}

# just for maps
edu_choice0 <- function(d){
  ch <- edu1
  if(d$edu == 0)
    ch <- edu1[1]
  if(d$name %in% c("ggapedu15", "ggapedu25"))
    ch <- edu1[-(1:2)]
  if(d$name %in% c("etfr"))
    ch <- edu1[-2]
  return(ch)
}

