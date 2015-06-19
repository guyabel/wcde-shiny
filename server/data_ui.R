load("label.RData")
##
##reactive ui selections
##
output$data_ind <- renderUI({
  ind.c<-NULL
  if(input$data_cat=="phcs")
    ind.c<-ind1
  if(input$data_cat=="demo")
    ind.c<-ind2
  if(input$data_cat=="si")
    ind.c<-ind3
  selectizeInput('data_ind', 'Indicator', choices = ind.c, selected=ind.c[[1]][1], width="100%")
})

allarea <- reactive({
  c(input$reg,input$nat)
})

output$nat0 <- renderUI({
  n1<-NULL
  if(input$regnat==TRUE){
    if(length(input$reg)>0){
      if(input$reg!="World")
        n1<-c(geog$name[geog$region %in% input$reg],geog$name[geog$continent %in% input$reg])
      if(input$reg=="World")
        n1<-nn1
    }
  }
  if(!is.null(n1))
    n1<-sort(n1)
  selectizeInput("nat", "Country", choices = nn2,  multiple=TRUE, selected=n1, width="100%", 
                 options = list(placeholder = 'Type or click for countries'))
})

output$reg0 <- renderUI({
  reg.c <- geo2
  reg.p<-'Type or click for regions'
  if(length(input$data_ind)>0){
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("net")){
      reg.c <- NULL
      reg.p<-'Not available'
    }
  }
  selectizeInput("reg", "Region", choices = reg.c,  multiple=TRUE, width="100%", #selected=geo2[[1]][1], 
                 options = list(placeholder = reg.p))
})




output$age0 <- renderUI({
  age.s<-NULL
  age.c<-NULL
  age.p<-'Not available'
  if(length(input$data_ind)>0){
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["age"]]==1){
      age.s<-age1[1]
      age.c<-age1
      age.p<-'Type or click for multiple options'
      if(input$allage==TRUE)
        age.s<-age1
    }
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["bage"]]==1){
      age.s<-bage1[1]
      age.c<-bage1
      if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("bprop"))
         age.c<-bage1[-(2:3)] 
      age.p<-'Type or click for multiple options'
      if(input$allage==TRUE)
        age.s<-bage1
    }
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["sage"]]==1){
      age.s<-sage1[2]
      age.c<-sage1
      age.p<-'Type or click for multiple options'
      if(input$allage==TRUE)
        age.s<-sage1
    }
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]]=="asfr"){
      age.s<-age1[5]
      age.c<-age1[5:11]
      age.p<-'Type or click for multiple options'
      if(input$allage==TRUE)
        age.s<-age1[5:11]
    }
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]]=="prop"){
      age.s<-age1[5]
      age.c<-age1[-(1:4)]
      age.p<-'Type or click for multiple options'
      if(input$allage==TRUE)
        age.s<-age1[-(1:4)]
    }
  }
  selectizeInput("age","Age", choices = age.c, multiple=TRUE, selected=age.s, width="100%", options = list(placeholder = age.p))
})

output$sex0 <- renderUI({
  sex.s<-NULL
  sex.c<-NULL
  sex.p<-'Not available'
  if(length(input$data_ind)>0){
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["sex"]]==1){
      sex.s<-sex1[[1]]
      sex.c<-sex1
      sex.p<-'Type or click for multiple options'
    }
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("assr","eassr")){
      sex.c<-sex1[-1]
      sex.s<-sex.c
      sex.p<-'Type or click for multiple options'
    }
  }
  selectizeInput("sex","Sex", choices = sex.c, multiple=TRUE, selected=sex.s, width="100%", options = list(placeholder = sex.p))
})

output$scenario0 <- renderUI({
  scenario.s<-sn1[1]
  scenario.c<-sn1
  scenario.p<-'Type or click for multiple options'
  if(length(input$data_ind)>0){
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["df2only"]]==1){
      scenario.c<-sn1[1]
    }
  }
  selectizeInput("scenario","Scenario", choices = scenario.c, multiple=TRUE, selected=scenario.s, width="100%", 
                 options = list(placeholder = scenario.p))
})


output$year0 <- renderUI({
  if(length(input$data_ind)>0){
    year.p<-'Type or click for multiple options'
    #years
    year.l<-"Year"
    year.s<-yn1[1]
    year.c<-yn1
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["past"]]==1)
      year.c<-yn3
    if(input$allyear==TRUE)
      year.s<-year.c
    #periods
    if(ind %>% filter(fullname %in% input$data_ind) %>% .[["period"]]==1){
      year.l<-"Period"
      year.c<-yn2
      if(ind %>% filter(fullname %in% input$data_ind) %>% .[["past"]]==1)
        year.c<-yn4
      if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]]=="asfr")
        year.c<-yn4[-(1:5)]
      year.s<-yn2[1]
      
        
      if(input$allyear==TRUE)
        year.s<-year.c
      
      
    }
    selectizeInput("year",year.l, choices = as.list(year.c), selected=year.s, multiple=TRUE, width="100%", options = list(placeholder = year.p))
  }
})