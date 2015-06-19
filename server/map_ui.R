output$map_age0 <- renderUI({
  age.s<-NULL
  age.c<-NULL
  age.p<-'Not available'
  if(length(input$map_ind)>0){
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["age"]]==1){
      age.s<-age1[1]
      age.c<-age1
      age.p<-'Type to select'
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["bage"]]==1){
      age.s<-bage1[1]
      age.c<-bage1
      if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("bprop"))
         age.c<-bage1[-(2:3)] 
      age.p<-'Type to select'
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["sage"]]==1){
      age.s<-sage1[2]
      age.c<-sage1
      age.p<-'Type to select'
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["name"]]=="asfr"){
      age.s<-age1[5]
      age.c<-age1[5:11]
      age.p<-'Type to select'
      if(input$allage==TRUE)
        age.s<-age1[5:11]
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["name"]]=="prop"){
      age.s<-age1[5]
      age.c<-age1[-(1:4)]
      age.p<-'Type to select'
      if(input$allage==TRUE)
        age.s<-age1[-(1:4)]
    }
  }
  selectizeInput("map_age","Age", choices = age.c, selected=age.s, width="100%", options = list(placeholder = age.p))
})

output$map_sex0 <- renderUI({
  sex.s<-NULL
  sex.c<-NULL
  sex.p<-'Not available'
  if(length(input$map_ind)>0){
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["sex"]]==1){
      sex.s<-sex1[1]
      sex.c<-sex1
      sex.p<-'Type to select'
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["name"]] %in% c("assr","eassr")){
      sex.c<-sex1[-1]
      sex.s<-sex.c[1]
      sex.p<-'Type or click for multiple options'
    }
  }
  selectizeInput("map_sex","Sex", choices = sex.c, selected=sex.s, width="100%", options = list(placeholder = sex.p))
})

output$map_edu0 <- renderUI({
  edu.s<-NULL
  edu.c<-NULL
  edu.p<-'Not available'
  if(length(input$map_ind)>0){
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["edu"]]==1){
      edu.s<-edu1[3]
      edu.c<-edu1
      edu.p<-'Type to select'
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["edu2"]]==1){
      edu.s<-edu1[3]
      edu.c<-edu1[-(1:2)]
      if(ind %>% filter(fullname %in% input$map_ind) %>% .[["name"]]=="etfr")
        edu.c<-edu1[-2]
      edu.p<-'Type to select'
    }
  }
  selectizeInput("map_edu","Education", choices = edu.c, selected=edu.s, width="100%", options = list(placeholder = edu.p))
})

output$map_sn0 <- renderUI({
  scenario.s<-sn1[1]
  scenario.c<-sn1
  if(length(input$map_ind)>0){
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["df2only"]]==1){
      scenario.c<-sn1[1]
    }
  }
  selectizeInput("map_sn","Scenario", choices = scenario.c, selected=scenario.s, width="100%")
})

output$map_year0 <- renderUI({
  year.min<-2010
  year.l<-"Time"
  if(length(input$map_ind)>0){
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["past"]]==1){
      year.min<-1970
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["period"]]==1){
      year.l<-"Beginning of 5-Year Period"
    }
    if(ind %>% filter(fullname %in% input$map_ind) %>% .[["name"]]=="asfr"){
      year.min<-1995
    }
  }
  sliderInput("map_year", year.l, min = year.min, max = 2100, value = c(2010), step= 5, sep="", ticks= FALSE, width="100%")
})