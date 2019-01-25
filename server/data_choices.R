##
## indicator
##

ind_choice <- reactive({
  validate(
    need(input$data_cat, label = "indicator category")
  )
  if(input$data_cat=="phcs")
    ch <- ind1
  if(input$data_cat=="demo")
    ch <- ind2
  if(input$data_cat=="si")
    ch <- ind3
  return(ch)
})

observe({
  updateSelectizeInput(session, inputId = "data_ind", choices = ind_choice())
})


##
## nation
##

nat_select <- reactive({
  validate(
    # need(input$data_ind, label = "indicator"),
    # need(input$reg, label = "region"),
    need(input$allnat, label = "all countries in region")
  )
  n <- NULL
  if(input$allnat){
    if(all(input$reg !="World"))
      n <- geog %>%
        filter(region %in% input$reg | continent %in% input$reg) %>%
        pull(name)
    if(any(input$reg == "World"))
      n <- nn1
  }
  if(!is.null(n))
    n <- sort(n)
  return(n)
})

observe({
  updateSelectizeInput(session, inputId = "nat", selected = nat_select())
})


##
## region
##

reg_choice <- reactive({
  cn <- NULL
  validate(
    need(input$data_ind, label = "indicator")
  )

  ch <- geo2
  ind_name <- ind %>%
    filter(fullname %in% input$data_ind) %>%
    pull(name)
  if(ind_name == "net")
    ch <- NULL
  return(ch)
})

observe({
  updateSelectizeInput(session, inputId = "reg",  choice = reg_choice())
})


##
## sex
##

sex_choice <- reactive({
  validate(
    need(input$data_ind, label = "indicator")
  )
  ch <- ind %>%
    filter(fullname %in% input$data_ind) %>%
    sex_choice0()
  return(ch)
})


sex_select <- reactive({
  s <- sex_choice()
  if(length(s) == 3)
    s <- s[1]
  return(s)
})

observe({
  updateSelectizeInput(session, inputId = "sex", choices = sex_choice(), selected = sex_select())
  # options = list(placeholder = sex_placeholder()))
})

# sex_placeholder <- reactive({
#   ph <- 'Not available'
#   validate(
#     need(input$data_ind, label = "indicator")
#   )
#   sex <- ind %>%
#     pull(sex)
#
#   if(sex == 1)
#     ph <- 'Type or click for multiple options'
#   return(ph)
# })

##
## age
##

age_choice <- reactive({
  validate(
    need(input$data_ind, label = "indicator")
  )
  # input$data_ind = ind$fullname[2]
  ch <- ind %>%
    filter(fullname %in% input$data_ind) %>%
    age_choice0()
  return(ch)
})

age_select <- reactive({
  s <- age_choice()[1]
  if(input$allage)
    s <- age_choice()
  return(s)
})

observe({
  updateSelectizeInput(session, inputId = "age", choices = age_choice(), selected = age_select())
})

##
## scenario
##

sn_choice <- reactive({
  ch <- sn1
  validate(
    need(input$data_ind, label = "indicator")
  )
  df2_only <- ind %>%
    filter(fullname %in% input$data_ind) %>%
    pull(df2only)

  if(df2_only == 1)
    ch <- sn1[1]
  return(ch)
})

observe({
  updateSelectizeInput(session, inputId = "scenario", choices = sn_choice(), selected = sn1[1])
})
 
##
## time
##

year_past <- reactive({
  validate(
    need(input$data_ind, label = "indicator")
  )
  x <- ind %>%
    filter(fullname %in% input$data_ind) %>%
    pull(past)
  return(x)
})

year_period <- reactive({
  validate(
    need(input$data_ind, label = "indicator")
  )
  x <- ind %>%
    filter(fullname %in% input$data_ind) %>%
    pull(period)
  return(x)
})

year_label <- reactive({
  validate(
    need(input$data_ind, label = "indicator")
  )
  l <- ifelse(year_period() == 1, "Period", "Year")
  return(l)
})

year_choice <- reactive({
  ch <- as.list(yn1)
  if(year_past() == 1)
    ch <- as.list(yn3)
  if(year_period() == 1)
    ch <- as.list(yn2)
  if(year_period() == 1 & year_past() == 1)
    ch <- as.list(yn4)
  return(ch)
})

year_select <- reactive({
  s <- year_choice()[1]
  if(year_past() == 1 & year_period() == 0)
    s <- year_choice()[14]
  if(year_past() == 1 & year_period() == 1)
    s <- year_choice()[13]
  if(input$allyear == 1)
    s <- year_choice()
  return(s)
})

observe({
  updateSelectizeInput(session, inputId = "year", label = year_label(), 
                       choices = year_choice(), selected = year_select())
})






# ##
# ## old version
# ##
# output$data_ind <- renderUI({
#   ind.c<-NULL
#   if(input$data_cat=="phcs")
#     ind.c<-ind1
#   if(input$data_cat=="demo")
#     ind.c<-ind2
#   if(input$data_cat=="si")
#     ind.c<-ind3
#   selectizeInput('data_ind', 'Indicator', choices = ind.c, selected=ind.c[[1]][1], width="100%")
# })
# 
# output$nat0 <- renderUI({
#   validate(
#     need(input$data_ind, label = "indicator")
#   )
#   n1<-NULL
#   if(input$regnat==TRUE){
#     if(length(input$reg)>0){
#       if(all(input$reg !="World"))
#         n1 <- geog %>%
#           filter(region %in% input$reg | continent %in% input$reg) %>%
#           pull(name)
#       # n1<-c(geog$name[geog$region %in% input$reg],geog$name[geog$continent %in% input$reg])
#       if(any(input$reg == "World"))
#         n1 <- nn1
#     }
#   }
#   if(!is.null(n1))
#     n1<-sort(n1)
#   selectizeInput("nat", "Country", choices = nn2,  multiple=TRUE, selected=n1, width="100%",
#                  options = list(placeholder = 'Type or click for countries'))
# })
# 
# output$reg0 <- renderUI({
#   validate(
#     need(input$data_ind, label = "indicator")
#   )
#   reg.c <- geo2
#   reg.p<-'Type or click for regions'
#   if(length(input$data_ind)>0){
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("net")){
#       reg.c <- NULL
#       reg.p<-'Not available'
#     }
#   }
#   selectizeInput("reg", "Region", choices = reg.c,  multiple=TRUE, width="100%", #selected=geo2[[1]][1],
#                  options = list(placeholder = reg.p))
# })
# 
# 
# output$sex0 <- renderUI({
#   sex.s<-NULL
#   sex.c<-NULL
#   sex.p<-'Not available'
#   if(length(input$data_ind)>0){
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["sex"]]==1){
#       sex.s<-sex1[[1]]
#       sex.c<-sex1
#       sex.p<-'Type or click for multiple options'
#     }
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("assr","eassr")){
#       sex.c<-sex1[-1]
#       sex.s<-sex.c
#       sex.p<-'Type or click for multiple options'
#     }
#   }
#   selectizeInput("sex","Sex", choices = sex.c, multiple=TRUE, selected=sex.s, width="100%", options = list(placeholder = sex.p))
# })
# 
# output$age0 <- renderUI({
#   validate(
#     need(input$data_ind, label = "indicator")
#   )
#   age.s<-NULL
#   age.c<-NULL
#   age.p<-'Not available'
#   if(length(input$data_ind)>0){
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["age"]]==1){
#       age.s<-age1[1]
#       age.c<-age1
#       age.p<-'Type or click for multiple options'
#       if(input$allage==TRUE)
#         age.s<-age1
#     }
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["bage"]]==1){
#       age.s<-bage1[1]
#       age.c<-bage1
#       if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]] %in% c("bprop"))
#         age.c<-bage1[-(2:3)]
#       age.p<-'Type or click for multiple options'
#       if(input$allage==TRUE)
#         age.s<-bage1
#     }
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["sage"]]==1){
#       age.s<-sage1[2]
#       age.c<-sage1
#       age.p<-'Type or click for multiple options'
#       if(input$allage==TRUE)
#         age.s<-sage1
#     }
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]]=="asfr"){
#       age.s<-age1[5]
#       age.c<-age1[5:11]
#       age.p<-'Type or click for multiple options'
#       if(input$allage==TRUE)
#         age.s<-age1[5:11]
#     }
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]]=="prop"){
#       age.s<-age1[5]
#       age.c<-age1[-(1:4)]
#       age.p<-'Type or click for multiple options'
#       if(input$allage==TRUE)
#         age.s<-age1[-(1:4)]
#     }
#   }
#   selectizeInput("age","Age", choices = age.c, multiple=TRUE, selected=age.s, width="100%", options = list(placeholder = age.p))
# })
# 
# output$scenario0 <- renderUI({
#   scenario.s<-sn1[1]
#   scenario.c<-sn1
#   scenario.p<-'Type or click for multiple options'
#   if(length(input$data_ind)>0){
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["df2only"]]==1){
#       scenario.c<-sn1[1]
#     }
#   }
#   selectizeInput("scenario","Scenario", choices = scenario.c, multiple=TRUE, selected=scenario.s, width="100%",
#                  options = list(placeholder = scenario.p))
# })
# 
# output$year0 <- renderUI({
#   if(length(input$data_ind)>0){
#     year.p<-'Type or click for multiple options'
#     #years
#     year.l<-"Year"
#     year.s<-yn1[1]
#     year.c<-yn1
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["past"]]==1)
#       year.c<-yn3
#     if(input$allyear==TRUE)
#       year.s<-year.c
#     #periods
#     if(ind %>% filter(fullname %in% input$data_ind) %>% .[["period"]]==1){
#       year.l<-"Period"
#       year.c<-yn2
#       if(ind %>% filter(fullname %in% input$data_ind) %>% .[["past"]]==1)
#         year.c<-yn4
#       if(ind %>% filter(fullname %in% input$data_ind) %>% .[["name"]]=="asfr")
#         year.c<-yn4[-(1:5)]
#       year.s<-yn2[1]
#       if(input$allyear==TRUE)
#         year.s<-year.c
#     }
#     selectizeInput("year",year.l, choices = as.list(year.c), selected=year.s, multiple=TRUE, width="100%", options = list(placeholder = year.p))
#   }
# })