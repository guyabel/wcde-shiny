map_age_choice <- reactive({
  validate(
    need(input$map_ind, " ", "")
  )
  # input$map_ind = ind$fullname[6]
  ch <- ind %>%
    filter(fullname %in% input$map_ind) %>%
    age_choice0()
  # ch <- ifelse(length(ch)==1, NULL, ch)
  return(ch)
})

observe({
  updateSelectizeInput(session, inputId = "map_age", choices = map_age_choice())
})


map_sex_choice <- reactive({
  validate(
    need(input$map_ind, " ", "")
  )
  ch <- ind %>%
    filter(fullname %in% input$map_ind) %>%
    sex_choice1()
  return(ch)
})

map_sex_select <- reactive({
  s <- map_sex_choice()
  if(length(s) == 3)
    s <- s[1]
  return(s)
})

observe({
  updateSelectizeInput(session, inputId = "map_sex", 
                       choices = map_sex_choice(), selected = map_sex_select())
})


map_edu_choice <- reactive({
  validate(
    need(input$map_ind, " ", ""),
    need(input$map_year, " ", "")
  )
  ch <- ind %>%
    filter(fullname %in% input$map_ind) %>%
    edu_choice0()
  return(ch)
})

observe({
  updateSelectizeInput(session, inputId = "map_edu", choices = map_edu_choice())
})


# 
# map_area_choice <- reactive({
#   validate(
#     need(input$map_edu, "", ""),
#     need(input$map_ind, "", "")
#   )
#   ch <- geo2
#   e <- ind %>%
#     filter(fullname %in% input$map_ind) %>%
#     pull(edu)
#   if(e == 1)
#     if(input$map_edu %in% edu1[8:10])
#       ch <- geo4
#   return(ch)
# })
# 
# observe({
#   updateSelectizeInput(session, inputId = "map_area", choices = map_area_choice())
# })


# output$map_sn0 <- renderUI({
#   scenario.s<-sn1[1]
#   scenario.c<-sn1
#   if(length(input$map_ind)>0){
#     if(ind %>% filter(fullname %in% input$map_ind) %>% .[["df2only"]]==1){
#       scenario.c<-sn1[1]
#     }
#   }
#   selectizeInput("map_sn","Scenario", choices = scenario.c, selected=scenario.s, width="100%")
# })

# map_year_min <- reactive({
#   validate(
#     need(input$map_year, " ", ""),
#     need(input$map_ind, " ", "")
#   )
#   p <- ind %>% 
#     filter(fullname %in% input$map_ind) %>% 
#     pull(past)
#   
#   m <- 1950
#   if(p != 1)
#     m <- 2015
#   return(m)
# })
# 
# map_year_label <- reactive({
#   validate(
#     need(input$map_year, " ", ""),
#     need(input$map_ind, " ", "")
#   )
#   p <- ind %>% 
#     filter(fullname %in% input$map_ind) %>% 
#     pull(period)
#   
#   l <- "Time"
#   if(p != 1)
#     l <- "Beginning of 5-Year Period"
#   return(l)
# }
# )
# observe({
#   updateSelectizeInput(session, inputId = "map_year", choices = map_year_min(), label = map_year_label())
# })


output$map_year0 <- renderUI({
  validate(
    need(input$map_ind, " ", "")
  )
  year.min<-2020
  year.l<-"Time"
  if(ind %>% filter(fullname %in% input$map_ind) %>% .[["past"]]==1){
    year.min<-1950
  }
  if(ind %>% filter(fullname %in% input$map_ind) %>% .[["period"]]==1){
    year.l<- "Beginning of 5-Year Period"
  }
  sliderInput("map_year", year.l, min = year.min, max = 2100, value = c(2020), step= 5, sep="", ticks= FALSE, width="100%")
})