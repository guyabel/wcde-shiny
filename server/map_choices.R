map_age_choice <- reactive({
  validate(
    need(input$map_ind, " ", "")
  )
  ch <- dim_avail %>%
    filter(fullname %in% input$map_ind,
           dim == "age") %>%
    select(label, code) %>%
    deframe() %>%
    as.list()
  return(ch)
})

observe({
  updateSelectizeInput(session, inputId = "map_age", choices = map_age_choice())
})


map_sex_choice <- reactive({
  validate(
    need(input$map_ind, " ", "")
  )
  ch <- dim_avail %>%
    filter(fullname %in% input$map_ind,
           dim == "sex") %>%
    select(label, code) %>%
    deframe() %>%
    as.list()
  return(ch)
})

# map_sex_select <- reactive({
#   s <- map_sex_choice()
#   if(length(s) > 1)
#     s <- s[1]
#   return(s)
# })

observe({
  updateSelectizeInput(session, inputId = "map_sex", choices = map_sex_choice())
})


map_edu_choice <- reactive({
  validate(
    need(input$map_ind, " ", ""),
  )
  ch <- dim_avail %>%
    filter(fullname %in% input$map_ind,
           dim == "edu") %>%
    select(label, code) %>%
    deframe() %>%
    as.list()
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
  year_min<-2020
  year.l<-"Time"
  if(ind %>% filter(fullname %in% input$map_ind) %>% .[["past"]]==1){
    year_min<-1950
  }
  if(ind %>% filter(fullname %in% input$map_ind) %>% .[["period"]]==1){
    year.l<- "Beginning of 5-Year Period"
  }
  sliderInput("map_year", year.l, min = year_min, max = 2100, value = c(2020), step= 5, sep="", ticks= FALSE, width="100%")
})
