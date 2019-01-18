# library(googleVis)
# library(saves)

# setwd("E:/VID/project/wcde")
# load("label.RData")
# source("server/pyr_fn.R")
# input<-NULL; input$pyr_sn1<-2; input$pyr_geo1="France"; input$pyr_year1=2020; input$pyr_sn2<-2; input$pyr_geo2="Germany"; input$pyr_edu=6; input$pyr_dl="png"; input$pyr_prop = FALSE

# output$temp <- renderPrint({
#   list(max1=max1,max2=max2)
#   pyr_max()
# })

output$pyr_warn1 <- renderUI({
  tt<-""
  validate(
    need(input$pyr_geo1 != "", " "),
    need(input$pyr_sn1 != "", " ")
  )
  df1 <- geog %>% 
    filter(name %in% input$pyr_geo1)
  if(df1$dim=="country" & df1$is185==0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})
output$pyr_warn2 <- renderUI({
  tt<-""
  validate(
    need(input$pyr_geo2 != "", " "),
    need(input$pyr_sn2 != "", " ")
  )
  df1 <- geog %>% 
    filter(name %in% input$pyr_geo2)
  if(df1$dim=="country" & df1$is185==0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})

pyr_max <- reactive({
  m1 <- m2 <- NULL
  if(input$pyr_x == "allyear"){
    m1 <- d1 %>% 
      max_age_sex() %>%
      pull(pop) %>%
      max(., na.rm=TRUE)
    
    m2 <- d2 %>% 
      max_age_sex() %>%
      pull(pop) %>%
      max(., na.rm=TRUE)
  }
  if(input$pyr_x=="allarea"){
    m1 <- m2 <- d1 %>% 
      bind_rows(d2) %>%
      max_age_sex() 
  }
  return(list(max1 = m1, max2 = m2))
})

pyr_fill <- reactive({
  p1 <- p2 <- FALSE
  if(input$pyr_year1 < 2015 & input$pyr_edu == 8){
    p1 <- TRUE
  }
  if(input$pyr_year2 < 2015 & input$pyr_edu == 8){
    p2 <- TRUE
  }
  n1 <- geog %>% 
    filter(name %in% input$pyr_geo1) %>%
    pull(edu8)
  if(n1 == 0 & input$pyr_edu == 8){
    p1 <- TRUE
  }
  n2 <- geog %>% 
    filter(name %in% input$pyr_geo2) %>%
    pull(edu8)
  if(n2 == 0 & input$pyr_edu == 8){
    p1 <- TRUE
  }
  return(list(p1 = p1, p2 = p2))
})

d1 <- reactive({
  d <- pyr_data(geo = input$pyr_geo1, sn = input$pyr_sn1, edu = input$pyr_edu1)
  return(d)
})

d2 <- reactive({
  d <- pyr_data(geo = input$pyr_geo2, sn = input$pyr_sn2, edu = input$pyr_edu2)
  return(d)
})


output$pyr1 <- renderGvis({
  gg<-NULL
  validate(
    need(input$pyr_geo1 != "", "Please select Area"),
    need(input$pyr_sn1 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Pyramid', value = 0, {
    incProgress(1/4)
    m <- pyr_max()
    f <- pyr_fill()
    d <- d1()
    incProgress(2/4)
    gg <- pyr_gvis(
      d_pyr = d, 
      pyr_year = input$pyr_year1, 
      pyr_col = ifelse(test = f$p1, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f$p1,
      pmax = m$max1,
      prop = input$pyr_prop,
      # pyr_col = iiasa4,
      legend = "none"
    )
    incProgress(4/4)
  })
  return(gg)
})


# 
# 
# output$pyr1<- renderGvis({
#   gg <- df_pyr1 <- noedu_pyr1 <- NULL
#   validate(
#     need(input$pyr_geo1 != "", "Please select Area"),
#     need(input$pyr_sn1 != "", "Please select Scenario")
#   )
#   withProgress(message = 'Loading Left Pyramid', value = 0, {
#     incProgress(1/4)
#     x <- pyr_max()$max1
#     df_pyr1 <- pyr_data(geo = input$pyr_geo1, 
#                         sn = input$pyr_sn1,
#                         edu = input$pyr_edu)
#     
#     incProgress(2/4)
#     # noedu_pyr1 <- geog %>% 
#     #   filter(name==input$pyr_geo1) %>% 
#     #   pull(is185) %in% 0 & input$pyr_year1<2015
#     # if(input$pyr_geo1=="Israel" & input$pyr_year1<2015)
#     #   noedu_pyr1 <- TRUE
#     # if(input$pur_edu == 8 & input$pyr_year1<2015)
#     #   noedu_pyr1 <- TRUE
#     max1 <- pyr_max()$max1
#     
#     incProgress(3/4)
#     gg <- pyr_gvis(df_pyr = df_pyr1, 
#                    pyear = input$pyr_year1, 
#                    pcol = ifelse(noedu_pyr1, "['darkgrey']", get(paste0("iiasa",input$pyr_edu))), 
#                    w = 295, legend="none", 
#                    pmax = max1,
#                    # no.edu = noedu_pyr1, 
#                    prop = input$pyr_prop)
#     # plot(gg)
#     incProgress(4/4)
#   })
#   return(gg)
# })


output$pyr2<- renderGvis({
  gg <- df_pyr1 <- noedu_pyr1 <- NULL
  validate(
    need(input$pyr_geo2 != "", "Please select Area"),
    need(input$pyr_sn2 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Right Pyramid', value = 0, {
    incProgress(1/4)
    df_pyr2 <- pyr_data(geo = input$pyr_geo2, 
                        sn = input$pyr_sn2,
                        edu = input$pyr_edu)
    
    incProgress(2/4)
    noedu_pyr2 <- geog %>% 
      filter(name==input$pyr_geo2) %>% 
      pull(is185) %in% 0 & input$pyr_year2 < 2015
    if(input$pyr_geo2 == "Israel" & input$pyr_year2 < 2015)
      noedu_pyr2 <- TRUE
    max2 <- pyr_max()$max2
    
    incProgress(3/4)
    gg <- pyr_gvis(df_pyr = df_pyr2, 
                   pyear = input$pyr_year2, 
                   pcol = ifelse(noedu_pyr2, "['darkgrey']", get(paste0("iiasa",input$pyr_edu))), 
                   w = 295, legend="none", 
                   pmax = max2,
                   # no.edu = noedu_pyr2, 
                   prop = input$pyr_prop)
    # plot(gg)
    incProgress(4/4)
  })
  return(gg)
})


output$pyr_leg<- renderGvis({
  if(input$pyr_edu==4){
    leg <- edu4 %>%
      leg_data()
    w <- 600
  }
  if(input$pyr_edu==6){
    leg <- edu6 %>%
      leg_data()
    w <- 900
  }
  if(input$pyr_edu==8){
    leg <- edu10 %>%
      leg_data()
    w <- 900
  }
  g <- gvisBarChart(leg, xvar = "Total", yvar = names(leg)[-1], 
               options = list(colors = get(paste0("iiasa", input$pyr_edu)), 
                              height = 30, width = w, 
                              legend="{position:'top', textStyle: {fontSize: 12}}",
                              chartArea="{right:'0%',left:'0%',width:'100%',top:'100%',height:'0%'}"))
})


output$pyr1_dl <- downloadHandler(
  filename = function() { 
    paste0('wic_pyr.', if(input$pyr_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    tt <-"Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information"
    fh <- file("head.html", "w")
    cat(pdfinfo, file = fh)
    cat(paste0("Population (000's) Pyramid","<br>\n"), file = fh)
    cat(paste0(input$pyr_geo1, "<br>\n"), file = fh)
    cat(paste0(input$pyr_year1, "<br>"), file = fh)
    cat(paste0(dimen %>% filter(dim=="scenario", code==input$pyr_sn1) %>% .[["name"]], "<br>\n<br>\n"), file = fh)
    cat(ifelse(geog %>% filter(name %in% input$pyr_geo1) %>% .[["is171"]] %in% 0, paste0(tt,"<br>\n<br>\n"), ""), file = fh)
    close(fh)
    
    df_pyr1 <- pyr_data(geo = input$pyr_geo1, 
                        sn = input$pyr_sn1,
                        edu = input$pyr_edu)
    
    noedu_pyr1 <- geog %>% 
      filter(name==input$pyr_geo1) %>% 
      pull(is185) %in% 0 & input$pyr_year1<2015
    if(input$pyr_geo1=="Israel" & input$pyr_year1<2015)
      noedu_pyr1 <<- TRUE
    max1 <- pyr_max()$max1
    
    gg <- pyr_gvis(df_pyr = df_pyr1, 
                   pyear = input$pyr_year1, 
                   pcol = ifelse(noedu_pyr1, "['darkgrey']", get(paste0("iiasa",input$pyr_edu))), 
                   w = 295, legend="none", 
                   pmax = max1,
                   no.edu = noedu_pyr1, 
                   prop = input$pyr_prop)
    
    gg$html$caption <- includeHTML("head.html")
    print(gg, file="gg.html")
    
    if(input$pyr_dl=="pdf"){
      system("wkhtmltopdf   --enable-javascript --javascript-delay 2000 gg.html gg.pdf") #; file.show("gg.pdf")
      file.copy("./gg.pdf", file)
      file.remove("gg.pdf")
    }
    if(input$pyr_dl=="png"){
      system("wkhtmltoimage --enable-javascript --javascript-delay 2000 gg.html gg.png") #; file.show("gg.png")
      file.copy("./gg.png", file)
      file.remove("gg.png")
    }
    file.remove("gg.html")
    file.remove("head.html")
  }
)

output$pyr2_dl <- downloadHandler(
  filename = function() { 
    paste0('wic_pyr.', if(input$pyr_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    tt <-"Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information"
    fh <- file("head.html", "w")
    cat(pdfinfo, file = fh)
    cat(paste0("Population (000's) Pyramid","<br>\n"), file = fh)
    cat(paste0(input$pyr_geo2, "<br>\n"), file = fh)
    cat(paste0(input$pyr_year2, "<br>"), file = fh)
    cat(paste0(dimen %>% 
                 filter(dim=="scenario", code==input$pyr_sn2) %>% 
                 pull(name), "<br>\n<br>\n"), file = fh)
    cat(ifelse(geog %>% 
                 filter(name %in% input$pyr_geo2) %>% 
                 pull(is185) %in% 0, paste0(tt,"<br>\n<br>\n"), ""), file = fh)
    close(fh)
    
    df_pyr2 <- pyr_data(geo = input$pyr_geo2, 
                        sn = input$pyr_sn2,
                        edu = input$pyr_edu)
    
    noedu_pyr2 <- geog %>% 
      filter(name==input$pyr_geo2) %>% 
      pull(is185) %in% 0 & input$pyr_year2<2015
    if(input$pyr_geo2=="Israel" & input$pyr_year2<2015)
      noedu_pyr2 <<- TRUE
    max2 <- pyr_max()$max2
    
    gg <- pyr_gvis(df_pyr = df_pyr2, 
                   pyear = input$pyr_year2, 
                   pcol = ifelse(noedu_pyr2, "['darkgrey']", get(paste0("iiasa",input$pyr_edu))), 
                   w = 295, legend="none", 
                   pmax = max2,
                   no.edu = noedu_pyr2, 
                   prop = input$pyr_prop)
    
    gg$html$caption<-readLines("head.html")
    print(gg, file="gg.html")
    
    if(input$pyr_dl=="pdf"){
      system("wkhtmltopdf   --enable-javascript --javascript-delay 2000 gg.html gg.pdf")
      file.copy("./gg.pdf", file)
      file.remove("gg.pdf")
    }
    if(input$pyr_dl=="png"){
      system("wkhtmltoimage --enable-javascript --javascript-delay 2000 gg.html gg.png")
      file.copy("./gg.png", file)
      file.remove("gg.png")
    }
    file.remove("gg.html")
    file.remove("head.html")
  }
)
