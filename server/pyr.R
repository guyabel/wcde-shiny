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

# output$pyr_warn1 <- renderUI({
#   tt<-""
#   validate(
#     need(input$pyr_geo1 != "", " "),
#     need(input$pyr_sn1 != "", " ")
#   )
#   df1 <- geog %>% 
#     filter(name %in% input$pyr_geo1)
#   if(df1$dim=="country" & df1$is185==0)
#     tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
#   HTML(tt)
# })
# output$pyr_warn2 <- renderUI({
#   tt<-""
#   validate(
#     need(input$pyr_geo2 != "", " "),
#     need(input$pyr_sn2 != "", " ")
#   )
#   df1 <- geog %>% 
#     filter(name %in% input$pyr_geo2)
#   if(df1$dim=="country" & df1$is185==0)
#     tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
#   HTML(tt)
# })

pyr_max <- reactive({
  m1 <- m2 <- NULL
  d1 <- pyr_d1()
  d2 <- pyr_d2()
  if(input$pyr_x == "allyear"){
    m1 <- d1 %>% 
      max_age_sex()
    
    m2 <- d2 %>% 
      max_age_sex()
  }
  if(input$pyr_x=="allarea"){
    m1 <- m2 <- d1 %>% 
      bind_rows(d2) %>%
      max_age_sex() 
  }
  return(list(max1 = m1, max2 = m2))
})


pyr_warn1 <- renderUI({
  f <- pyr_fill1()
  w <- pyr_warn(f = f, year = input$pyr_year1)
  HTML(w)
})

pyr_warn2 <- renderUI({
  f <- pyr_fill2()
  pyr_warn(f = f, year = input$pyr_year2)
  HTML(w)
})

pyr_fill1 <- reactive({
  f <- pyr_fill(year = input$pyr_year1, edu = input$pyr_edu, geo = input$pyr_geo1)
  return(f)
})

pyr_fill2 <- reactive({
  f <- pyr_fill(year = input$pyr_year2, edu = input$pyr_edu, geo = input$pyr_geo2)
  return(f)
})

pyr_d1 <- reactive({
  d <- pyr_data(geo = input$pyr_geo1, sn = input$pyr_sn1, edu = input$pyr_edu)
  return(d)
})

pyr_d2 <- reactive({
  d <- pyr_data(geo = input$pyr_geo2, sn = input$pyr_sn2, edu = input$pyr_edu)
  return(d)
})


output$pyr1 <- renderGvis({
  gg <- NULL
  validate(
    need(input$pyr_geo1 != "", "Please select Area"),
    need(input$pyr_sn1 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Pyramid', value = 0, {
    m <- pyr_max()
    f <- pyr_fill1()
    incProgress(1/3)
    d <- pyr_d1()
    incProgress(2/3)
    gg <- pyr_gvis(
      d_pyr = d, 
      pyr_year = input$pyr_year1, 
      pyr_col = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f,
      pmax = m$max1,
      prop = input$pyr_prop,
      legend = "none"
    )
    incProgress(3/3)
  })
  return(gg)
})

output$pyr2 <- renderGvis({
  gg <- NULL
  validate(
    need(input$pyr_geo2 != "", "Please select Area"),
    need(input$pyr_sn2 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Right Pyramid', value = 0, {
    m <- pyr_max()
    f <- pyr_fill2()
    incProgress(1/3)
    d <- pyr_d2()
    incProgress(2/3)
    gg <- pyr_gvis(
      d_pyr = d, 
      pyr_year = input$pyr_year2, 
      pyr_col = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f,
      pmax = m$max2,
      prop = input$pyr_prop,
      legend = "none"
    )
    incProgress(3/3)
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
