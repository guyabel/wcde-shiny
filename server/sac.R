##
##Stacked area charts
##
#library(googleVis)
#source("./server/sac_fn.R")
#input<-NULL; input$sac_sn1=2; input$sac_geo1="France"; input$sac_edu=6; input$sac_year1=c(1950,2100); input$sac_prop = TRUE

output$sac_warn1 <- renderUI({
  tt<-""
  validate(
    need(input$sac_geo1 != "", " "),
    need(input$sac_sn1 != "", " ")
  )
  df1 <- geog %>% filter(name %in% input$sac_geo1)
  if(df1$dim=="country" & df1$is185 == 0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})
output$sac_warn2 <- renderUI({
  tt<-""
  validate(
    need(input$sac_geo2 != "", " "),
    need(input$sac_sn2 != "", " ")
  )
  df1 <- geog %>% filter(name %in% input$sac_geo2)
  if(df1$dim=="country" & df1$is185 == 0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})

sac_max <- reactive({
  max1 <- max2 <- NULL
  if(input$sac_y=="allarea"){
    df_sac1 <- sac_data(geo = input$sac_geo1, 
                        sn = input$sac_sn1,
                        edu = input$sac_edu,
                        year_range = c(input$sac_year1[1], input$sac_year1[2]))
    
    df_sac2 <- sac_data(geo = input$sac_geo1, 
                        sn = input$sac_sn1,
                        edu = input$sac_edu,
                        year_range = c(input$sac_year2[1], input$sac_year2[2]))
    
    max1 <- df_sac1 %>% 
      filter(ageno != 0, sexno != 0, eduno == 0) %>% 
      pull(pop) %>%
      max(., na.rm = TRUE)
    
    max2 <- df_sac2 %>% 
      filter(ageno != 0, sexno != 0, eduno == 0) %>% 
      pull(pop) %>%
      max(., na.rm = TRUE)
    
    max1 <- max2 <- max(max1, max2)
  }
  return(list(max1 = max1, max2 = max2))
})


output$sac1<- renderGvis({
  gg<-NULL
  validate(
    need(input$sac_geo1 != "", "Please select Area"),
    need(input$sac_sn1 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Plot', value = 0, {
    incProgress(1/4)
    df_sac1 <- sac_data(geo = input$sac_geo1, 
                        sn = input$sac_sn1,
                        edu = input$sac_edu, 
                        year_range = c(input$sac_year1[1], input$sac_year1[2]))
    
    incProgress(2/4)
    max1 <- sac_max()$max1
    
    incProgress(3/4)
    gg <- sac_gvis(df_sac = df_sac1, 
                   pcol = get(paste0("iiasa",input$sac_edu)), 
                   w = 300, legend="none", 
                   pmax = max1,
                   prop = input$sac_prop)

    # plot(gg)
    incProgress(4/4)
  })
  return(gg)
})

output$sac2<- renderGvis({
  gg<-NULL
  validate(
    need(input$sac_geo2 != "", "Please select Area"),
    need(input$sac_sn2 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Right Plot', value = 0, {
    incProgress(1/4)
    df_sac1 <- sac_data(geo = input$sac_geo2, 
                        sn = input$sac_sn2,
                        edu = input$sac_edu, 
                        year_range = c(input$sac_year2[1], input$sac_year2[2]))
    
    incProgress(2/4)
    max2 <- sac_max()$max2
    
    incProgress(3/4)
    gg <- sac_gvis(df_sac = df_sac1, 
                   pcol = get(paste0("iiasa",input$sac_edu)), 
                   w = 300, legend="none", 
                   pmax = max2,
                   prop = input$sac_prop)
    
    # plot(gg)
    incProgress(4/4)
  })
  return(gg)
})

output$sac_leg<- renderGvis({
  if(input$sac_edu==4){
    leg <- edu4 %>%
      leg_data()
    w <- 600
  }
  if(input$sac_edu==6){
    leg <- edu6 %>%
      leg_data()
    w <- 900
  }
  if(input$sac_edu==10){
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


output$sac1_dl <- downloadHandler(
  filename = function() { 
    paste0('wic_sac.', if(input$sac_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    tt <-"Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information"
    fh <- file("head.html", "w")
    cat(pdfinfo, file = fh)
    cat(paste0("Population (000's)","<br>\n"), file = fh)
    cat(paste0(input$sac_geo1, "<br>\n"), file = fh)
    cat(paste0(dimen %>% filter(dim=="scenario", code==input$sac_sn1) %>% .[["name"]], "<br>\n<br>\n"), file = fh)
    cat(ifelse(geog %>% filter(name %in% input$sac_geo1) %>% .[["is171"]] %in% 0, paste0(tt,"<br>\n<br>\n"), ""), file = fh)
    close(fh)
    
    df_sac1 <- sac_data(geo = input$sac_geo1, 
                        sn = input$sac_sn1,
                        edu = input$sac_edu, 
                        year_range = c(input$sac_year1[1], input$sac_year1[2]))
    
    gg <- sac_gvis(df_sac = df_sac1, 
                   pcol = get(paste0("iiasa",input$sac_edu)), 
                   w = 300, legend="none", 
                   pmax = sac_max()$max1,
                   prop = input$sac_prop)
    
    gg$html$caption<-readLines("head.html")
    print(gg, file="gg.html")
    
    if(input$sac_dl=="pdf"){
      system("wkhtmltopdf   --enable-javascript --javascript-delay 2000 gg.html gg.pdf")
      file.copy("./gg.pdf", file)
      file.remove("gg.pdf")
    }
    if(input$sac_dl=="png"){
      system("wkhtmltoimage --enable-javascript --javascript-delay 2000 gg.html gg.png")
      file.copy("./gg.png", file)
      file.remove("gg.png")
    }
    file.remove("gg.html")
    file.remove("head.html")
  }
)

output$sac2_dl <- downloadHandler(
  filename = function() { 
    paste0('wic_sac.', if(input$sac_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    tt <-"Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information"
    fh <- file("head.html", "w")
    cat(pdfinfo, file = fh)
    cat(paste0("Population (000's)","<br>\n"), file = fh)
    cat(paste0(input$sac_geo2, "<br>\n"), file = fh)
    cat(paste0(dimen %>% filter(dim=="scenario", code==input$sac_sn2) %>% .[["name"]], "<br>\n<br>\n"), file = fh)
    cat(ifelse(geog %>% filter(name %in% input$sac_geo2) %>% .[["is171"]] %in% 0, paste0(tt,"<br>\n<br>\n"), ""), file = fh)
    close(fh)
    
    df_sac1 <- sac_data(geo = input$sac_geo2, 
                        sn = input$sac_sn2,
                        edu = input$sac_edu, 
                        year_range = c(input$sac_year2[1], input$sac_year2[2]))
    
    gg <- sac_gvis(df_sac = df_sac1, 
                   pcol = get(paste0("iiasa",input$sac_edu)), 
                   w = 300, legend="none", 
                   pmax = sac_max()$max2,
                   prop = input$sac_prop)
    
    gg$html$caption<-readLines("head.html")
    print(gg, file="gg.html")
    
    if(input$sac_dl=="pdf"){
      system("wkhtmltopdf   --enable-javascript --javascript-delay 2000 gg.html gg.pdf")
      file.copy("./gg.pdf", file)
      file.remove("gg.pdf")
    }
    if(input$sac_dl=="png"){
      system("wkhtmltoimage --enable-javascript --javascript-delay 2000 gg.html gg.png")
      file.copy("./gg.png", file)
      file.remove("gg.png")
    }
    file.remove("gg.html")
    file.remove("head.html")
  }
)
