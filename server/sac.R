##
##Stacked area charts
##
#library(googleVis)
#source("./server/sac_fn.R")
#source("./server/pyr_fn.R")
#input<-NULL; input$sac_sn1=2; input$sac_geo1="France"; input$sac_edu=6; input$sac_year1=c(1950,2100); input$sac_prop = TRUE

output$sac_warn1 <- renderUI({
  f <- sac_fill1()
  # can use pyr_warn with sac inputs
  w <- pyr_warn(f = f, year = input$sac_year1[1])
  HTML(w)
})

output$sac_warn2 <- renderUI({
  f <- sac_fill2()
  # can use pyr_warn with sac inputs
  w <- pyr_warn(f = f, year = input$sac_year2[1])
  HTML(w)
})

sac_fill1 <- reactive({
  validate(
    need(input$sac_year1 != "", " "),
    need(input$sac_geo1 != "", " "),
    need(input$sac_sn1 != "", " ")
  )
  # can use pyr_fill with sac inputs
  f <- pyr_fill(year = input$sac_year1[1], edu = input$sac_edu, geo = input$sac_geo1)
  return(f)
})

sac_fill2 <- reactive({
  validate(
    need(input$sac_year2 != "", " "),
    need(input$sac_geo2 != "", " "),
    need(input$sac_sn2 != "", " ")
  )
  # can use pyr_fill with sac inputs
  f <- pyr_fill(year = input$sac_year2[1], edu = input$sac_edu, geo = input$sac_geo2)
  return(f)
})

sac_max1 <- reactive({
  m <- NULL
  d1 <- sac_d1()
  if(input$sac_y == "allarea"){
    d2 <- pyr_d2()
    m <- d1 %>%
      bind_rows(d2) %>%
      max_age_sex()
  }
  return(m)
})

sac_max2 <- reactive({
  m <- NULL
  d2 <- sac_d1()
  if(input$sac_y == "allarea"){
    d1 <- pyr_d1()
    m <- d2 %>%
      bind_rows(d1) %>%
      max_age_sex()
  }
  return(m)
})

sac_d1 <- reactive({
  validate(
    need(input$sac_geo1 != "", " "),
    need(input$sac_sn1 != "", " ")
  )
  d <- sac_data(geo = input$sac_geo1, sn = input$sac_sn1, edu = input$sac_edu, 
                year_range = c(input$sac_year1[1], input$sac_year1[2]))
  return(d)
})

sac_d2 <- reactive({
  validate(
    need(input$sac_geo2 != "", " "),
    need(input$sac_sn2 != "", " ")
  )
  d <- sac_data(geo = input$sac_geo2, sn = input$sac_sn2, edu = input$sac_edu,
                year_range = c(input$sac_year2[1], input$sac_year2[2]))
  return(d)
})


output$sac1 <- renderGvis({
  gg <- NULL
  validate(
    need(input$sac_geo1 != "", "Please select Area"),
    need(input$sac_sn1 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Plot', value = 0, {
    m <- sac_max1()
    # m = NULL; f = FALSE
    f <- sac_fill1()
    incProgress(1/3)
    d <- sac_d1()
    incProgress(2/3)
    gg <- sac_gvis(
      df_sac = d,
      pcol = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$sac_edu))),
      no_edu = f,
      pmax = m,
      prop = input$sac_prop,
      legend = "none"
    )
    incProgress(3/3)
  })
  return(gg)
})

output$sac2 <- renderGvis({
  gg <- NULL
  validate(
    need(input$sac_geo2 != "", "Please select Area"),
    need(input$sac_sn2 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Plot', value = 0, {
    m <- sac_max2()
    f <- sac_fill2()
    incProgress(1/3)
    d <- sac_d2()
    incProgress(2/3)
    gg <- sac_gvis(
      df_sac = d,
      pcol = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$sac_edu))),
      no_edu = f,
      pmax = m,
      prop = input$sac_prop,
      legend = "none"
    )
    incProgress(3/3)
  })
  return(gg)
})

output$sac_leg <- renderGvis({
  gg <- leg_gvis(edu = input$sac_edu)
  return(gg)
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
    
    gg$html$caption <- includeHTML("head.html")
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
    
    gg$html$caption<- includeHTML("head.html")
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
