##
##Stacked area charts
##
#library(googleVis)
#source("./server/sac_fn.R")
#source("./server/pyr_fn.R")
#input<-NULL; input$sac_sn1=2; input$sac_geo1="France"; input$sac_edu=8; input$sac_year1=c(1950,2100); input$sac_prop = TRUE

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
    d2 <- sac_d2()
    m <- d1 %>%
      bind_rows(d2) %>%
      max_total()
  }
  return(m)
})

sac_max2 <- reactive({
  m <- NULL
  d2 <- sac_d2()
  if(input$sac_y == "allarea"){
    d1 <- sac_d1()
    m <- d2 %>%
      bind_rows(d1) %>%
      max_total()
  }
  return(m)
})

sac_d1 <- reactive({
  validate(
    need(input$sac_geo1 != "", " "),
    need(input$sac_sn1 != "", " "),
    need(input$sac_year1 != "", " ")
  )
  d <- sac_data(geo = input$sac_geo1, sn = input$sac_sn1, edu = input$sac_edu, 
                year_range = c(input$sac_year1[1], input$sac_year1[2]))
  return(d)
})

sac_d2 <- reactive({
  validate(
    need(input$sac_geo2 != "", " "),
    need(input$sac_sn2 != "", " "),
    need(input$sac_year2 != "", " ")
  )
  d <- sac_data(geo = input$sac_geo2, sn = input$sac_sn2, edu = input$sac_edu,
                year_range = c(input$sac_year2[1], input$sac_year2[2]))
  return(d)
})


output$sac1 <- renderGvis({
  gg <- NULL
  validate(
    need(input$sac_geo1 != "", "Please select Area"),
    need(input$sac_sn1 != "", "Please select Scenario"),
    need(input$sac_year1 != "", " ")
  )
  withProgress(message = 'Loading Left Plot', value = 0, {
    m <- sac_max1()
    # m = NULL; f = FALSE
    f <- sac_fill1()
    incProgress(1/3)
    d <- sac_d1()
    incProgress(2/3)
    gg <- sac_gvis(
      d_sac = d,
      pcol = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$sac_edu))),
      no_edu = f,
      pmax = m,
      prop = input$sac_prop
    )
    incProgress(3/3)
  })
  return(gg)
})

output$sac2 <- renderGvis({
  gg <- NULL
  validate(
    need(input$sac_geo2 != "", "Please select Area"),
    need(input$sac_sn2 != "", "Please select Scenario"),
    need(input$sac_year2 != "", " ")
  )
  withProgress(message = 'Loading Left Plot', value = 0, {
    m <- sac_max2()
    f <- sac_fill2()
    incProgress(1/3)
    d <- sac_d2()
    incProgress(2/3)
    gg <- sac_gvis(
      d_sac = d,
      pcol = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$sac_edu))),
      no_edu = f,
      pmax = m,
      prop = input$sac_prop
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
    paste0("pop_",
           tolower(input$sac_geo1), "_", input$sac_year1, "_s", input$sac_sn1, "_e", input$sac_edu, ".",
           if(input$sac_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    f <- sac_fill1()
    m <- sac_max1()
    gg <- sac_gvis(
      d_sac =  sac_d1(),
      pcol = ifelse(test = f, yes = "['darkgrey']", 
                    no = get(paste0("iiasa",input$sac_edu))),
      no_edu = f,
      pmax = m,
      prop = input$sac_prop,
      dl = TRUE
    )
    
    tdir = tempdir()
    dir.create(tdir, showWarnings = FALSE)
    temp_gg <- tempfile(pattern = "wcde_v2_", tmpdir = tdir, fileext = ".html")
    temp_img <- tempfile(pattern = "wcde_v2_", tmpdir = tdir, 
                         fileext = paste0(".", input$sac_dl))
    
    gg$html$caption <- dl_head(year = input$sac_year1, scenario = input$sac_sn1, geo = input$sac_geo1, type = "sac")
    print(gg, file = temp_gg)
    
    webshot(
      url = temp_gg, 
      file = temp_img, 
      delay = 2
    )
    file.copy(temp_img, file)
    file.remove(temp_gg)
    file.remove(temp_img)
  }
)

output$sac2_dl <- downloadHandler(
  filename = function() {
    # paste0("wic_sac.", if(input$sac_dl=="pdf") 'pdf' else 'png')
    paste0("pop_",
           tolower(input$sac_geo2), "_", input$sac_year2, "_s", 
           input$sac_sn2, "_e", input$sac_edu, ".",
           if(input$sac_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    f <- sac_fill2()
    m <- sac_max2()
    gg <- sac_gvis(
      d_sac =  sac_d2(),
      pcol = ifelse(test = f, yes = "['darkgrey']", 
                    no = get(paste0("iiasa",input$sac_edu))),
      no_edu = f,
      pmax = m,
      prop = input$sac_prop,
      dl = TRUE
    )

    tdir = tempdir()
    dir.create(tdir, showWarnings = FALSE)
    temp_gg <- tempfile(pattern = "wcde_v2_", tmpdir = tdir, fileext = ".html")
    temp_img <- tempfile(pattern = "wcde_v2_", tmpdir = tdir, 
                         fileext = paste0(".", input$sac_dl))
    
    gg$html$caption <- dl_head(year = input$sac_year2, scenario = input$sac_sn2, geo = input$sac_geo2, type = "sac")
    print(gg, file = temp_gg)
    
    webshot(
      url = temp_gg, 
      file = temp_img, 
      delay = 2
    )
    file.copy(temp_img, file)
    file.remove(temp_gg)
    file.remove(temp_img)
  }
)
