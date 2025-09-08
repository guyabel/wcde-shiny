# library(googleVis)
# library(saves)

# setwd("E:/VID/project/wcde")
# load("label.RData")
# source("server/pyr_fn.R")
# input<-NULL; input$pyr_sn1<-2; input$pyr_geo1="World"; input$pyr_year1=2020; input$pyr_sn2<-2; input$pyr_geo2="Germany"; input$pyr_edu=6; input$pyr_dl="png"; input$pyr_prop = FALSE

pyr_max1 <- reactive({
  m <- NULL
  d1 <- pyr_d1()
  if(input$pyr_x == "allyear"){
    m <- d1 %>%
      max_age_sex()
  }
  if(input$pyr_x == "allarea"){
    d2 <- pyr_d2()
    m <- d1 %>%
      bind_rows(d2) %>%
      max_age_sex()
  }
  return(m)
})

pyr_max2 <- reactive({
  m <- NULL
  d2 <- pyr_d2()
  if(input$pyr_x == "allyear"){
    m <- d2 %>%
      max_age_sex()
  }
  if(input$pyr_x == "allarea"){
    d1 <- pyr_d1()
    m <- d2 %>%
      bind_rows(d1) %>%
      max_age_sex()
  }
  return(m)
})

pyr_fill1 <- reactive({
  validate(
    need(input$pyr_year1 != "", " "),
    need(input$pyr_geo1 != "", " "),
    need(input$pyr_sn1 != "", " ")
  )
  f <- pyr_fill(year = input$pyr_year1, edu = input$pyr_edu, geo = input$pyr_geo1)
  return(f)
})

pyr_fill2 <- reactive({
  validate(
    need(input$pyr_year2 != "", " "),
    need(input$pyr_geo2 != "", " "),
    need(input$pyr_sn2 != "", " ")
  )
  f <- pyr_fill(year = input$pyr_year2, edu = input$pyr_edu, geo = input$pyr_geo2)
  return(f)
})

output$pyr_warn1 <- renderUI({
  f <- pyr_fill1()
  w <- pyr_warn(f = f, year = input$pyr_year1)
  HTML(w)
})

output$pyr_warn2 <- renderUI({
  f <- pyr_fill2()
  w <- pyr_warn(f = f, year = input$pyr_year2)
  HTML(w)
})


pyr_d1 <- reactive({
  validate(
    need(input$pyr_geo1 != "", " "),
    need(input$pyr_sn1 != "", " ")
  )
  # message("calculating d1")
  d <- pyr_data(geo = input$pyr_geo1, sn = input$pyr_sn1, edu = input$pyr_edu)
  return(d)
})

pyr_d2 <- reactive({
  validate(
    need(input$pyr_geo2 != "", " "),
    need(input$pyr_sn2 != "", " ")
  )
  # message("calculating d2")
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
    m <- pyr_max1()
    f <- pyr_fill1()
    # f = TRUE
    incProgress(1/3)
    d <- pyr_d1()
    incProgress(2/3)
    gg <- pyr_gvis(
      d_pyr = d,
      pyr_year = input$pyr_year1,
      pyr_col = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f,
      pmax = m,
      prop = input$pyr_prop
    )
    incProgress(3/3)
  })
  return(gg)
})

output$pyr2 <- renderGvis({
  validate(
    need(input$pyr_geo2 != "", "Please select Area"),
    need(input$pyr_sn2 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Right Pyramid', value = 0, {
    m <- pyr_max2()
    f <- pyr_fill2()
    incProgress(1/3)
    d <- pyr_d2()
    incProgress(2/3)
    gg <- pyr_gvis(
      d_pyr = d,
      pyr_year = input$pyr_year2,
      pyr_col = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f,
      pmax = m,
      prop = input$pyr_prop
    )
    incProgress(3/3)
  })
  return(gg)
})


output$pyr_leg <- renderGvis({
  gg <- leg_gvis(edu = input$pyr_edu)
  # f1 <- pyr_fill1()
  # f2 <- pyr_fill2()
  # if(f1 == TRUE & f2 == TRUE)
  #   gg <- NULL
  return(gg)
})



output$pyr1_dl <- downloadHandler(
  filename = function() {
    # paste0("wic_pyr.", if(input$pyr_dl=="pdf") 'pdf' else 'png')
    paste0("pyr_",
           tolower(input$pyr_geo1), "_", input$pyr_year1, "_s", input$pyr_sn1, "_e", input$pyr_edu, ".",
           if(input$pyr_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    f <- pyr_fill1()
    gg <- pyr_gvis(
      d_pyr =  pyr_d1(),
      pyr_year = input$pyr_year1,
      pyr_col = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f,
      pmax = pyr_max1(),
      prop = input$pyr_prop,
      legend = TRUE,
      edu = input$pyr_edu,
      dl = TRUE
    )
    
    tdir = tempdir()
    dir.create(tdir, showWarnings = FALSE)
    temp_gg <- tempfile(pattern = "wcde_v3_", tmpdir = tdir, fileext = ".html")
    temp_img <- tempfile(pattern = "wcde_v3_", tmpdir = tdir, 
                        fileext = paste0(".", input$pyr_dl))
    
    # generate gg.html
    gg$html$caption <- dl_head(year = input$pyr_year1, scenario = input$pyr_sn1, geo = input$pyr_geo1,
                               type = "pyr")
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


output$pyr2_dl <- downloadHandler(
  filename = function() {
    paste0("pyr_",
           tolower(input$pyr_geo2), "_", input$pyr_year2, "_s", input$pyr_sn2, "_e", input$pyr_edu, ".",
           if(input$pyr_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    f <- pyr_fill2()
    gg <- pyr_gvis(
      d_pyr =  pyr_d2(),
      pyr_year = input$pyr_year2,
      pyr_col = ifelse(test = f, yes = "['darkgrey']", no = get(paste0("iiasa",input$pyr_edu))),
      no_edu = f,
      pmax = pyr_max2(),
      prop = input$pyr_prop,
      legend = TRUE,
      edu = input$pyr_edu,
      dl = TRUE
    )
    
    tdir = tempdir()
    dir.create(tdir, showWarnings = FALSE)
    temp_gg <- tempfile(pattern = "wcde_v3_", tmpdir = tdir, fileext = ".html")
    temp_img <- tempfile(pattern = "wcde_v3_", tmpdir = tdir, 
                         fileext = paste0(".", input$pyr_dl))
    
    # generate gg.html
    gg$html$caption <- dl_head(year = input$pyr_year2, scenario = input$pyr_sn2, geo = input$pyr_geo2,
                               type = "pyr")
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

