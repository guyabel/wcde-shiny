# output$temp <- renderPrint({
#   #as.list(input)
#   map_build()
# })


map_geo<-reactive({
  geo <- NULL
  if(input$map_area=="World"){
    geo <- geog %>% filter(dim=="country") %>% .[["name"]]
  }
  if(input$map_area %in% an1[-1]){
    geo <- geog %>% filter(continent==input$map_area) %>% .[["name"]]
  }
  if(input$map_area %in% rn1){
    geo <- geog %>% filter(region==input$map_area) %>% .[["name"]]
  }
  return(geo)
})

#input=NULL;input$map_year=2015; input$map_ind=ind4[[4]][3]; input$map_proj="mercator";input$map_area="Latin America and the Caribbean"; input$map_sn=1; input$map_age="";input$map_sex=1;
#input$map_edu=0; input$map_age = 5; input$map_sex=2;
map_build <- reactive({
  validate( 
    need(input$map_ind != "", "Please select Indicator"), 
    need(input$map_area != "", "Please select Area"), 
    need(input$map_sn != "", "Please select Scenario"),
    need(input$map_age != "", "Please select Age"),
    need(input$map_sex != "", "Please select Sex"),
    need(input$map_edu != "", "Please select Education"),
    need(input$map_year != "", "Please select Years")
  )
  df1<-NULL;df2<-NULL; gg<-NULL
  withProgress(message = 'Loading Map', value = 0, {
    if(length(input$map_sn)>0){
      cn <- ind %>% 
        filter(fullname == input$map_ind) %>%
        pull(cname)
      
      v <- c("year", "ageno", "sexno", "eduno")
      if(input$map_area != "World"){
        v <- geog %>%
          filter(continent == input$map_area | region == input$map_area) %>%
          pull(isono) %>%
          c(v, .)
      }
      if(input$map_area == "World"){
        v <- geog %>%
          filter(dim == "country") %>%
          pull(isono) %>%
          c(v, .)
      }
      
      fn <- ind %>% 
        filter(fullname == input$map_ind) %>%
        pull(name)

      incProgress(1/4)
      df1 <- loads(file = paste0("df",input$map_sn, "/", fn, "/"), 
                   variables = v, ultra.fast = TRUE, to.data.frame=TRUE) %>%
        tbl_df()
      incProgress(2/4)
      d2a <- geog %>% 
        filter(dim == "country") %>%
        select(name, ggarea, isono)
      
      df2 <- df2 %>% 
        # gather(key = isono, value = x, -(1:4), convert = TRUE) %>%
        filter(year == input$map_year,
               ageno == input$map_age,
               sexno == input$map_sex,
               eduno == input$map_edu) %>%
        full_join(d2a, by="isono") %>%
        # replace_na(list(x = 0)) %>%
        rename(!!cn := x)
      
      incProgress(3/4)
      gg <- gvisGeoChart(
        data = df2, 
        locationvar = "ggarea", colorvar = cn, 
        hovervar = "name", chartid = "map",
        options = list(
          width = 1100, height = 600, 
          projection = input$map_proj, 
          dataMode = "regions", 
          region = geog %>% 
            filter(name == input$map_area) %>% 
            pull(ggarea),
          colorAxis = "{colors:['lightgrey', 'dodgerblue']}",
          defaultColor = 'white')
      )
      incProgress(4/4)
    }
  })
  return(gg)
})   

output$map <- renderGvis({
  map_build()
})

output$map1_dl <- downloadHandler(
  filename = function() {
    paste0("wic_map_", Sys.time(), ".", if(input$map_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    gg <- map_build()
    
    #generate head.html
    dl_head(year = input$map_year, scenario = input$map_sn, geo = input$map_area, 
            type = "map", 
            ind = input$map_ind, age = input$map_age, 
            sex = input$map_sex, edu = input$map_edu)
    #generate gg.html
    gg$html$caption <- includeHTML("head.html")
    print(gg, file = "gg.html")
    
    webshot(
      url = "gg.html", 
      file = paste0("./output.", input$map_dl), 
      delay = 5,
      zoom = ifelse(input$map_dl == ".pdf", 0.5, 1)
    )
    
    file.copy(paste0("output.", input$map_dl), file)
    file.remove("gg.html")
    file.remove("head.html")
    file.remove(paste0("output.", input$map_dl))
  }
)
