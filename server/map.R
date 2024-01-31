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

#input=NULL;input$map_year=2020; input$map_ind=ind4[[4]][3]; input$map_proj="mercator";input$map_area="Latin America and the Caribbean"; input$map_sn=1; input$map_age="";input$map_sex=1;
#input$map_edu=0; input$map_age = 5; input$map_sex=2;
# input=NULL;input$map_year=2020; input$map_ind=ind4[[1]][1]; input$map_proj="mercator";input$map_area="World"; input$map_sn=2; input$map_age="0";input$map_sex="0"; input$map_edu=0;
map_build <- reactive({
  validate( 
    need(input$map_ind != "", ""), 
    need(input$map_area != "", ""), 
    need(input$map_sn != "", ""),
    need(input$map_age != "", ""),
    need(input$map_sex != "", ""),
    need(input$map_edu != "", ""),
    need(input$map_year != "", "Push Generate Map button once you have your desired data selections")
  )
  df1 <- df2 <- gg<-NULL
  withProgress(message = 'Loading Map', value = 0, {
    if(length(input$map_sn)>0){
      cn <- ind %>% 
        filter(fullname == input$map_ind) %>%
        pull(cname)
      
      v <- c("year", "ageno", "sexno", "eduno")
      
      # availabe countries for given scenario and indicator
      fn <- ind %>% 
        filter(fullname == input$map_ind) %>%
        pull(name)
      
      xx <- paste0("../wcde-data/wcde-v3-single/", input$map_sn, "/", fn, "/") %>%
        dir(all.files = TRUE) %>%
        str_remove(pattern = ".rds") %>%
        str_subset(pattern = "[0-9]") %>%
        as.numeric()
        
      if(input$map_area != "World"){
        v <- geog %>%
          filter(continent == input$map_area | region == input$map_area,
                 isono %in% xx) %>%
          pull(isono) %>%
          c(v, .)
      }
      
      if(input$map_area == "World"){
        v <- geog %>%
          filter(dim == "country",
                 isono %in% xx) %>%
          pull(isono) %>%
          c(v, .)
      }

      incProgress(1/4)
      
      df1 <- tibble(
        v = v,
        file = paste0("../wcde-data/wcde-v3-single/", input$map_sn, "/", fn, "/", v, ".rds")
      ) %>%
        mutate(d = map(.x = file, .f = ~read_rds(.x))) %>%
        select(-file) %>%
        pivot_wider(names_from = v, values_from = d) %>%
        unnest(col = names(.))
      # df1 <- loads(file = paste0("df",input$map_sn, "/", fn, "/"), 
      #              variables = v, ultra.fast = TRUE, to.data.frame=TRUE) %>%
      #   as_tibble()
      incProgress(2/4)
      d2a <- geog %>% 
        filter(dim == "country") %>%
        select(name, ggarea, isono)
      
      df2 <- df1 %>% 
        gather(key = isono, value = x, -(1:4), convert = TRUE) %>%
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
  # validate( 
  #   need(input$map_go != "", "Push Generate Map button once you have your desired data selections")
  # )
  m <- NULL
  input$map_go
  # Use isolate() to avoid dependency on input$obs
  # m <- isolate(map_build())
  m <- map_build()
  m
})

output$map1_dl <- downloadHandler(
  filename = function() {
    paste0("wic_map_", Sys.time(), ".", if(input$map_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    gg <- map_build()
    
    tdir = tempdir()
    dir.create(tdir, showWarnings = FALSE)
    temp_gg <- tempfile(pattern = "wcde_v2_", tmpdir = tdir, fileext = ".html")
    temp_img <- tempfile(pattern = "wcde_v2_", tmpdir = tdir, 
                         fileext = paste0(".", input$pyr_dl))
    
    gg$html$caption <-  dl_head(year = input$map_year, scenario = input$map_sn, 
                                geo = input$map_area, type = "map", 
                                ind = input$map_ind, age = input$map_age, 
                                sex = input$map_sex, edu = input$map_edu)
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
