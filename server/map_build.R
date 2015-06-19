output$temp <- renderPrint({
  #as.list(input)
  map_build()
})


map_geo<-reactive({
  geo<-NULL
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

#input=NULL;input$map_year=2010; input$map_ind=ind[[1]][1]; input$map_proj="mercator";input$map_area="Asia"; input$map_sn=1; 
#input$map_age="";input$map_sex=1;input$map_edu=""
map_build <- reactive({
  validate( 
    need(input$map_ind != "", "Please select Indicator"), 
    need(input$map_area != "", "Please select Area"), 
    need(input$map_sn != "", "Please select Scenario")
  )
  df1<-NULL;df2<-NULL; gg<-NULL
  withProgress(message = 'Loading Map', value = 0, {
    if(length(input$map_sn)>0){
      fn <- ind %>% filter(fullname==input$map_ind)  %>% .[["name"]]
      cn <- ind %>% filter(fullname==input$map_ind)  %>% .[["cname"]]
      incProgress(1/4)
      df1<-loads(file=paste0("df",input$map_sn), variables=fn, ultra.fast = TRUE, to.data.frame=TRUE)
      incProgress(2/4)
      df2 <- cbind(df0,df1) %>% 
        filter(area %in% map_geo(), #geo,#
               year==input$map_year,
               ageno==if(nchar(input$map_age)==0) 0 else input$map_age,
               sexno==if(nchar(input$map_sex)==0) 0 else input$map_sex,
               eduno==if(nchar(input$map_edu)==0) 0 else input$map_edu) %>%
        left_join(geog, by="isono") 
      names(df2)[names(df2)==fn]<-cn
      incProgress(3/4)
      gg<-gvisGeoChart(df2, locationvar="ggarea", colorvar=cn, hovervar="area", chartid="map",
                       options=list(width=1100, height=800, 
                                    dataMode="regions", 
                                    region=geog %>% filter(name==input$map_area) %>% .[["ggarea"]],
                                    projection=input$map_proj,
                                    colorAxis="{colors:['lightgrey', 'dodgerblue']}"))
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
    paste0('wic_map.', if(input$map_dl=="pdf") 'pdf' else 'png')
  },
  content = function(file) {
    fh <- file("head.html", "w")
    cat(pdfinfo, file = fh)
    cat(paste0(input$map_ind,  "<br>\n"), file = fh)
    cat(paste0(input$map_area, "<br>\n"), file = fh)
    cat(paste0(dimen %>% filter(dim=="scenario", code==input$map_sn) %>% .[["name"]], "<br>\n"), file = fh)
    cat(paste0(input$map_year, "<br>\n"), file = fh)
    if(length(input$map_age)>0)
      cat(paste0("Age: ", dimen %>% filter(dim=="sex", code==input$map_age) %>% .[["name"]], "<br>\n"), file = fh)
    if(length(input$map_sex)>0)
      cat(paste0("Sex: ", dimen %>% filter(dim=="sex", code==input$map_sex) %>% .[["name"]], "<br>\n"), file = fh)
    if(length(input$map_edu)>0)
      cat(paste0("Education: ", dimen %>% filter(dim=="sex", code==input$map_edu) %>% .[["name"]], "<br>\n"), file = fh)
    cat("<br>\n", file = fh)
    close(fh)

    gg<-map_build()
    gg$html$caption<-readLines("head.html")
    print(gg, file="gg.html")
    
    if(input$sac_dl=="pdf"){
      system("wkhtmltopdf   --enable-javascript --javascript-delay 2000 gg.html gg.pdf")
      file.copy("./gg.pdf", file)
      file.remove("gg.pdf")
    }
    if(input$sac_dl=="png"){
      system("wkhtmltoimage --enable-javascript --javascript-delay 4000 gg.html gg.png"); #file.show("gg.png")
      file.copy("./gg.png", file)
      file.remove("gg.png")
    }
    file.remove("gg.html")
    file.remove("head.html")
  }
)

output$map_dlwarn <- renderUI({
  tt<-""
  if(input$map_dl=="png")
    tt <-"<FONT COLOR='gray'>PNG Download of maps is not always possible, especially for more complex map images based on larger areas."
  HTML(tt)
})
#plot(gmap)