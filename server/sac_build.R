##
##Stacked area charts
##
#library(googleVis)
#setwd("C:/Users/gabel/Documents/shiny/wic2")
#df0<-loads(file="df1", variables=c("area", "isono", "year","period", "ageno","sexno","eduno","age","bage","sage","sex","edu"), ultra.fast = TRUE, to.data.frame=TRUE)
#source("./server/sac_fn.R")
#input<-NULL; input$sac_sn1=2; input$sac_geo1="France"; input$sac_edu=6; input$sac_year1=c(1970,2100)


output$sac_warn1 <- renderUI({
  tt<-""
  validate(
    need(input$sac_geo1 != "", " "),
    need(input$sac_sn1 != "", " ")
  )
  df1 <- geog %>% filter(name %in% input$sac_geo1)
  if(df1$dim=="country" & df1$is171==0)
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
  if(df1$dim=="country" & df1$is171==0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})

sac_max<-reactive({
  max1<-NULL;max2<-NULL
  if(input$sac_y=="allarea"){
    df3 <- bind_rows(df_sac1,df_sac2)
    max1<-max(df3 %>% filter(eduno==0, 
                             scenario %in% c(input$sac_sn1,input$sac_sn2), 
                             area %in% c(input$sac_geo1,input$sac_geo2)) %>% .[["pop"]], na.rm=TRUE)
    max2<-max1
  }
  return(list(max1=max1,max2=max2))
})

output$sac1<- renderGvis({
  gg<-NULL
  validate(
    need(input$sac_geo1 != "", "Please select Area"),
    need(input$sac_sn1 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Plot', value = 0, {
    df1<-loads(file=paste0("df",input$sac_sn1), variables="epop", ultra.fast = TRUE, to.data.frame=TRUE)
    incProgress(1/4)
    df_sac1 <- df0 %>% 
      bind_cols(df1) %>% 
      filter(ageno==0, sexno==0, area==input$sac_geo1, year %in% seq(input$sac_year1[1],input$sac_year1[2],5)) %>% 
      mutate(scenario=input$sac_sn1) %>% 
      rename(pop=epop)
    incProgress(2/4)
    if(input$sac_edu==4){
      levels(df_sac1$edu)<-names(edu2)
      df_sac1 <- df_sac1 %>% mutate(eduno=ifelse(eduno==3, 4, eduno)) %>% mutate(eduno=ifelse(eduno==5, 6, eduno))
      df_sac1 <- df_sac1 %>% group_by(area, isono, year, ageno, age, sex, sexno, edu, eduno, scenario) %>% summarise(pop=sum(pop))
      incProgress(3/4)
    }
    df_sac1 <<- df_sac1
    gg<-gsac(df_sac1, pcol=get(paste0("iiasa",input$sac_edu)), w=300, legend="none", pmax=sac_max()$max1, prop = input$sac_prop)
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
    df1<-loads(file=paste0("df",input$sac_sn2), variables="epop", ultra.fast = TRUE, to.data.frame=TRUE)
    incProgress(1/4)
    df_sac2 <- cbind(df0,df1) %>% filter(ageno==0, sexno==0, area==input$sac_geo2, year %in% seq(input$sac_year2[1],input$sac_year2[2],5)) %>% 
      mutate(scenario=input$sac_sn2) %>% rename(pop=epop)
    incProgress(2/4)
    if(input$sac_edu==4){
      levels(df_sac2$edu)<-names(edu2)
      df_sac2 <- df_sac2 %>% mutate(eduno=ifelse(eduno==3, 4, eduno)) %>% mutate(eduno=ifelse(eduno==5, 6, eduno))
      df_sac2 <- df_sac2 %>% group_by(area, isono, year, ageno, age, sex, sexno, edu, eduno, scenario) %>% summarise(pop=sum(pop))
      incProgress(3/4)
    }
    df_sac2 <<- df_sac2
    gg<-gsac(df_sac2, pcol=get(paste0("iiasa",input$sac_edu)), w=300, legend="none", pmax=sac_max()$max1, prop = input$sac_prop)
    incProgress(4/4)
  })
  return(gg)
})

output$sac_leg<- renderGvis({
  #want to only react to tick box
  df1 <- df0 %>% filter(year==2010, sexno==0, ageno==0, eduno!=0, isono==4) %>% select(edu,age,sexno) %>% 
    dcast(age~edu, value.var="sexno")
  w<-900
  if(input$sac_edu==4){
    df1 <- df0 %>% filter(year==2010, sexno==0, ageno==0, eduno %in% c(1,2,4,6,7), isono==4) %>% select(edu,age,sexno) 
    levels(df1$edu)<-names(edu2)
    df1 <- df1 %>% dcast(age~edu, value.var="sexno")
    w<-600
  }
  gvisBarChart(df1, xvar="age", yvar=names(df1)[-1], 
               options=list(colors=get(paste0("iiasa",input$sac_edu)), height=30, width=w, legend="{position:'top', textStyle: {fontSize: 12}}",
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
    
    gg<-gsac(df_sac1, pcol=get(paste0("iiasa",input$sac_edu)), w=500, h=700, legend="top", pmax=sac_max()$max1, prop = input$sac_prop)
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
    
    gg<-gsac(df_sac2, pcol=get(paste0("iiasa",input$sac_edu)), w=500, h=700, legend="top", pmax=sac_max()$max2, prop = input$sac_prop)
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
