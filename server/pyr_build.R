##
##PYRAMID
##
library(googleVis)
library(saves)
library(dplyr)
# setwd("E:/VID/project/wcde")
# load("label.RData")
# source("server/pyr_fn.R")
# df0<-loads(file="df1", variables=c("area", "isono", "year","period", "ageno","sexno","eduno","age","bage","sage","sex","edu"), ultra.fast = TRUE, to.data.frame=TRUE)
# input<-NULL; input$pyr_sn1<-2; input$pyr_geo1="France"; input$pyr_year1=2010; input$pyr_sn2<-2; input$pyr_geo2="Germany"; input$pyr_edu=6; input$pyr_dl="png"

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
  df1 <- geog %>% filter(name %in% input$pyr_geo1)
  if(df1$dim=="country" & df1$is171==0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})
output$pyr_warn2 <- renderUI({
  tt<-""
  validate(
    need(input$pyr_geo2 != "", " "),
    need(input$pyr_sn2 != "", " ")
  )
  df1 <- geog %>% filter(name %in% input$pyr_geo2)
  if(df1$dim=="country" & df1$is171==0)
    tt <-"<FONT COLOR='gray'>Your have selected a country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})

pyr_max<-reactive({
  max1<-NULL;max2<-NULL
  if(input$pyr_x=="allyear"){
    max1<-max(df_pyr1 %>% filter(ageno!=0, sexno!=0, eduno==0, scenario==input$pyr_sn1, area==input$pyr_geo1) %>% .[["pop"]], na.rm=TRUE)
    max2<-max(df_pyr2 %>% filter(ageno!=0, sexno!=0, eduno==0, scenario==input$pyr_sn2, area==input$pyr_geo2) %>% .[["pop"]], na.rm=TRUE)
  }
  if(input$pyr_x=="allarea"){
    df3 <- bind_rows(df_pyr1,df_pyr2)
    max1<-max(df3 %>% filter(ageno!=0, sexno!=0, eduno==0, scenario %in% c(input$pyr_sn1,input$pyr_sn2),
                             area %in% c(input$pyr_geo1,input$pyr_geo2)) %>% .[["pop"]], na.rm=TRUE)
    max2<-max1
  }
  return(list(max1=max1,max2=max2))
})

output$pyr1<- renderGvis({
  gg<-NULL
  validate(
    need(input$pyr_geo1 != "", "Please select Area"),
    need(input$pyr_sn1 != "", "Please select Scenario")
  )
  withProgress(message = 'Loading Left Pyramid', value = 0, {
    df1<-loads(file=paste0("df",input$pyr_sn1), variables="epop", ultra.fast = TRUE, to.data.frame=TRUE)
    incProgress(1/4)
    df_pyr1 <- cbind(df0,df1) %>% filter(area==input$pyr_geo1) %>% mutate(scenario=input$pyr_sn1) %>% rename(pop=epop)
    incProgress(2/4)
    if(input$pyr_edu==4){
      levels(df_pyr1$edu)<-names(edu2)
      df_pyr1 <- df_pyr1 %>% mutate(eduno=ifelse(eduno==3, 4, eduno)) %>% mutate(eduno=ifelse(eduno==5, 6, eduno))
      df_pyr1 <- df_pyr1 %>% group_by(area, isono, year, ageno, age, sex, sexno, edu, eduno, scenario) %>% summarise(pop=sum(pop))
      incProgress(3/4)
    }
    
    df_pyr1 <<- df_pyr1
    noedu_pyr1 <<- geog %>% filter(name==input$pyr_geo1) %>% .[["is171"]] %in% 0 & input$pyr_year1<2010
    if(input$pyr_geo1=="Israel" & input$pyr_year1<2010)
      noedu_pyr1 <<- TRUE
    
    if(noedu_pyr1==FALSE)
      gg<-gpyr(df_pyr1, pyear=input$pyr_year1, pcol=get(paste0("iiasa",input$pyr_edu)), w=295, legend="none", pmax=pyr_max()$max1, prop = input$pyr_prop)
    if(noedu_pyr1==TRUE)
      gg<-gpyr(df_pyr1, pyear=input$pyr_year1, pcol="['darkgrey']", w=295, legend="none", pmax=pyr_max()$max1, no.edu = TRUE, prop = input$pyr_prop)
    incProgress(4/4)
  })
  return(gg)
})

output$pyr2<- renderGvis({
  gg<-NULL
  validate(
    need(input$pyr_geo2 != "", "Please select an Area"),
    need(input$pyr_sn2 != "", "Please select a Scenario")
  )
  withProgress(message = 'Loading Right Pyramid', value = 0, {
    df2<-loads(file=paste0("df",input$pyr_sn2), variables="epop", ultra.fast = TRUE, to.data.frame=TRUE) %>% rename(pop=epop)
    incProgress(1/4)
    df_pyr2 <- cbind(df0,df2) %>% filter(area==input$pyr_geo2) %>% mutate(scenario=input$pyr_sn2)
    incProgress(2/4)
    if(input$pyr_edu==4){
      levels(df_pyr2$edu)<-names(edu2)
      df_pyr2 <- df_pyr2 %>% mutate(eduno=ifelse(eduno==3, 4, eduno)) %>% mutate(eduno=ifelse(eduno==5, 6, eduno))
      df_pyr2 <- df_pyr2 %>% group_by(area, isono, year, ageno, age, sex, sexno, edu, eduno, scenario) %>% summarise(pop=sum(pop))
      incProgress(3/4)
    }
    
    df_pyr2 <<- df_pyr2
    noedu_pyr2 <<- geog %>% filter(name==input$pyr_geo2) %>% .[["is171"]] %in% 0 & input$pyr_year2<2010
    if(input$pyr_geo2=="Israel" & input$pyr_year2<2010)
      noedu_pyr2 <<- TRUE
    
    if(noedu_pyr2==FALSE)
      gg<-gpyr(df_pyr2, pyear=input$pyr_year2, pcol=get(paste0("iiasa",input$pyr_edu)), w=295, legend="none", pmax=pyr_max()$max2, prop = input$pyr_prop)
    if(noedu_pyr2==TRUE)
      gg<-gpyr(df_pyr2, pyear=input$pyr_year2, pcol="['darkgrey']", w=295, legend="none", pmax=pyr_max()$max2, no.edu = TRUE, prop = input$pyr_prop)
    #gg<-gpyr(df_pyr2, pyear=input$pyr_year2, pcol=get(paste0("iiasa",input$pyr_edu)), w=295, legend="none", pmax=pyr_max()$max2)
    incProgress(4/4)
  })
  return(gg)
})


output$pyr_leg<- renderGvis({
  #want to only react to tick box
  df1 <- df0 %>% filter(year==2010, sexno==0, ageno==0, eduno!=0, isono==4) %>% select(edu,age,sexno) %>% 
    dcast(age~edu, value.var="sexno")
  w<-900
  if(input$pyr_edu==4){
    df1 <- df0 %>% filter(year==2010, sexno==0, ageno==0, eduno %in% c(1,2,4,6,7), isono==4) %>% select(edu,age,sexno) 
    levels(df1$edu)<-names(edu2)
    df1 <- df1 %>% dcast(age~edu, value.var="sexno")
    w<-600
  }
  gvisBarChart(df1, xvar="age", yvar=names(df1)[-1], 
               options=list(colors=get(paste0("iiasa",input$pyr_edu)), height=30, width=w, legend="{position:'top', textStyle: {fontSize: 12}}",
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
    
    if(noedu_pyr1==FALSE)
      gg<-gpyr(df_pyr1, pyear=input$pyr_year1, pcol=get(paste0("iiasa",input$pyr_edu)), w=500, h=700, legend="top", pmax=pyr_max()$max1, prop = input$pyr_prop)
    if(noedu_pyr1==TRUE)
      gg<-gpyr(df_pyr1, pyear=input$pyr_year1, pcol="['darkgrey']", w=500, h=700, legend="top", legend="none", pmax=pyr_max()$max1, no.edu = TRUE, prop = input$pyr_prop)
    gg$html$caption<-readLines("head.html")
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
    cat(paste0(dimen %>% filter(dim=="scenario", code==input$pyr_sn2) %>% .[["name"]], "<br>\n<br>\n"), file = fh)
    cat(ifelse(geog %>% filter(name %in% input$pyr_geo2) %>% .[["is171"]] %in% 0, paste0(tt,"<br>\n<br>\n"), ""), file = fh)
    close(fh)
    
    if(noedu_pyr2==FALSE)
      gg<-gpyr(df_pyr2, pyear=input$pyr_year2, pcol=get(paste0("iiasa",input$pyr_edu)), w=500, h=700, legend="top", pmax=pyr_max()$max2, prop = input$pyr_prop)
    if(noedu_pyr2==TRUE)
      gg<-gpyr(df_pyr2, pyear=input$pyr_year2, pcol="['darkgrey']",  w=500, h=700, legend="top", pmax=pyr_max()$max2, no.edu = TRUE, prop = input$pyr_prop)
    
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
