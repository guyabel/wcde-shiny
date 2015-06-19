# setwd("C:/Users/gabel/Documents/shiny/wic2")
# df0<-loads(file="df1", variables=c("area", "isono", "year","period", "ageno","sexno","eduno","age","bage","sage","sex","edu"), ultra.fast = TRUE, to.data.frame=TRUE)

# output$temp <- renderPrint({
#   as.list(input)
# })

output$tit_ind <- renderText({
  ind  %>% filter(fullname %in% input$data_ind) %>% .[["fullname"]]
})

output$def_ind <- renderUI({
  tt <- ind  %>% select(definition,fullname) %>% filter(fullname %in% input$data_ind) %>% .[["definition"]]
  HTML(tt)
})

output$tit_scen <- renderText({
  if(length(input$scenario)==1)
    tt <- dimen %>% filter(dim=="scenario") %>% filter(code==input$scenario) %>% .[["name"]]
  if(length(input$scenario)>1)
    tt <- "Multiple Scenarios Selected"
  return(tt)
})

output$def_scen <- renderUI({
  if(length(input$scenario)==1)
    tt <- dimen %>% filter(dim=="scenario") %>% filter(code==input$scenario) %>% .[["details"]]
  if(length(input$scenario)>1)
    tt <- "See About tab for full scenario details."
  return(HTML(tt))
})

output$df_warn <- renderUI({
  tt<-""
  if(length(input$nat)>0)
    if(sum(geog %>% filter(name %in% input$nat) %>% .[["is171"]]==0)>0)
      tt <-"<FONT COLOR='gray'>Your selection includes at least one country with limited base year data on educational attainment. Please consult the FAQ in the About page for more information<br><br>"
  HTML(tt)
})

df_build <- reactive({
  df1<-NULL;df2<-NULL; df3<-NULL;
  df1 <- ind %>% filter(fullname==input$data_ind)
  fn <- df1  %>% .[["name"]]
  withProgress(message = 'Creating Data', detail = 'May take a few seconds...', value = 0, {
    for(i in input$scenario){
      #input<-NULL; input$age=0;  input$sex=0;  input$year=0;  input$year=2010; input$data_ind=ind2[[3]][4]; input$scenario=c(sn1[1],sn1[2]); input$isono=TRUE; i=2
      df2<- NULL
      df2 <- loads(file=paste0("df",i), variables=fn, ultra.fast = TRUE, to.data.frame=TRUE)
      sn <- dimen %>% filter(dim=="scenario", code==i) %>% .[["sname"]]
      edu0 <- 0 
      if(df1$edu==1)
        edu0<-unlist(edu1)
      if(df1$edu2==1)
        edu0<-unlist(edu1)[-(1:2)]
      if(fn=="etfr")
        edu0<-unlist(edu1)[-2]
      df2 <- df0 %>% mutate(Scenario=sn) %>% bind_cols(df2) %>% 
        filter(ageno %in% if(df1$age==1 | df1$bage==1 | df1$sage==1) input$age else 0,
               area %in% allarea(),#"France",#
               year %in% input$year,
               eduno %in% edu0,
               sexno %in% if(df1$sex!=0) input$sex else 0) %>%
        arrange(eduno, sexno, ageno, area, year) %>% 
        rename(Area=area) %>% select(Area, Scenario, everything())

      if(df1$period==0)
        df2 <- df2 %>% select(-period) %>% rename(Year=year)
      if(df1$period==1)
        df2 <- df2 %>% select(-year) %>% rename(Period=period)
      
      if(df1$age==0)
        df2 <- df2 %>% select(-age)
      if(df1$age==1)
        df2 <- df2 %>% rename(Age=age)
      if(df1$bage==0)
        df2 <- df2 %>% select(-bage)
      if(df1$bage==1)
        df2 <- df2 %>% rename(Age=bage)
      if(df1$sage==0)
        df2 <- df2 %>% select(-sage)
      if(df1$sage==1)
        df2 <- df2 %>% rename(Age=sage)
      if(sum(as.numeric(input$age))==0 & sum(df1$age,df1$bage,df1$sage)>0)
        df2 <- df2 %>% select(-Age)
      df2 <- df2 %>% select(-ageno)
      
      if(df1$sex==0)
        df2 <- df2 %>% select(-sex)
      if(df1$sex==1)
        df2 <- df2 %>% rename(Sex=sex)
      if(sum(as.numeric(input$sex))==0 & df1$sex==1)
        df2 <- df2 %>% select(-Sex)
      df2 <- df2 %>% select(-sexno)
      
      if(df1$edu==0)
        df2 <- df2 %>% select(-edu)
      if(df1$edu==1)
        df2 <- df2 %>% rename(Education=edu)
      df2 <- df2 %>% select(-eduno)
      
      if(input$isono==FALSE)
        df2 <- df2 %>% select(-isono)
      if(input$isono==TRUE)
        df2 <- df2 %>% rename(ISOCode=isono)
      
      names(df2)[ncol(df2)] <- ind %>% filter(fullname==input$data_ind) %>% .[["cname"]]
      
      if(length(input$scenario)==1)
        df2 <- df2 %>% select(-Scenario)
      
      df3 <- df3 %>% bind_rows(df2)
      incProgress(1/length(input$scenario))
    }
  })
  return(df3)
})

output$df <- renderDataTable({
  df_build()
},  options = list(searching = TRUE, paging = TRUE, 
                   aoColumnDefs = list(list(sClass="alignRight",aTargets=list(-1))),
                   drawCallback = I("function( settings ) {document.getElementById('df').style.width = '800px';}") ))


output$data_dl <- downloadHandler(
  filename = function() { "wicdf.csv" },
  content = function(filename) {
    sn0<-dimen %>% filter(dim=="scenario") %>% filter(code==input$scenario) %>% .[["name"]]
    df2 <- ind %>% filter(fullname==input$data_ind) %>% select(fullname, definition) %>% 
      mutate(scen=ifelse(length(input$scenario)==1, sn0,"Multiple Scenarios"))
    df2 <- rbind(t(df2)," ") 
    colnames(df2)<-""

    warn<-NULL
    if(length(allarea())>0){
      if(sum(geog %>% filter(name %in% allarea()) %>% .[["is171"]], na.rm=TRUE)>0){
        warn<-"Note: Your selection includes at least one country with limited base year data on educational attainment. Please consult the FAQ for more information"
      }
    }
    csvinfo0<-paste0(substring(csvinfo,1,nchar(csvinfo)-1),warn, "\n")

    fh <- file(filename, "w")
    cat(csvinfo0, file = fh)
    write.csv(df2, file = fh, row.names = FALSE)
    write.csv(df_build(), fh, row.names = FALSE)
    close(fh)
  }
)


output$data_dl0 <- downloadHandler(
  filename = function() { "wicdf.csv" },
  content = function(filename) {
    sn0<-dimen %>% filter(dim=="scenario") %>% filter(code==input$scenario) %>% .[["name"]]
    df2 <- ind %>% filter(fullname==input$data_ind) %>% select(fullname, definition) %>% 
      mutate(scen=ifelse(length(input$scenario)==1, sn0,"Multiple Scenarios"))
    df2 <- rbind(t(df2)," ") 
    colnames(df2)<-""
    
    warn<-NULL
    if(length(allarea())>0){
      if(sum(geog %>% filter(name %in% allarea()) %>% .[["is171"]], na.rm=TRUE)>0){
        warn<-"Note: Your selection includes at least one country with limited base year data on educational attainment. Please consult the FAQ for more information"
      }
    }
    csvinfo0<-paste0(substring(csvinfo,1,nchar(csvinfo)-1),warn, "\n")
    
    fh <- file(filename, "w")
    cat(csvinfo0, file = fh)
    write.csv(df2, file = fh, row.names = FALSE)
    write.csv(df_build(), fh, row.names = FALSE)
    close(fh)
  }
)



