output$ass_nat0 <- renderUI({
  nat.s<-NULL
  nat.c<-nn2
  nat.p<-'Type for multiple countries'
  if(input$ass_prevnat==TRUE){
    nat.c<-input$nat
    nat.s<-input$nat
  }
  selectizeInput("ass_nat","Country", choices = nat.c, selected=nat.s, multiple=TRUE, width="100%",  
                 options = list(placeholder = nat.p))
})

output$ass_sn0 <- renderUI({
  scenario.c<-as.list(sn1)
  scenario.s<-NULL
  scenario.p<-'Type for multiple scenarios'
  if(input$ass_prevsn==TRUE){
    #scenario.c<-sn2[input$scenario]
    scenario.s<-input$scenario
  }
  selectizeInput("ass_sn","Scenario", choices = scenario.c, selected=scenario.s, multiple=TRUE, width="100%",  
                 options = list(placeholder = scenario.p))
})

#input<-NULL; input$ass_nat<-"France"; input$ass_sn<-sn1[1:2]
  
output$data_ass <- renderDataTable({
  df1<-NULL;
  df1 <- assump %>% filter(country %in% input$ass_nat, sno %in% input$ass_sn) %>% 
    select(country,scenario,mortality,fertility,migration,education) %>%
    rename(Country=country,Scenario=scenario,Mortality=mortality,Fertility=fertility,Migration=migration,Education=education)
},  options = list(searching = FALSE, paging = FALSE, 
                   drawCallback = I("function( settings ) {document.getElementById('df').style.width = '800px';}")))


output$temp <- renderPrint({
  as.list(input)
})