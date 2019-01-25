output$sac_year10 <- renderUI({
  validate(
    need(input$sac_edu, "", "")
  )
  year.min<-1950
  if(input$sac_edu == 8){
    year.min <- 2015
  }
  sliderInput("sac_year1", "Year", min = 1950, max = 2100, value = c(year.min,2100), step= 5, ticks= FALSE, sep="", width="100%")
})

output$sac_year20 <- renderUI({
  validate(
    need(input$sac_edu, "", "")
  )
  year.min<-1950
  if(input$sac_edu == 8){
    year.min <- 2015
  }
  sliderInput("sac_year2", "Year", min = 1950, max = 2100, value = c(year.min,2100), step= 5, ticks= FALSE, sep="", width="100%")
})