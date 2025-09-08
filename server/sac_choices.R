output$sac_year10 <- renderUI({
  validate(
    need(input$sac_edu, "", "")
  )
  year_min<-1950
  if(input$sac_edu == 8){
    year_min <- 2020
  }
  sliderInput(inputId = "sac_year1", label = "Year", 
              min = 1950, max = 2100, value = c(year_min,2100), step= 5, 
              ticks= FALSE, sep="", width="100%")
})

output$sac_year20 <- renderUI({
  validate(
    need(input$sac_edu, "", "")
  )
  year_min<-1950
  if(input$sac_edu == 8){
    year_min <- 2020
  }
  sliderInput(inputId = "sac_year2", label = "Year", 
              min = 1950, max = 2100, value = c(year_min,2100), step= 5, 
              ticks= FALSE, sep="", width="100%")
})
