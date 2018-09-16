output$temp <- renderPrint({reactiveValuesToList(input)})

# output$temp <- renderPrint({
#   as.list(input)
# })
