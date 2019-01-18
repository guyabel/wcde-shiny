# output$temp <- renderPrint({reactiveValuesToList(input)})
output$temp <- renderPrint({d})

# output$temp <- renderPrint({
#   as.list(input)
# })
