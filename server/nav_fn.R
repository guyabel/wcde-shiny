observe({
  data <- parseQueryString(session$clientData$url_search)
  session$sendCustomMessage(type='updateSelections', data)
})
