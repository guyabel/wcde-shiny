observe({
  query <- parseQueryString(session$clientData$url_search)
  # print(query)
  if (length(query) > 0) {
    if (names(query) == "tab") {
      v <- query[["tab"]] %>%
        ifelse(. == "about", yes = ., no = paste(., "explorer")) %>%
        stringr::str_to_title()
      # print(v)
      try(updateTabsetPanel(session, "page", selected = v))
    }
  }
})