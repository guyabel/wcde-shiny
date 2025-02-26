library(tidyverse, quietly = TRUE)
options(dplyr.summarise.inform = FALSE)

library(magrittr)
library(shiny)
library(googleVis, quietly = TRUE)
# for webshot to work on iiasa server
Sys.setenv(OPENSSL_CONF="/dev/null")
library(webshot)
library(DT)

load("label.RData")

options(shiny.sanitize.errors = FALSE)
options(shiny.legacy.datatable = TRUE)

shinyServer(function(input, output, session) {
  for (file in list.files("server")) {
    source(file.path("server", file), local = TRUE)
  }
})

