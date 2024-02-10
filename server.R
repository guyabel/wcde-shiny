library(tidyverse)
library(magrittr)
library(shiny)
library(googleVis)
library(webshot)

load("label.RData")

options(shiny.sanitize.errors = FALSE)

shinyServer(function(input, output, session) {
  for (file in list.files("server")) {
    source(file.path("server", file), local = TRUE)
  }
})


