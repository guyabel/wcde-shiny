library(reshape2)
library(tidyverse)
library(magrittr)
library(saves)
library(shiny)
library(googleVis)
# library(webshot)

load("label.RData")

shinyServer(function(input, output, session) {
  for (file in list.files("server")) {
    source(file.path("server", file), local = TRUE)
  }
})


