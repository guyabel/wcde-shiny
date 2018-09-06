library(reshape2)
library(dplyr)
library(tidyr)
library(magrittr)
library(saves)
library(shiny)
library(googleVis)

#setwd("./shiny/wic2")
# df0 <- loads(file = "df1/pop", variables = c("year", "ageno", "sexno", "eduno"), ultra.fast = TRUE, to.data.frame=TRUE)

shinyServer(function(input, output, session) {
  for (file in list.files("server")) {
    source(file.path("server", file), local = TRUE)
  }
#   source("server/data_build.R", local = TRUE )
#   source("server/data_ui.R", local = TRUE )
#   source("server/pyr_build.R", local = TRUE )
})


