tagList(
  fluidRow(
    column(
      width = 3, 
      selectizeInput(inputId = "map_ind", label  ="Indicator", choices = ind4, width="100%")
    ),
    column(
      width = 3, 
      selectizeInput(inputId = "map_area", label = "Area", choices = NULL, width = "100%")
    ),
    column(
      width = 3, 
      selectizeInput(inputId = "map_sn", label = "Scenario", choices = sn1, width="100%")
    ),
    # column(
    #   width = 3, 
    #   selectizeInput(inputId = "map_year", label = "Time", width="100%",
    #                  min = 1950, max = 2100, value = c(2015), step= 5, sep="", ticks= FALSE)
    # ),
    column(width = 3, uiOutput("map_year0"))
  ),
  fluidRow(
    column(
      width = 3, 
      selectizeInput(inputId = "map_age", label = "Age", choices = NULL, width="100%")
    ),
    column(
      width = 3, 
      selectizeInput(inputId = "map_sex", label = "Sex", choices = NULL, width="100%")
    ),
    column(
      width = 3, 
      selectizeInput(inputId = "map_edu", label = "Education", choices = NULL, width="100%")
    )
  ),
  htmlOutput("map"),
  br(),
  fluidRow(
    column(12,
      fluidRow(
        column(6, 
          h4("Graphic Options:"),
          fluidRow(
            column(6, 
              selectizeInput("map_proj", "Projection", width="100%",
                             choices = list("Mercator"="mercator", "Kavrayskiy VII"="kavrayskiy-vii", "Albers"="albers", "Lambert"="lambert"), 
                             selected = "kavrayskiy-vii")
            )
          )
        ),
        column(6,
          h4("Download Options:"),
          fluidRow(
            column(6, selectizeInput("map_dl", "File Type", width="100%",  choices = list("PDF"="pdf","PNG"="png")), htmlOutput("map_dlwarn") ),
            column(4, downloadButton('map1_dl', 'Map'))
          )
        )
      )
    )
  ),
  tags$style(type='text/css', "#map1_dl { width:100%; margin-top: 25px;}"),
  br(),
  br(),
  br(),
  br(),
  br(),
  br()
)


