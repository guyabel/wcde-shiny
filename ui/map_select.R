tagList(
  fluidRow(
    column(width = 3, selectizeInput("map_ind", "Indicator", choices = ind4, selected=ind4[[1]][1], width="100%")),
    column(width = 3, selectizeInput("map_area", "Area", choices = geo2, selected=geo2[[1]][1], width="100%")),
    column(width = 3, uiOutput("map_sn0")),
    column(width = 3, uiOutput("map_year0"))
  ),
  fluidRow(
    column(width = 3, uiOutput("map_age0")),
    column(width = 3, uiOutput("map_sex0")),
    column(width = 3, uiOutput("map_edu0"))
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
                             choices = list("Mercator"="mercator", "Kavrayskiy VII"="kavrayskiy-vii", "Albers"="albers", "Lambert"="lambert"))
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


