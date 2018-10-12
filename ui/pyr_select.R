tagList(
  fluidRow(
    column(width = 3, selectizeInput("pyr_geo1", "Area", choices = geo1, width="100%")),
    column(width = 3, selectizeInput("pyr_sn1", "Scenario", choices = sn1, selected=sn1[1], width="100%")),
    column(width = 3, selectizeInput("pyr_geo2", "Area", choices = geo1, width="100%")),
    column(width = 3, selectizeInput("pyr_sn2", "Scenario", choices = sn1, selected=sn1[1], width="100%"))
  ),
  fluidRow(
    column(width = 6, sliderInput("pyr_year1", "Year", min = 1950, max = 2100, value = 2015, step= 5, ticks= FALSE, sep="", width="100%")),
                                  # animate = TRUE)),
    column(width = 6, sliderInput("pyr_year2", "Year", min = 1950, max = 2100, value = 2050, step= 5, ticks= FALSE, sep="", width="100%"))
                                  # animate = TRUE))
  ),
  fluidRow(
    column(width = 6, htmlOutput("pyr_warn1")),
    column(width = 6, htmlOutput("pyr_warn2"))
  ),
  fluidRow(
    column(width=12, align="center", htmlOutput("pyr_leg"))
  ),
  br(),
  fluidRow(
    column(width = 6, htmlOutput("pyr1")),
    column(width = 6, htmlOutput("pyr2")),  style = "overflow:hidden;"
  ),
  br(),
  fluidRow(
    column(12, 
      fluidRow(
        column(6, 
          h4("Graphic Options:"),
          fluidRow(
            column(4, selectizeInput("pyr_edu", "Educational Breakdown", width="100%",  choices = list("Four Categories"=4,"Six Categories"=6))),
            column(4, selectizeInput("pyr_x", "Horizontal Axis", width="100%",
                                     choices = list("Data Specific"="data","Entire Time Horizon"="allyear","Both Plots"="allarea"))),
            column(4, selectizeInput("pyr_prop", "Data", width="100%",
                                     choices = list("Population"="FALSE","Percentage"="TRUE")))
          )
        ),
        column(6,
          h4("Download Options:"),
          fluidRow(
            column(4, selectizeInput("pyr_dl", "File Type", width="100%",  choices = list("PNG"="png","PDF"="pdf"))),
            column(4, downloadButton('pyr1_dl', 'Left Pyramid')),
            column(4, downloadButton('pyr2_dl', 'Right Pyramid'))
          )
        )
      )
    )
  ),
  tags$style(type='text/css', "#pyr1_dl { width:100%; margin-top: 25px;}"),
  tags$style(type='text/css', "#pyr2_dl { width:100%; margin-top: 25px;}"),
  br(),
  br(),
  br(),
  br()
)
