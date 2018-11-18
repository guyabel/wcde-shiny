tagList(
  fluidRow(
    column(width = 3, selectizeInput("sac_geo1", "Area", choices = geo1, width="100%")),
    column(width = 3, selectizeInput("sac_sn1", "Scenario", choices = sn1, selected=sn1[1], width="100%")),
    column(width = 3, selectizeInput("sac_geo2", "Area", choices = geo1, width="100%")),
    column(width = 3, selectizeInput("sac_sn2", "Scenario", choices = sn1, selected=sn1[4], width="100%"))
  ),
  fluidRow(
    column(width = 6, sliderInput("sac_year1", "Year", min = 1950, max = 2100, value = c(1950,2100), step= 5, ticks= FALSE, sep="", width="100%")),
    column(width = 6, sliderInput("sac_year2", "Year", min = 1950, max = 2100, value = c(1950,2100), step= 5, ticks= FALSE, sep="", width="100%"))
  ),
  fluidRow(
    column(width = 6, htmlOutput("sac_warn1")),
    column(width = 6, htmlOutput("sac_warn2"))
  ),
  fluidRow(
    column(width=12, align="center", htmlOutput("sac_leg"))
  ),
  br(),
  fluidRow(
    column(width = 6, htmlOutput("sac1")),
    column(width = 6, htmlOutput("sac2")),  style = "overflow:hidden;"
  ),
  br(),
  fluidRow(
    column(12,
      fluidRow(
        column(6, 
          h4("Graphic Options:"),
          fluidRow(
            column(4, selectizeInput("sac_edu", "Educational Categories", width="100%",  list("Four Categories"=4,"Six Categories"=6))),
            column(4, selectizeInput("sac_y", "Vertical Axis", width="100%",
                                     choices = list("Plot Specific"="data","Both Plots"="allarea"))),
            column(4, selectizeInput("sac_prop", "Data", width="100%",
                                     choices = list("Population"="FALSE","Percentage"="TRUE")))
          )
        ),
        column(6,
          h4("Download Options:"),
          fluidRow(
            column(4, selectizeInput("sac_dl", "File Type", width="100%",  choices = list("PNG"="png","PDF"="pdf"))),
            column(4, downloadButton('sac1_dl', 'Left Plot')),
            column(4, downloadButton('sac2_dl', 'Right Plot'))
          )
        )
      )
    )
  ),
  tags$style(type='text/css', "#sac1_dl { width:100%; margin-top: 25px;}"),
  tags$style(type='text/css', "#sac2_dl { width:100%; margin-top: 25px;}"),
  br(),
  br(),
  br(),
  br()
)
