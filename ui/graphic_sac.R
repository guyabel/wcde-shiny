tagList(
  fluidRow(
    column(
      width = 3, 
      selectizeInput(
        inputId = "sac_geo1", label = "Area", width = "100%",
        choices = geo1
      )
    ),
    column(width = 3, selectizeInput("sac_sn1", "Scenario", choices = sn1, selected=sn1[1], width="100%")),
    column(width = 3, selectizeInput("sac_geo2", "Area", choices = geo1, width="100%")),
    column(width = 3, selectizeInput("sac_sn2", "Scenario", choices = sn1, selected=sn1[2], width="100%"))
  ),
  fluidRow(
    column(
      width = 6, 
      uiOutput(outputId = "sac_year10")
    ),
    column(
      width = 6, 
      uiOutput(outputId = "sac_year20")
    )
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
            column(
              width = 4, 
              selectizeInput(
                inputId = "sac_edu", 
                label = "Educational Categories", 
                width = "100%",  
                choices = 
                  list(
                    "Four Categories" = 4,
                    "Six Categories" = 6
                    # "Eight Categories" = 8
                  )
              )
            ),
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
