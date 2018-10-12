tagList(
  fluidRow(
    column(width = 3, 
           includeMarkdown("md/prof_blurb.md"),
           selectizeInput("prof_geo", "Area", choices = nn2, width="100%"),
           strong("Download"),
           downloadButton('prof_dl', 'Profile'),
           br(),
           br(),
           helpText("Note: The image to the right is a sample preview. It will not update.")
    ),
    column(width = 9,
           img(src="prof.png", width="100%")
           )
  ),
  tags$style(type='text/css', "#prof_dl { width:100%; margin-top: 5px;}"),
  br(),
  br(),
  br(),
  br()
)
