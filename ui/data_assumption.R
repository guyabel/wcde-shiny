tagList(
  column(3, 
         br(),
         p("Basic underlying assumptions used in the projection of future indicators. Available at the country level only. 
           Full information on the scenarios can be found on the About page."),
         br(),
         uiOutput("ass_sn0"),
         # selectizeInput(inputId = "ass_sn",  label = "Scenario", 
         #                choices = sn1, 
         #                multiple = TRUE, width="100%",  
         #                options = list(placeholder = 'Type for multiple scenarios')),
         checkboxInput('ass_prevsn', 'Use scenarios in Selection tab', TRUE), 
         br(),
         uiOutput("ass_nat0"),
         # selectizeInput(inputId = "ass_nat", label = "Country", 
         #                choices = nn2, 
         #                multiple = TRUE, width = "100%", 
         #                options = list(placeholder = 'Type for multiple countries')),
         checkboxInput('ass_prevnat', 'Use countries in Selection tab', TRUE), 
         br()
  ),
  column(9, br(), DTOutput("data_ass"))
)
  
  
