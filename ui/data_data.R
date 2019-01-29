tagList(
  column(9, br(), dataTableOutput("df")),
  column(3, 
         br(),
         HTML("<div id='linkToSelect'><button type='button' class='btn btn-block btn-primary'><span class='glyphicon glyphicon-repeat'></span> Back to Selection</button></div>"), 
         br(), h4("Indicator Details:"), strong(textOutput("tit_ind")), htmlOutput("def_ind"), 
         br(), h4("Scenario Details:"), strong(textOutput("tit_scen")), htmlOutput("def_scen"), 
         br(), htmlOutput("df_warn"),
         h4("Access Data:"), downloadButton('data_dl', 'Download'), checkboxInput('isono', 'Include ISO Country Codes', FALSE)),
  tags$script("$('#linkToSelect').click(function() {
            tabs = $('.tabbable .nav.nav-tabs li a');
            $(tabs[0]).click();
            })"),
  tags$style(type='text/css', "#data_dl { width:100%}"),
  tags$head(tags$style(".table .alignRight {text-align:right; InfoThousands}"))
)

  