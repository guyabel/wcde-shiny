tagList(
  br(),
  fluidRow(class = "myRow1", 
    column(
      width = 3, 
      br(), 
      h4("1. Indicators"), 
      br(), 
      selectizeInput(
        inputId = 'data_cat', label = 'Indicator Type',
        choices = ind0, selected = ind0[[1]][1],
        width="100%"
      ),
      uiOutput("data_ind")
      # selectizeInput(
      #   inputId = 'data_ind', label = 'Indicator', 
      #   choices = ind1, selected = ind1[1], 
      #   width="100%"
      # )
    ), 
    column(
      width = 3, 
      br(), 
      h4("2. Geography"), 
      br(),
      uiOutput("nat0"),
      uiOutput("reg0"),
      # selectizeInput(
      #   inputId = "nat", label = "Country", 
      #   choices = nn2, 
      #   multiple = TRUE, width="100%", 
      #   options = list(placeholder = 'Type or click for countries')
      # ),
      # selectizeInput(
      #   inputId = "reg", label = "Region", 
      #   choices = geo2,  
      #   multiple = TRUE, width="100%", 
      #   options = list(placeholder = 'Type or click for countries')
      # ),
      checkboxInput(
        inputId = 'regnat', label = 'Include countries of selected regions', 
        value = FALSE
      )
    ),
    column(
      width = 3, 
      br(), 
      h4("3. Breakdown"),  
      br(),
      uiOutput("sex0"), 
      uiOutput("age0"), 
      # selectizeInput(
      #   inputId = "sex", label = "Sex", 
      #   choices = NULL, multiple = TRUE, 
      #   width = "100%", 
      #   options = list(placeholder = 'Type or click for multiple options')
      # ),
      # selectizeInput(
      #   inputId = "age", label = "Age", 
      #   choices = NULL, multiple = TRUE, 
      #   width = "100%", 
      #   options = list(placeholder = 'Type or click for multiple options')
      # ),
      checkboxInput('allage', 'Include all age groups', FALSE)
    ),
    column(
      width = 3, 
      br(),
      h4("4. Time Horizon"),
      br(),
      uiOutput("scenario0"), 
      uiOutput("year0"), 
      # selectizeInput(
      #   inputId = "scenario", label = "Scenario", 
      #   choices = NULL, multiple = TRUE, 
      #   width = "100%", 
      #   options = list(placeholder = 'Type or click for multiple options')
      # ),
      # selectizeInput(
      #   inputId = "year", label = "Year", 
      #   choices = NULL, multiple = TRUE, 
      #   width = "100%", 
      #   options = list(placeholder = 'Type or click for multiple options')
      # ),
      checkboxInput('allyear', 'Include all times', FALSE),
      br(),
      HTML("
        <div id='linkToData'>
        <button type='button' class='btn btn-block btn-default'>
           View Data 
          <span class='glyphicon glyphicon-new-window'></span>
        </button>
        </div>"
      ), 
      br(),
      downloadButton('data_dl0', 'Download'),
      br(), 
      br()
    )
  ),
  tags$script("$('#linkToData').click(function() {
            tabs = $('.tabbable .nav.nav-tabs li a');
            $(tabs[1]).click();
            })"),
  tags$style(type='text/css', "#data_dl0 { width:100%}"),
  tags$style(".myRow1{background: rgba(212,228,239,1); 
               background: -moz-linear-gradient(left, rgba(212,228,239,1) 0%, rgba(44,146,208,1) 100%);
               background: -webkit-gradient(left top, right top, color-stop(0%, rgba(212,228,239,1)), color-stop(100%, rgba(44,146,208,1)));
               background: -webkit-linear-gradient(left, rgba(212,228,239,1) 0%, rgba(44,146,208,1) 100%);
               background: -o-linear-gradient(left, rgba(212,228,239,1) 0%, rgba(44,146,208,1) 100%);
               background: -ms-linear-gradient(left, rgba(212,228,239,1) 0%, rgba(44,146,208,1) 100%);
               background: linear-gradient(to right, rgba(212,228,239,1) 0%, rgba(44,146,208,1) 100%);
               filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#d4e4ef', endColorstr='#2c92d0', GradientType=1 );
               border-radius: 10px 10px 10px 10px;
               -moz-border-radius: 10px 10px 10px 10px;
               -webkit-border-radius: 10px 10px 10px 10px;}"
             ),
  br(),
  br()
)
