library(markdown)
load("label.RData")

shinyUI(
  navbarPage(
    title=" ", #footer=source("intro_foot.R", local=TRUE)$value, 
    windowTitle="Wittgenstein Centre Human Capital Data Explorer", collapsible=TRUE, fluid=FALSE,
    id='page',
    tabPanel(
      title = "Data Explorer",
      id='data_tabs',
      fluidRow(
        column(width = 8, includeMarkdown("md/intro_data.md")),
        column(width = 4, includeHTML("md/intro_logo.html"))
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel("Selection", source("ui/data_select.R", local=TRUE)$value),
        tabPanel("Data", source("ui/data_data.R", local=TRUE)$value),
        tabPanel("Assumptions", source("ui/data_assumption.R", local=TRUE)$value)
      )
      # tabPanel("Output", verbatimTextOutput("temp"))
    ),
    tabPanel(
      title = "Graphic Explorer",
      id='graphic_tabs',
      fluidRow(
        column(width = 8, includeMarkdown("md/intro_graphic.md")),
        column(width = 4, includeHTML("md/intro_logo.html"))
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Pyramids",
          br(),
          source("ui/graphic_pyr.R", local=TRUE)$value
        ),
        tabPanel(
          title = "Population Size",
          br(),
          source("ui/graphic_sac.R", local=TRUE)$value
        ),
        tabPanel(
          title = "Map",
          br(),
          source("ui/graphic_map.R", local=TRUE)$value
        )
        # tabPanel("Output", verbatimTextOutput("temp"))
      )
    ),
    tabPanel(
      title = "About",
      id='about_tabs',
      fluidRow(
        # column(width = 8, includeMarkdown("md/intro_about.md")),
        column(width = 4, includeHTML("md/intro_logo.html"), offset = 8)
      ),
      # br(),
      tabsetPanel(
        type = "tabs",
         tabPanel("Details", includeMarkdown("md/about_details.md")),
        tabPanel(
          title = "FAQ",
          br(),
          h4("General:"),
          dataTableOutput("about_faq2"),
          h4("Data and Graphic Explorer:"),
          dataTableOutput("about_faq1")
        ),
        tabPanel(
          title = "Scenario Defintions",
          dataTableOutput("about_scenario"),
          h4("Summary Component Table:"),
          includeHTML("md/scenario_table0.html"),
          includeMarkdown("md/about_scenario.md")
        ),
        tabPanel(
          title = "Education Definitions",
          dataTableOutput("about_edu"),
          includeMarkdown("md/about_edu.md")
        )
      )
    ),
   tags$footer(
     hr(),
     fluidRow(
       column(width = 4, includeHTML("md/r_blurb.html")),
       column(width = 2, includeHTML("md/bottom_logo_adri.html")),
       column(width = 2, includeHTML("md/bottom_logo_iiasa.html")),
       column(width = 2, includeHTML("md/bottom_logo_oeaw.html")),
       column(width = 2, includeHTML("md/bottom_logo_uniwien.html"))
     ),
     includeCSS("www/style.css"),
     tags$head(includeScript("www/google-analytics.js")),
     tags$script(includeHTML("www/sm-share.html")),
     tags$head(
       tags$script(
         "Shiny.addCustomMessageHandler('updateSelections',
        function(data) {
        var nav_ref = '#page a:contains(\"' + data.nav + '\")';
        var tabpanel_id = data.nav == 'Alpha' ? '#data_tabs' : '#graphic_tabs';
        $(nav_ref).tab('show');
        })"
       )
     )
   )
  )
)

