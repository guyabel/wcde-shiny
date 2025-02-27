library(markdown)
load("label.RData")

shinyUI(
  navbarPage(
    windowTitle = "Wittgenstein Centre Human Capital Data Explorer", 
    title = " ",  
    id = 'page', 
    collapsible = TRUE,
    fluid = FALSE,
    tabPanel(
      title = "Data Explorer",
      fluidRow(
        column(width = 8, includeMarkdown("md/intro_data.md")),
        column(width = 4, includeHTML("www/intro_logo.html"))
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
      fluidRow(
        column(width = 8, includeMarkdown("md/intro_graphic.md")),
        column(width = 4, includeHTML("www/intro_logo.html"))
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
      fluidRow(
        column(width = 8, includeMarkdown("md/intro_about.md")),
        column(width = 4, includeHTML("www/intro_logo.html"))
      ),
      # br(),
      tabsetPanel(
        type = "tabs",
         tabPanel("Details", includeMarkdown("md/about_details.md")),
        tabPanel(
          title = "FAQ",
          br(),
          h4("General:"),
          DT::DTOutput("about_faq2"),
          h4("Data and Graphic Explorer:"),
          DT::DTOutput("about_faq1")
        ),
        tabPanel(
          h4("Description:"),
          title = "Scenario Defintions",
          DT::DTOutput("about_scenario"),
          h4("Summary Component Table:"),
          includeHTML("md/scenario_table.html"),
          h4("Further Details:"),
          includeMarkdown("md/about_scenario.md")
        ),
        tabPanel(
          title = "Education Definitions",
          DT::DTOutput("about_edu"),
          includeMarkdown("md/about_edu.md")
        )
      ),
    ),
    footer = 
      tagList(
        fluidRow(
          hr(),
          column(width = 4, includeHTML("www/r_blurb.html")),
          column(width = 2, includeHTML("www/bottom_logo_adri.html")),
          column(width = 2, includeHTML("www/bottom_logo_iiasa.html")),
          column(width = 2, includeHTML("www/bottom_logo_oeaw.html")),
          column(width = 2, includeHTML("www/bottom_logo_uniwien.html"))
        ),
        includeCSS("www/style.css"),
        tags$script(
          "Shiny.addCustomMessageHandler('updateSelections',
         function(data) {
         var nav_ref = '#page a:contains(\"' + data.nav + '\")';
         var tabpanel_id = data.nav == 'Alpha' ? '#data_tabs' : '#graphic_tabs';
         $(nav_ref).tab('show');
         })"),
        tags$script(includeHTML("www/sm-share.html"))
      ),
    header = includeScript("www/google-analytics.js")
  )
)
