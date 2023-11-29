
library(shiny)
library(GCAMdashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme="style.css",
  # Application title
  titlePanel(
    div(
      column(width = 10, h2("MIT Joint Program Outlook Dashboard")),
      column(width = 2, img(src = "joint-program-logo.jpg", height = "150px")),
    ),
    windowTitle = "2023 Global Change Outlook"
  ),

  # Sidebar with user controls
  sidebarLayout(
    sidebarPanel(
      selectInput('plotScenario', 'Select Scenario to Plot', choices=list()),
      p(class = "scenario-description", textOutput('scenarioDescription')),
      radioButtons('dataType', 'Data Type', choices=list(`Emissions and Climate` = "Climate", `Energy and Economics` = "Econ")),
      selectInput('plotQuery', 'Select Data to Plot', choices=list()),
      checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
      conditionalPanel(
        condition = "input.diffCheck == true",
        selectInput('diffScenario', 'Select Difference Scenario', choices=list())
      ),
      img(src = "EPPA_regions.png", width = "100%"),
      p(class = "blurb", "Download the", a("full Outlook report", href = "https://globalchange.mit.edu/sites/default/files/newsletters/files/2023-JP-Outlook.pdf")),
      p(class = "blurb", "Download", a("data for each scenario", href = "https://globalchange.mit.edu/publications/signature/2023-global-change-outlook"), "in Excel format")
    ),

    # main display area
    mainPanel(
      div(
        class = "hoverContainer",
        plotOutput('timePlot',
                   height='600px',
                   hover = hoverOpts("exploreHover", delay = 50, delayType = 'throttle')),
        barChartHoverUI("timePlot"),
      ),
      p(textOutput("queryDescription")),
      conditionalPanel(
        condition = "output.show_breakdown_input",
        selectInput('subcategorySelect', 'Break plot down by:', choices=c('none','EPPA Region', 'Regional Group'))
      ),
      conditionalPanel(
        condition = "output.show_region_select",
        checkboxInput('tvFilterCheck', 'Limit plot to selected regions'),
        uiOutput("region_controls"),
      ),
    ) #  main Panel
  ),   # sidebar layout
  tags$footer(
    p("Website by Cypress Frankenfeld"),
    a(
      href="https://github.com/cypressf/EPPADashboard/tree/climate-outlook",
      img(src = "github-logo.png")
    ),
    p("Contribute on github")
  )
))
