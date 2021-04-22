
library(shiny)
library(GCAMdashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme="style.css",
  # Application title
  titlePanel(
    div(
      column(width = 10, h2("MIT Joint Program Outlook Dashboard")),
      column(width = 2, img(src = "joint-program-logo.jpg", height = "150px"))
    ),
    windowTitle = "2021 Climate Outlook"
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
      p(class = "blurb", "Visit the MIT website for the", a("full outlook report", href = "https://globalchange.mit.edu/news-media/jp-news-outreach/charting-earth%E2%80%99s-future-21st-century")),
      p(class = "blurb", "Data is available to download in excel format")
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
      conditionalPanel(
        condition = "output.show_breakdown_input",
        selectInput('subcategorySelect', 'Break plot down by:', choices=c('none','EPPA Region', 'Regional Group'))
      ),
      checkboxInput('tvFilterCheck', 'Limit plot to selected regions'),
      uiOutput("region_controls"),
    ) #  main Panel
  )   # sidebar layout
))
