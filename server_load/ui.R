library(shiny)


shinyUI(fluidPage(
  plotOutput("plot1"),
  fluidRow(
    column(1),
    column(10, uiOutput('timeRangeS'),
    column(1)
    )
  ),
  fluidRow(
    column(1),
    column(10, checkboxInput('longRange', 'Show full time series?', FALSE), 
    column(1)
    )
  )
))