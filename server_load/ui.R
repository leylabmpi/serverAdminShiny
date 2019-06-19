library(shiny)


shinyUI(fluidPage(
  plotOutput("load_plot"),
  fluidRow(
    column(1),
    column(3, numericInput('num_hours', 
                            label = 'Number of hours to display',
                            value = 4,
                            min = 1,
                            max = 24 * 7 * 4,
                            step = 1),
    column(8)
    )
  )
))