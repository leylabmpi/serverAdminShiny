library(shiny)


shinyUI(fluidPage(
  plotOutput("plot1"),
  fluidRow(
    column(1),
    column(10,
           sliderInput("timeRangeS", label = "Short time range", width=800,
                       min = as.POSIXct(format(Sys.time() - 60 * 60 * 24, "%Y-%m-%d %H:%M:%S")),
                       max = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                       value = c(
                         as.POSIXct(format(Sys.time() - 60 * 60 * 1, "%Y-%m-%d %H:%M:%S")),
                         as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
                       )
           ),
           column(1)
    )
  ),
  fluidRow(
    column(1),
    column(10,
      sliderInput("timeRangeL", label = "Long time range", width=800,
                min = as.POSIXct(format(Sys.time() - 60 * 60 * 24 * 7 * 4, "%Y-%m-%d %H:%M:%S")),
                max = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                value = c(
                  as.POSIXct(format(Sys.time() - 60 * 60 * 24, "%Y-%m-%d %H:%M:%S")),
                  as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
                )
     ),
    column(1)
    )
  )
      
  
  # pageWithSidebar(
  #   titlePanel("LUX I/O load"),
  #   sidebarPanel( width = 3,
  #     sliderInput("timeRange", label = "Time range",
  #                 min = as.POSIXct(format(Sys.time() - 60 * 60 * 24 * 7 * 4, "%Y-%m-%d %H:%M:%S")),
  #                 max = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  #                 value = c(
  #                   as.POSIXct(format(Sys.time() - 60 * 60 * 24 * 7 * 4, "%Y-%m-%d %H:%M:%S")),
  #                   as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  #                 )
  #     )
  #   ),
  #   mainPanel(
  #     plotOutput("plot1")
  #   )
  # )
))