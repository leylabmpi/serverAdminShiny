library(shiny)


shinyUI(fluidPage(
  plotOutput("server_load_plot"),
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
  ),
  #plotOutput("qstat_plot"),
  #plotOutput("ps_rick_plot"),
  #plotOutput("ps_morty_plot"),
  fluidRow(
    column(1),
    column(3, numericInput('min_num_jobs', 
                           label = 'Job number cutoff',
                           value = 5,
                           min = 1,
                           step = 1)
    ),
    column(3, textInput('uname', 
                         label = 'Username',
                         value = NULL)
    )
  ),
  fluidRow(
    column(7, plotOutput("df_now_perc_plot", height='700px')),
    column(5, plotOutput("df_now_size_plot", height='700px'))
  )
))