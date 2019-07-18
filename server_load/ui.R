library(shiny)


#shinyUI(fluidPage(
shinyUI(
  navbarPage("Compute status",
    tabPanel("Server load",
      plotOutput("server_load_plot"),
        fluidRow(
          column(1),
          column(3, numericInput('num_hours', 
                            label = 'Number of hours to display',
                            value = 4,
                            min = 1,
                            max = 24 * 7 * 4,
                            step = 1)),
          column(8)
        )
    ),
    tabPanel("Server/cluster jobs",
             fluidRow(
               column(1),
               column(3, numericInput('min_num_jobs',
                                      label = 'Job number cutoff',
                                      value = 5,
                                      min = 1,
                                      step = 1)),
               column(3, textInput('uname',
                                   label = 'Username filter',
                                   value = NULL))
             ),
            plotOutput("qstat_plot"),
            plotOutput("ps_rick_plot"),
            plotOutput("ps_morty_plot")
    ),
    tabPanel("Project sizes",
      fluidRow(
        column(3, textInput('projname',
                            label = 'Project name filter',
                            value = NULL))
      ),
      fluidRow(
        column(7, plotOutput("df_now_perc_plot", height='700px')),
        column(5, plotOutput("df_now_size_plot", height='700px'))
      )
    ),
    tabPanel("Project inodes",
             fluidRow(
               column(3, textInput('projname',
                                   label = 'Project name filter',
                                   value = NULL))
             ),
      fluidRow(
        column(7, plotOutput("df_now_iperc_plot", height='700px')),
        column(5, plotOutput("df_now_inodes_plot", height='700px'))
      )
    ),
    tabPanel("RStudio Server Dashboard",
             fluidRow(
               br(),
               column(12, htmlOutput('rstudo_dashboard'))
             ),
             fluidRow(
               column(6,
                      h5('Direct link: ', tags$a(href="http://morty.eb.local:8787/admin/dashboard", "RStudio Server Dashboard"))
               )
             )
    )
))