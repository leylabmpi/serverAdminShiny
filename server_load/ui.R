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
                                      label = 'Job count cutoff',
                                      value = 5,
                                      min = 1,
                                      step = 1)),
               column(3, textInput('uname',
                                   label = 'Username filter',
                                   value = NULL)),
               column(3, numericInput('num_hours2', 
                                      label = 'Number of hours to display',
                                      value = 4,
                                      min = 1,
                                      max = 24 * 7 * 4,
                                      step = 1))
             ),
            tabsetPanel(type = "tabs",
                        tabPanel('Cluster', 
                                 plotOutput("qstat_plot", height='650px')),
                        tabPanel('rick VM',
                                 plotOutput("ps_rick_plot", height='650px')),
                        tabPanel('morty VM',
                                 plotOutput("ps_morty_plot", height='650px'))
            )
    ),
    tabPanel("Project sizes",
      fluidRow(
        column(3, textInput('dirname_disk',
                            label = 'Directory name filter',
                            value = NULL))
      ),
      tabsetPanel(type = "tabs",
                  tabPanel('abt3-projects', 
                           plotOutput("du_now_plot_abt3_projects",
                                      height='750px')),
                  tabPanel('tmp-global2', 
                           plotOutput("du_now_plot_tmp_global2")),
                  tabPanel('abt3-home', 
                           plotOutput("du_now_plot_abt3_home"))
      )
    ),
    tabPanel("Project inodes",
             fluidRow(
               column(3, textInput('dirname_inodes',
                                   label = 'Directory name filter',
                                   value = NULL))
             ),
             tabsetPanel(type = "tabs",
                         tabPanel('abt3-projects', 
                                  plotOutput("inodes_now_plot_abt3_projects",
                                             height='750px')),
                         tabPanel('tmp-global2', 
                                  plotOutput("inodes_now_plot_tmp_global2")),
                         tabPanel('abt3-home', 
                                  plotOutput("inodes_now_plot_abt3_home"))
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