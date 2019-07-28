library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

source('utils.R')
source('server_load.R')
source('PS.R')
source('qstat.R')
source('usage.R')


shinyServer(function(input, output, session){

  # server load
  .server_load_log = reactiveFileReader(10000, session=session, 
                                 filePath=which_file('SERVER-LOAD-LOG.csv'), 
                                 readFunc=server_load_log)
  # ps 
  .ps_rick_log = reactiveFileReader(10000, session=session, 
                                    filePath=which_file('PS-rick-LOG.tsv'), 
                                    readFunc=ps_log,
                                    input=input)
  .ps_morty_log = reactiveFileReader(10000, session=session, 
                                    filePath=which_file('PS-morty-LOG.tsv'), 
                                    readFunc=ps_log,
                                    input=input)
  # qstat
  .qstat_log = reactiveFileReader(10000, session=session, 
                                  filePath=which_file('QSTAT-JOB-LOG.tsv'), 
                                  readFunc=qstat_log,
                                  input=input)
  # disk usage 
  .du_log = reactiveFileReader(10000, session=session,
                               filePath=which_file('disk-usage-LOG.tsv'),
                               readFunc=disk_usage_log,
                               input=input)
  # inodes
  .inodes_log = reactiveFileReader(10000, session=session,
                                   filePath=which_file('inodes-LOG.tsv'),
                                   readFunc=inodes_log,
                                   input=input)
  
  #-- reactive --#
  # server load log
  observe({
    output$server_load_plot <- renderPlot({
      server_load_plot(.server_load_log(), input)
    })
  })

  # PS & qstat logs
  observe({
    output$qstat_plot <- renderPlot({
     ps_log_plot(.qstat_log(), input, 'Cluster')
    })
    output$ps_rick_plot <- renderPlot({
      ps_log_plot(.ps_rick_log(), input, 'rick VM')
    })
    output$ps_morty_plot <- renderPlot({
      ps_log_plot(.ps_morty_log(), input, 'morty VM')
    })
  })
  
  # disk usage log
  observe({
    df = .du_log()
    output$du_now_plot_abt3_projects <- renderPlot({
      du_now_plot(df, keep_cat='abt3-projects',
                  dir_filter=input$dirname_disk)
    })
    output$du_now_plot_tmp_global2 <- renderPlot({
      du_now_plot(df, keep_cat='tmp-global2',
                  dir_filter=input$dirname_disk)
    })
    output$du_now_plot_abt3_home <- renderPlot({
      du_now_plot(df, keep_cat='abt3-home',
                  dir_filter=input$dirname_disk)
    })
  })
  
  # inodes log
  observe({
    df = .inodes_log()
    output$inodes_now_plot_abt3_projects <- renderPlot({
      du_now_plot(df, keep_cat='abt3-projects',
                  dir_filter=input$dirname_inodes)
    })
    output$inodes_now_plot_tmp_global2 <- renderPlot({
      du_now_plot(df, keep_cat='tmp-global2',
                  dir_filter=input$dirname_inodes)
    })
    output$inodes_now_plot_abt3_home <- renderPlot({
      du_now_plot(df, keep_cat='abt3-home',
                  dir_filter=input$dirname_inodes)
    })
  })
  
  # Rstudio server pro dashboard
  output$rstudio_dashboard <- renderUI({
    tags$iframe(src = 'http://morty.eb.local:8787/admin/dashboard', height=500, width=1000)
  })
})
