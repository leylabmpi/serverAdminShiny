library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(plotly)

source('utils.R')
source('server_load.R')
source('PS.R')
source('qstat.R')
source('usage.R')
source('node_FS_mount.R')


shinyServer(function(input, output, session){

  # lux server load
  .server_load_log = reactiveFileReader(10000, session=session, 
                                 filePath=which_file('SERVER-LOAD-LOG.csv'), 
                                 readFunc=server_load_log)
  .server_load_log_abt3scratch = reactiveFileReader(10000, session=session, 
                                                    filePath=which_file('SERVER-LOAD-LOG_abt3-scratch.csv'), 
                                                    readFunc=server_load_log)
  .server_load_log_tmp_global2 = reactiveFileReader(10000, session=session, 
                                        filePath=which_file('SERVER-LOAD-LOG_tmp-global2.csv'), 
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
  
  # server file system mount
  .node_FS_log = reactiveFileReader(10000, session=session,
                                   filePath=which_file('node_FS_mount.txt'),
                                   readFunc=node_FS_log,
                                   input=input)
  
  #-- reactive --#
  # server load log
  observe({
    output$server_load_plot <- renderPlot({
      server_load_plot(.server_load_log(), input, ylab='LUX file server I/O load')
    })
    output$server_load_plot_abt3scratch <- renderPlot({
      server_load_plot(.server_load_log_abt3scratch(), input, ylab='/ebio/abt3_scratch/ file server I/O load')
    })
    output$server_load_plot_tmp_global2 <- renderPlot({
      server_load_plot(.server_load_log_tmp_global2(), input, ylab='/tmp/global2/ file server I/O load')
    })
  })

  # PS & qstat logs
  observe({
    output$qstat_plot = renderPlotly({
     ps_log_plot(.qstat_log(), input, 'Cluster')
    })
    output$ps_rick_plot = renderPlotly({
      ps_log_plot(.ps_rick_log(), input, 'rick VM')
    })
    output$ps_morty_plot = renderPlotly({
      ps_log_plot(.ps_morty_log(), input, 'morty VM')
    })
  })
  
  # disk usage log
  observe({
    df = .du_log()
    output$du_now_plot_abt3_projects = renderPlotly({
      du_now_plot(df, keep_cat='abt3-projects',
                  dir_filter=input$dirname_disk)
    })
    output$du_now_plot_abt3scratch = renderPlotly({
      du_now_plot(df, keep_cat='abt3-scratch',
                  dir_filter=input$dirname_disk)
    })
    output$du_now_plot_tmp_global2 = renderPlotly({
      du_now_plot(df, keep_cat='tmp-global2',
                  dir_filter=input$dirname_disk)
    })
    output$du_now_plot_abt3_home = renderPlotly({
      du_now_plot(df, keep_cat='abt3-home',
                  dir_filter=input$dirname_disk)
    })
  })
  
  # inodes log
  observe({
    df = .inodes_log()
    output$inodes_now_plot_abt3_projects = renderPlotly({
      du_now_plot(df, keep_cat='abt3-projects',
                  dir_filter=input$dirname_inodes)
    })
    output$inodes_now_plot_abt3scratch = renderPlotly({
      du_now_plot(df, keep_cat='abt3-scratch',
                  dir_filter=input$dirname_inodes)
    })
    output$inodes_now_plot_tmp_global2 = renderPlotly({
      du_now_plot(df, keep_cat='tmp-global2',
                  dir_filter=input$dirname_inodes)
    })
    output$inodes_now_plot_abt3_home = renderPlotly({
      du_now_plot(df, keep_cat='abt3-home',
                  dir_filter=input$dirname_inodes)
    })
  })
  
  # node file system mount
  observe({
    output$node_FS_mount_plot = renderPlotly({
      node_FS_plot(.node_FS_log())
    })
  })
  
  # Rstudio server pro dashboard
  output$rstudio_dashboard <- renderUI({
    tags$iframe(src = 'http://morty.eb.local:8787/admin/dashboard',
                height=500, width=1000)
  })
})
