library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

server_load_log = function(file){
  x = fread(file, sep=',', header=FALSE, fill=TRUE)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'IO_load')
  x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  return(x)
}

ps_log = function(file){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'uname', 'ppid', 'pid', 
                  'etime', 'perc_cpu', 'perc_mem')
  #x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  x = x %>%
    group_by(Time, uname) %>%
    summarize(n_jobs = n(),
              perc_cpu = sum(as.numeric(perc_cpu)),
              perc_mem = sum(as.numeric(perc_mem))) %>%
    ungroup()
  return(x)
}

qstat_log = function(file){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'X', 'JB_job_number', 'JB_name', 
                  'uname', 'JB_department', 'state', 
                  'io_usage', 'cpu_usage', 'mem_usage')
  #x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  x = x %>%
    group_by(Time, uname) %>%
    summarize(n_jobs = n(),
              io_usage = sum(io_usage),
              cpu_usage = sum(cpu_usage),
              mem_usage = sum(mem_usage)) %>%
    ungroup()
  return(x)
}

df_log = function(file){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE)
  x = as.data.frame()
  colnames(x) = c('Time', 'file', 'itotal', 'iused', 'iavail',
                  'ipcent', 'size', 'used', 'avail', 'pcent')
  #x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  return(x)
}

format_time = function(x){
  #x = as.POSIXct(f(x, "%Y-%m-%d %H:%M:%S"))
  x = as.POSIXct(as.POSIXlt(x,tz=Sys.timezone()))
  return(x)
}

which_file = function(log_file){
  vol_dir = '/Volumes/abt3_projects/databases/server/'
  vm_dir = '/ebio/abt3_projects/databases/server/'
  F = file.path(vol_dir, log_file)
  if(! file.exists(F)){
    F = file.path(vm_dir, log_file)
  }
  return(F)
}

server_load_plot = function(x, input){
  min_time = max(x$Time) - input$num_hours * 60 * 60
  
  # filter log df
  x = x[x$Time >= min_time,]
  x$Time = as.POSIXct(x$Time)
    
  # plotting
  p = ggplot(x, aes(Time, IO_load, color=IO_load)) + 
      geom_line() +
      geom_point() +
      scale_color_continuous(low='black', high='red') +
      labs(y='LUX file server I/O load') +
      theme_bw() +
      theme(
        text = element_text(size=14),
        legend.position = 'none'
    )
  # return plot
  return(p)
}

# qstat_log_plot = function(df, input){
#   df = df[df$n_jobs > input$min_num_jobs,]
#   if(! is.null(input$uname) & input$uname != ''){
#     df = df[grepl(input$uname, df$uname),]
#   }
#   df$Time = format_time(df$Time) 
#   if(nrow(df) <= 0){
#     return(NULL)
#   }
#   p = df %>%
#     gather(Metric, Value, -Time, -uname) %>%
#     ggplot(aes(Time, Value, group=uname, color=uname)) +
#     geom_line(alpha=0.25) +
#     geom_point(alpha=0.5) +
#     facet_grid(Metric ~ ., scales='free_y') +
#     theme_bw()
#   return(p)
# }

ps_log_plot = function(df, input){
  min_time = max(as.POSIXlt(df$Time)) - input$num_hours * 60 * 60
  df = df[df$Time >= min_time,]
  df$Time = as.POSIXct(df$Time)
  
  df = df[df$n_jobs > input$min_num_jobs,]
  if(! is.null(input$uname) & input$uname != ''){
    df = df[grepl(input$uname, df$uname),]
  }
  df$Time = format_time(df$Time) 
  if(nrow(df) <= 0){
    return(NULL)
  }
  p = df %>%
    gather(Metric, Value, -Time, -uname) %>%
    ggplot(aes(Time, Value, group=uname, color=uname)) +
    geom_line(alpha=0.25) +
    geom_point(alpha=0.5) +
    facet_grid(Metric ~ ., scales='free_y') +
    theme_bw()
  return(p)
}

shinyServer(function(input, output, session){

  # server load
  .server_load_log = reactiveFileReader(10000, session=session, 
                                 filePath=which_file('SERVER-LOAD-LOG.csv'), 
                                 readFunc=server_load_log)
  # ps 
  .ps_rick_log = reactiveFileReader(10000, session=session, 
                                    filePath=which_file('PS-rick-LOG.tsv'), 
                                    readFunc=ps_log)
  .ps_morty_log = reactiveFileReader(10000, session=session, 
                                    filePath=which_file('PS-morty-LOG.tsv'), 
                                    readFunc=ps_log)
  # qstat
  .qstat_log = reactiveFileReader(10000, session=session, 
                                  filePath=which_file('QSTAT-JOB-LOG.tsv'), 
                                  readFunc=qstat_log)
  
  # df 
  .df_log = reactiveFileReader(10000, session=session, 
                                  filePath=which_file('DF-LOG.tsv'), 
                                  readFunc=df_log)
  
  #-- reactive --#
  # server load log
  observe({
    output$server_load_plot <- renderPlot({
      server_load_plot(.server_load_log(), input)
    })
  })

  # PS log
  observe({
    output$qstat_plot <- renderPlot({
     ps_log_plot(.qstat_log(), input)
    })
    output$ps_rick_plot <- renderPlot({
      ps_log_plot(.ps_rick_log(), input)
    })
    output$ps_morty_plot <- renderPlot({
      ps_log_plot(.ps_morty_log(), input)
    })
  })
  
})
