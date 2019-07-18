library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

as.Num = function(x){
  x = as.numeric(as.character(x))
  return(x)
}

#' Reading in server load log 
server_load_log = function(file){
  x = fread(file, sep=',', header=FALSE, fill=TRUE)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'IO_load')
  x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  return(x)
}

#' Reading in ps (server jobs) log 
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

#' Reading in qstat log
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
              io_usage = sum(as.Num(io_usage), na.rm=TRUE),
              cpu_usage = sum(as.Num(cpu_usage), na.rm=TRUE),
              mem_usage = sum(as.Num(mem_usage), na.rm=TRUE)) %>%
    ungroup()
  return(x)
}

#' Reading in "df" log
df_log = function(file, input){
  col_cls = c(V3 = 'numeric', V4 = 'numeric', V5 = 'numeric', 
              V6 = 'numeric', V7 = 'numeric', V8 = 'numeric')
  x = fread(file, sep='\t', header=FALSE, fill=TRUE, 
            colClasses = col_cls)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'file', 'size', 'used', 
                  'avail', 'pcent', 'itotal', 'iused')
  x$size = x$size * 1024 / 1e+12
  x$used = x$used * 1024 / 1e+12
  x$avail = x$avail * 1024 / 1e+12

  x$itotal = ifelse(x$itotal <= x$iused , x$iused, x$itotal)
  x$itotal = ifelse(x$itotal > 1e9, NA, x$itotal)
  x$iused = ifelse(x$iused > 1e9, NA, x$iused)
  x$iavail = x$itotal - x$iused
  x$ipcent = x$iused / x$itotal * 100.0
  x$iavail = x$itotal - x$iused
  x$iavail = ifelse(x$iavail > 1e12, NA, x$iavail)
  
  x$itotal = x$itotal / 1e6
  x$iused = x$iused / 1e6
  x$iavail = x$iavail / 1e6
  
  # filter
  if(! is.null(input$projname) & input$projname != ''){
    x = x[grepl(input$projname, basename(x$file)),]
  }
  return(x)
}

format_time = function(x){
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

ps_log_plot = function(df, input, plot_title){
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
    scale_color_discrete('Username') +
    facet_grid(Metric ~ ., scales='free_y') +
    labs(title=plot_title) +
    theme_bw() +
    theme(
      axis.title.y = element_blank()
    )
  return(p)
}

df_log_format = function(df, input){
  df = df[df$Time == max(df$Time),]
  to_rm = c('collectl', 'disk-usage', 'server-load', 'small_projects')
  df = df %>%
    mutate(file = gsub('/ebio/abt3_projects/', '', df$file)) %>%
    filter(!grepl('^\\.', file),
           ! file %in% to_rm) %>%
    distinct(Time, file, .keep_all=TRUE) 
  return(df)
}

df_log_now_perc_plot = function(df, input){
  df = df_log_format(df, input)
  
  # plotting
  p = df %>%
    dplyr::select(file, pcent) %>%
    rename('Perc_Used' = pcent) %>%
    mutate(file = reorder(file, Perc_Used)) %>%
    ggplot(aes(file, Perc_Used, fill=Perc_Used)) + 
    geom_bar(stat='identity') +
    scale_y_continuous(limits=c(0, 100)) +
    scale_fill_continuous(low='black', high='red') +
    coord_flip() +
    labs(x='/ebio/abt3_projects/', y='% size used') +
    theme_bw() +
    theme(
      legend.position = 'none'
    )
  return(p)
}

df_log_now_iperc_plot = function(df, input){
  df = df_log_format(df, input)
  
  # plotting
  p = df %>%
    dplyr::select(file, ipcent) %>%
    rename('Perc_Used' = ipcent) %>%
    mutate(file = reorder(file, Perc_Used)) %>%
    ggplot(aes(file, Perc_Used, fill=Perc_Used)) + 
    geom_bar(stat='identity') +
    scale_y_continuous(limits=c(0, 100)) +
    scale_fill_continuous(low='black', high='red') +
    coord_flip() +
    labs(x='/ebio/abt3_projects/', y='% inodes used') +
    theme_bw() +
    theme(
      legend.position = 'none'
    )
  return(p)
}

df_log_now_size_plot = function(df, input){
  df = df_log_format(df, input)
  
   Mt = c('Avail. (Tb)', 'Used (Tb)')
   RN = data.frame(old_name = c('avail', 'used'),
                   Metric = factor(Mt, levels=Mt))
  
  # plotting
  p = df %>%
    dplyr::select(file, avail, used, pcent) %>%
    gather(old_name, Value, -file, -pcent) %>%
    inner_join(RN, 'old_name') %>%
    mutate(file = reorder(file, pcent)) %>%
    ggplot(aes(file, Value, fill=Metric)) + 
    geom_bar(stat='identity') +
    scale_fill_manual(values=c('blue', 'red')) +
    coord_flip() +
    labs(y='Available [blue] & Used [red]  (Tb)') +
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
  return(p)
}

df_log_now_inodes_plot = function(df, input){
  df = df_log_format(df, input)
  
  Mt = c('Available', 'Used')
  RN = data.frame(old_name = c('iavail', 'iused'),
                  Metric = factor(Mt, levels=Mt))
  
  # plotting
  p = df %>%
    dplyr::select(file, iavail, iused, ipcent) %>%
    gather(old_name, Value, -file, -ipcent) %>%
    inner_join(RN, 'old_name') %>%
    mutate(file = reorder(file, ipcent)) %>%
    ggplot(aes(file, Value, fill=Metric)) + 
    geom_bar(stat='identity') +
    scale_fill_manual(values=c('blue', 'red')) +
    coord_flip() +
    labs(y='Available [blue] & Used [red]  (mil. inodes)') +
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
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
                               readFunc=df_log,
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
  
  # df log
  observe({
    df = .df_log()
    output$df_now_perc_plot <- renderPlot({
      df_log_now_perc_plot(df, input)
    })
    output$df_now_size_plot <- renderPlot({
      df_log_now_size_plot(df, input)
    })
    output$df_now_iperc_plot <- renderPlot({
      df_log_now_iperc_plot(df, input)
    })
    output$df_now_inodes_plot <- renderPlot({
      df_log_now_inodes_plot(df, input)
    })
  })
  
  # Rstudio server pro dashboard
  output$rstudio_dashboard <- renderUI({
    tags$iframe(src = 'http://morty.eb.local:8787/admin/dashboard', height=500, width=1000)
  })
})
