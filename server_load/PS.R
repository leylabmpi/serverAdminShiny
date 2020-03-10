source('utils.R')

#' Reading in ps (server jobs) log 
ps_log = function(file, input){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'uname', 'ppid', 'pid', 
                  'etime', 'perc_cpu', 'perc_mem')
  x$Time = strptime(x$Time, "%Y-%m-%d %H:%M:%S")
  
  # filter
  min_time = max(x$Time) - input$num_hours2 * 60 * 60
  x = x[x$Time >= min_time,]
  x$Time = as.character(x$Time)
  
  # summarizing
  x = x %>%
    group_by(Time, uname) %>%
    summarize(n_jobs = n(),
              perc_cpu = sum(as.numeric(perc_cpu)),
              perc_mem = sum(as.numeric(perc_mem))) %>%
    ungroup()
  return(x)
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
  df = filter(df, !is.na(Time))
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
