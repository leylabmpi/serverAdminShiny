source('utils.R')

#' Reading in server load log 
server_load_log = function(file){
  x = fread(file, sep=',', header=FALSE, fill=TRUE)
  x = as.data.frame(x)
  colnames(x) = c('Time', 'IO_load')
  x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  x$IO_load = as.numeric(x$IO_load)
  x = x[!is.na(x$Time),]
  x = x[!is.na(x$IO_load),]
  return(x)
}

server_load_plot = function(x, input, ylab='Server I/O load'){
  min_time = max(x$Time) - input$num_hours * 60 * 60
  
  # filter log df
  x = x[x$Time >= min_time,]
  x$Time = as.POSIXct(x$Time)
  
  # plotting
  p = ggplot(x, aes(Time, IO_load, color=IO_load)) + 
    geom_line() +
    geom_point() +
    scale_color_continuous(low='black', high='red') +
    labs(y=ylab) +
    theme_bw() +
    theme(
      text = element_text(size=14),
      legend.position = 'none'
    )
  # return plot
  return(p)
}
