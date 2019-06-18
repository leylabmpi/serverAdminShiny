library(shiny)
library(ggplot2)

read_log = function(file){
  x = read.delim(file, sep=',', header=FALSE)
  colnames(x) = c('Time', 'IO_load')
  x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  return(x)
}

format_time = function(x){
  x = as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"))
  return(x)
}

shinyServer(function(input, output, session){
  # Reading log file
  F = '/Volumes/abt3_projects/databases/server/SERVER-LOAD-LOG.csv'
  if(! file.exists(F)){
    F = '/ebio/abt3_projects/databases/server/SERVER-LOAD-LOG.csv'
  }
  .read_log = reactiveFileReader(5000, session=session, 
                                 filePath=F, 
                                 readFunc=read_log)
  
  
  # reactive
  observe({
    # reading log 
    x = .read_log()
    min_time = format_time(min(x$Time))
    max_time = format_time(max(x$Time))
    last_hour = format_time(max_time - 60 * 60)
    last_day = format_time(max_time - 60 * 60 * 24)
    if(last_day < min_time){
      last_day = min_time
    }
    
    # slider params
    if(is.null(input$timeRangeS)){
      # defaults
      if(input$longRange == TRUE){
        min_val = last_day
      } else {
        min_val = last_hour
      }
      timeRangeS_vals = c(min_val, max_time)
    } else {
      # min time
      if (format_time(input$timeRangeS[1]) <= min_time){
        min_val = min_time
      } else {
        min_val = format_time(input$timeRangeS[1])
      }
      # max time
      if (as.numeric(max_time - input$timeRangeS[2], units='secs') < 120){
        max_val = max_time
      } else {
        max_val = format_time(input$timeRangeS[2])
      }
      timeRangeS_vals = c(min_val, max_val)
    }
    
    # sliders
    if(input$longRange == TRUE){
      min_range = min_time
    } else {
      min_range = last_day
    }
    output$timeRangeS = renderUI({
      sliderInput("timeRangeS", label = "Time range", 
                  value = timeRangeS_vals,
                  min = min_range, 
                  max = max_time)
    })
    
    # Render plot
    output$plot1 <- renderPlot({
      ## filter df
      min_time = format_time(input$timeRangeS[1])
      max_time = format_time(input$timeRangeS[2])
      x = x[x$Time >= min_time & x$Time <= max_time,]
      
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
    })
  })
  
  # Generate a new plot
  output$plot <- renderPlot({
    hist(rnorm(isolate(input$n)))
  })
})
