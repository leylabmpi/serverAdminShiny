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
    min_time = max(x$Time) - input$num_hours * 60 * 60
    
    # Render plot
    output$load_plot <- renderPlot({
      ## filter log df
      x = x[x$Time >= min_time,]
      
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
})
