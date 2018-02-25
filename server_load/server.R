library(shiny)
library(ggplot2)

read_log = function(){
  #f = '/Users/nick/dev/R/serverAdminShiny/server_load/SERVER-LOAD-LOG_test.csv'
  f = '/ebio/abt3_projects/databases/server/SERVER-LOAD-LOG.csv'
  x = read.delim(f, sep=',', header=FALSE)
  colnames(x) = c('Time', 'IO_load')
  x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  return(x)
}

shinyServer(function(input, output, session) {
  # Anything that calls autoInvalidate will automatically invalidate
  autoInvalidate <- reactiveTimer(30*1000)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    output$plot1 <- renderPlot({
      x = read_log() 
      # get range between the 2 sliders; short range has priority
      ## min time
      min_timeS = as.POSIXct(format(Sys.time() - 60 * 60 * 23.8, "%Y-%m-%d %H:%M:%S"))
      if(input$timeRangeS[1] < min_timeS){
        if(input$timeRangeL[1] < min_timeS){
          min_time = input$timeRangeL[1]
        } else {
          min_time = min_timeS
        }
      } else {
        min_time = input$timeRangeS[1]
      }
      ## max time
      if(input$timeRangeL[2] < input$timeRangeS[2]){
        max_time = input$timeRangeL[2]
      } else {
        max_time = input$timeRangeS[2]
      }
      x = x[x$Time >= min_time & x$Time <= max_time,]
      # plotting
      p = ggplot(x, aes(Time, IO_load, color=IO_load)) + 
        geom_line() +
        geom_point() +
        scale_color_continuous(low='black', high='red') +
        labs(y='I/O load') +
        theme_bw() +
        theme(
          text = element_text(size=14),
          legend.position = 'none'
        )
      # updating slider input
      t_diff = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")) - input$timeRangeS[2]
      if(as.numeric(t_diff, units="secs") < 120){
        valS = c(input$timeRangeS[1], as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
        valL = c(input$timeRangeL[1], as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      } else {
        valS = valL = NULL
      }
      updateSliderInput(session, "timeRangeS", value=valS,
                        min = as.POSIXct(format(Sys.time() - 60 * 60 * 24, "%Y-%m-%d %H:%M:%S")), 
                        max =  as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      updateSliderInput(session, "timeRangeL", value=valL,
                        min = as.POSIXct(format(Sys.time() - 60 * 60 * 24 * 7 * 4, "%Y-%m-%d %H:%M:%S")), 
                        max =  as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      # return plot
      return(p)
    })
  })
  
  # Generate a new histogram each time the timer fires, but not when
  output$plot <- renderPlot({
    autoInvalidate()
    hist(rnorm(isolate(input$n)))
  })
})