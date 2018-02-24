library(shiny)
library(ggplot2)

read_log = function(){
  x = read.delim('/tmp/SERVER-LOAD-LOG.csv', sep=',', header=FALSE)
  colnames(x) = c('Time', 'IO_load')
  x$Time = strptime(x$Time, "%m/%d/%Y_%H:%M")
  return(x)
}

shinyServer(function(input, output, session) {
  # Anything that calls autoInvalidate will automatically invalidate
  autoInvalidate <- reactiveTimer(60*1000)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the timer fires.
    autoInvalidate()
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    output$plot1 <- renderPlot({
      x = read_log() 
      x = x[x$Time >= input$timeRangeL[1] & x$Time <= input$timeRangeL[2],]
      x = x[x$Time >= input$timeRangeS[1] & x$Time <= input$timeRangeS[2],]
      ggplot(x, aes(Time, IO_load, color=IO_load)) + 
        geom_line() +
        geom_point() +
        scale_color_continuous(low='black', high='red') +
        labs(y='I/O load') +
        theme_bw() +
        theme(
          text = element_text(size=14),
          legend.position = 'none'
        )
    })
  })
  
  # Generate a new histogram each time the timer fires, but not when
  output$plot <- renderPlot({
    autoInvalidate()
    hist(rnorm(isolate(input$n)))
  })
})