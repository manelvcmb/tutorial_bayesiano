library(shiny)

shinyServer(function(input, output) {
  
  passData <- reactive({
    
  })
  
  
    
  output$plot <- renderPlot({
    passData()
    N<-input$n
    
    
    #N <- 10000 
    R <- 1
    x <- runif(N, min= 0, max= R)
    y <- runif(N, min= 0, max= R)
    cae.dentro <- (x^2 + y^2) <= R^2
    pi.estimado <- 4 * sum(cae.dentro) / N
    pi.estimado
    
    #plot.new()
    plot.window(xlim = 1.1 * R * c(0, 1), ylim = 1.1 * R * c(0, 1))
    points(x[ cae.dentro], y[ cae.dentro], pch = '.', col = "blue")
    points(x[!cae.dentro], y[!cae.dentro], pch = '.', col = "red")
    title(main=paste("La estimacion de PI es ",pi.estimado,sep=""))
    
    
  })
})
