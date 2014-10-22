library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  passData <- reactive({
    
  })
  
  calculoDF <- function(num.pruebas){
        num.cadenas=5
        alpha <-1
        cad <- 1:num.cadenas    
           
        vec <- vector("numeric", num.pruebas*num.cadenas)
        tiempo <- vector("numeric", num.pruebas*num.cadenas)
        
        ind =1
        for (j in 1:num.cadenas){
          x <- 0
          vec[ind] <- x
          tiempo[ind] <- x
          for (i in 2:num.pruebas) {
            innov <- runif(1, -alpha, alpha)
            can <- x + innov
            aprob <- min(1, dnorm(can)/dnorm(x))
            u <- runif(1)
            if (u < aprob) 
              x <- can
            vec[ind] <- x
            tiempo[ind] <- i
            ind <- ind+1
          }
        }
        df <- data.frame(tiempo=tiempo, cond = factor( rep(cad, each=num.pruebas) ), 
                         cadena = vec)
        df
  }
  
  
    
  output$ts <- renderPlot({
    passData()
  
    num.cadenas<-input$cad
    num.pruebas<-input$pru
    
    df <- calculoDF(num.pruebas)
    
    df.sub <- df[1:(num.cadenas*num.pruebas),]
      
    ggplot(df.sub,aes(x=tiempo,y=cadena,colour=cond)) + geom_line()
    
    
  })
  
  output$density <- renderPlot({
    passData()
    
    num.cadenas<-input$cad
    num.pruebas<-input$pru
    
    df <- calculoDF(num.pruebas)
    df.sub <- df[1:(num.cadenas*num.pruebas),]
    
    ggplot(df.sub, aes(x=cadena, fill=cond)) + geom_density(alpha=.3)
    
  })
    
})
