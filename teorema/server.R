library(shiny)

shinyServer(function(input, output) {
  
  passData <- reactive({
    
  })
  
  output$plot <- renderPlot({
    passData()
    y_lim=20
    N<-input$n
    mh <- input$historia
    mp <- input$posterior
    md <- input$datos
    p<-input$p
    a<-input$a
    b<-input$b
    x <- seq(0, 1, length = 101)
    
    outcomes<-sample(1:0,N,prob=c(p,1-p),replace=TRUE)
    success<-cumsum(outcomes)
    
    curve(dbeta(x,a,b),
          xlim=c(0,1),
          ylim=c(0,y_lim),
          col='red',
          xlab='p',
          ylab='Densidad',lty=2,lwd=4)
    
    if (md){curve(dbeta(x,success[N]+1,N-success[N]+1), col='green',lty=2,lwd=4,add=TRUE)}
    
    if (mh){ 
        for(i in 1:N)
        { 
            curve(dbeta(x,a+success[i]+1,b+(i-success[i])+1),
                  add=TRUE,col=rgb(0,0,100,(2.4-(N-i)/N) * 10,maxColorValue=255))
            
        }
    }
    if (mp){curve(dbeta(x,a+success[N]+1,b+(N-success[N])+1), add=TRUE,col=rgb(0,0,100,255,maxColorValue=255),lwd=2)}
    
    legend('topleft',legend=c('Prior', 'Posterior','Resultados'),
           lty=1,col=c('red','blue','green'))
    text(0.80,25,label=paste(success[N],"aciertos,",N-success[N],"fallos"))
  
  })
})
