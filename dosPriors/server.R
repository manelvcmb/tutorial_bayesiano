library(shiny)

shinyServer(function(input, output) {
  
  passData <- reactive({
 
    
  })
  
  output$plot <- renderPlot({
    passData()
    y_lim=30
    mh <- input$historia
    N<-input$n
    p<-input$p
    a_a<-input$a_a
    a_b<-input$a_b
    b_a<-input$b_a
    b_b<-input$b_b
    x <- seq(0, 1, length = 101)
    
    outcomes<-sample(1:0,N,prob=c(p,1-p),replace=TRUE)
    success<-cumsum(outcomes)
    
    curve(dbeta(x,a_a,a_b),
          xlim=c(0,1),
          ylim=c(0,y_lim),
          col='red',
          xlab='p',
          ylab='Densidad',lty=2,lwd=4)
    
    curve(dbeta(x,success[N]+1,N-success[N]+1),
          col='green',lty=2,lwd=4,add=TRUE)
    
    curve(dbeta(x,b_a,b_b),
          col='blue',lty=2,lwd=4,add=TRUE)
    
    if (mh){
      for(i in 1:N)
          { 
              curve(dbeta(x,a_a+success[i]+1,a_b+(i-success[i])+1),
                    add=TRUE,col=rgb(100,0,0,(2.4-(N-i)/N) * 10,maxColorValue=255))
              curve(dbeta(x,b_a+success[i]+1,b_b+(i-success[i])+1),
                    add=TRUE,col=rgb(0,0,100,(2.4-(N-i)/N) * 10,maxColorValue=255))      
          }
    }
    curve(dbeta(x,a_a+success[N]+1,a_b+(N-success[N])+1),
          add=TRUE,col=rgb(100,0,0,255,maxColorValue=255),lwd=2)
    curve(dbeta(x,b_a+success[N]+1,b_b+(N-success[N])+1),
          add=TRUE,col=rgb(0,0,100,255,maxColorValue=255),lwd=2)
    
    legend('topleft',legend=c('Prior A','Prior B', 'Resultados'),
           lty=1,col=c('red','blue','green'))
    text(0.75,17,label=paste(success[N],"aciertos,",N-success[N],"fallos"))
  
  })
})
