library(shiny)
library(ggplot2)
library(KernSmooth)
library(plot3D)

shinyServer(function(input, output) {
  
  passData <- reactive({
    
  })
  
  gibbs=function(N,thin)
  {
    mat=matrix(0,ncol=3,nrow=N)
    mat[,1]=1:N
    x=0
    y=0
    for (i in 1:N) {
      for (j in 1:thin) {
        x=rgamma(1,3,y*y+4)
        y=rnorm(1,1/(x+1),1/sqrt(x+1))
      }
      mat[i,2:3]=c(x,y)
    }
    mat=data.frame(mat)
    names(mat)=c("Iter","x","y")
    mat
  }
  
  
  fun=function(x,y)
  {
    x*x*exp(-x*y*y-y*y+2*y-4*x)
  }
  
    
  output$D2 <- renderPlot({
    passData()
  
    thin<-input$thin
    pasos<-input$pasos
   
    mat=gibbs(pasos,thin)
    par(mfrow=c(1,2))
    x=seq(0,4,0.1)
    y=seq(-2,4,0.1)
    z=outer(x,y,fun)
    contour2D(z,x,y,main="Distribución Real")
    
    fit=bkde2D(as.matrix(mat[,2:3]),c(0.1,0.1))
    contour2D(fit$fhat,fit$x1,fit$x2,main="Resultado del Gibb Sampler")
    par(mfrow=c(1,1))
    
  })
  
  output$D3 <- renderPlot({
    passData()
    
    thin<-input$thin
    pasos<-input$pasos
    
    x=seq(0,4,0.1)
    y=seq(-2,4,0.1)
    z=outer(x,y,fun)    
    par(mfrow=c(1,2))
    persp3D(x,y,z,theta=20,phi=40,col="gold",shade=0.5,main="Distribución Real ")
    
    mat=gibbs(pasos,thin)
    fit=bkde2D(as.matrix(mat[,2:3]),c(0.1,0.1))
    persp3D(fit$x1,fit$x2,fit$fhat,theta=20,phi=40,col="gold",shade=0.5,main="Resultado del Gibb Sampler")
    par(mfrow=c(1,1))
     
  })
    
})
