

# ###############################################################
# Modificación de un programa original de Corey Chivers, 2012   #
#                                                               #  
#             Jose Manuel Velasco Cabo, 2014.                   #
#                                                               #  
# ###############################################################

# #########################################################
# Fijamos el directorio principal 
# (modificar para su utilización en otra máquina)
#

dir.Principal <-"/home/manel/tutorial_bayesiano"

if (!file.exists(dir.Principal)){
  dir.create(file.path(dir.Principal))}

dir.Principal <-paste(dir.Principal,"/PeliculaDosPriors",sep="")

if (!file.exists(dir.Principal)){
  dir.create(file.path(dir.Principal))}

if (!file.exists(file.path(dir.Principal,'plots'))){
  dir.create(file.path(dir.Principal,'plots'))}

setwd(dir.Principal)




sim_dos_priors<-function(p=0.5,N=100,y_lim=20,a_a=2,a_b=10,b_a=8,b_b=3)
{
  
  x <- seq(0, 1, length = 21)
  outcomes<-sample(1:0,N,prob=c(p,1-p),replace=TRUE)
  success<-cumsum(outcomes)
  
  for(frame in 1:N)
  {
    png(paste("plots/",1000+frame,".png",sep=""))
    curve(dbeta(x,a_a,a_b),xlim=c(0,1),ylim=c(0,y_lim),col='red',xlab='p',ylab='Densidad',lty=2)
    curve(dbeta(x,b_a,b_b),col='blue',lty=2,add=TRUE)
    curve(dbeta(x,success[frame]+1,frame-success[frame]+1),
          col='green',lty=2,lwd=4,add=TRUE)
    for(i in 1:frame)
    {
      curve(dbeta(x,a_a+success[i]+1,a_b+(i-success[i])+1),add=TRUE,col=rgb(0,100,0,(1-(frame-i)/frame) * 100,maxColorValue=255))
      curve(dbeta(x,b_a+success[i]+1,b_b+(i-success[i])+1),add=TRUE,col=rgb(0,0,100,(1-(frame-i)/frame) * 100,maxColorValue=255))
    }
    curve(dbeta(x,a_a+success[i]+1,a_b+(i-success[i])+1),add=TRUE,col=rgb(0,100,0,255,maxColorValue=255),lwd=2)
    curve(dbeta(x,b_a+success[i]+1,b_b+(i-success[i])+1),add=TRUE,col=rgb(0,0,100,255,maxColorValue=255),lwd=2)
    
    legend('topleft',legend=c('Prior A','Prior B', 'Resultados'),lty=1,col=c('green','blue'))
    text(0.75,17,label=paste(success[i],"aciertos,",i-success[i],"fallos"))
    dev.off()
  }
  system('mencoder mf://plots/*.png -mf fps=15:type=png -ovc copy -oac copy -o dosPriors.avi')
}

sim_dos_priors()

