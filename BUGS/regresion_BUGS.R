# #########################################################
#
# REGRESION - EJEMPLO BÁSICO
#
# Introducción a la inferencia Bayesiana con R
#
# Santiago de Compostela. Octubre 2014.
#
#             Jose Manuel Velasco
# #########################################################

# #########################################################
# Fijamos el directorio principal 
# (modificar para su utilización en otra máquina)
#

dir.Principal <-"/home/manel/tutorial_bayesiano"

if (!file.exists(dir.Principal)){
  dir.create(file.path(dir.Principal))}

dir.Principal <-paste(dir.Principal,"/BUGS",sep="")

if (!file.exists(dir.Principal)){
  dir.create(file.path(dir.Principal))}

# #########################################################
# 
# Creamos los directorios: modelos, datos y gráficas
#
# Para cada unos creamos el subdirectorio Regresion

list.nombres <- c("modelos", "datos", "graficas")

for (nombre in list.nombres){
  directorio = file.path(dir.Principal, nombre)
  
  if (!file.exists(directorio)){ dir.create(directorio)}
  
  directorio = file.path(dir.Principal, nombre, "regresion")
  
  if (!file.exists(directorio)){dir.create(directorio)}
  }


# #########################################################
# GENERAMOS LOS DATOS 

oror.real <- 10
pendiente.real <- 3

sigma <- 5

x <- seq(0, 10, by = 0.1)
y <- pendiente.real * x + oror.real
y <- y + rnorm(length(y), 0, sigma)


fich.datos.regresion <- file.path(dir.Principal,'datos', 'regresion', 'regresion.txt') 
write.csv(data.frame(X = x, Y = y),
          file = fich.datos.regresion ,
          row.names = FALSE)

png(file.path(dir.Principal, 'graficas', 'regresion', 'datos.png'))
plot(y~x, main ="DATOS GENERADOS SINTÉTICAMENTE PARA REGRESIÓN LINEAL")
dev.off()


# #########################################################
# GUARDAMOS EL MODELO 

modelo <- "model
{
  for (i in 1:N)
  {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- pendiente * x[i] + oror
  }
  
  pendiente ~ dnorm(0, 0.0001)
  oror ~ dnorm(0, 0.0001)
  
  tau ~ dgamma(0.001,0.001)
}"

fich.modelo.regresion = file.path(dir.Principal, 'modelos','regresion',"regresion.txt")
fichero = file(fich.modelo.regresion)
writeLines(modelo, fichero)
close(fichero)


# #########################################################
# ANALISIS USANDO OPENBUGS
#
# VIA BRUGS Y OPENBUGS

library(arm)
library(BRugs)
library(R2OpenBUGS)

N <- length(x)
data <- list("N","y","x")
valores.iniciales <- function(){list(pendiente=0,oror=0,tau=1)}
parametros <- list("pendiente","oror","tau")
dir.trabajo.regresion = file.path(dir.Principal,'datos','regresion')

regresion.resultado.brug <- BRugsFit(data = data, 
                                inits = valores.iniciales,
                                para = parametros, 
                                modelFile = fich.modelo.regresion,
                                numChains = 1,
                                nBurnin = 1000,
                                nIter = 4000,
                                nThin = 4,
                                coda = F,
                                working.directory = dir.trabajo.regresion)

regresion.resultado.openBUGS <- bugs(data = data,
                                inits = valores.iniciales,
                                para = parametros, 
                                model.file = fich.modelo.regresion,
                                n.chains = 4,
                                n.burnin = 1000,
                                n.iter = 4000,
                                n.thin = 1,
                                codaPkg = T,
                                working.directory = dir.trabajo.regresion)

                                
print(regresion.resultado.brug)
print(regresion.resultado.openBUGS)

geweke.diag(regresion.resultado.brug)



























