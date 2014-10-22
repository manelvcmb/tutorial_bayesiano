
# #########################################################
#
# REGRESION - EJEMPLO BÁSICO - JAGS
#
# Introducción a la inferencia Bayesiana con R
#
# Santiago de Compostela. Octubre 2014.
#
#             Jose Manuel Velasco
# #########################################################

# #########################################################
#
# LIBRERIAS USADAS

library('rjags')
library('R2jags')
library(ggmcmc)

# #########################################################
# Fijamos el directorio principal 
# (modificar para su utilización en otra máquina)
#

dir.Principal <-"/home/manel/tutorial_bayesiano"

if (!file.exists(dir.Principal)){
  dir.create(file.path(dir.Principal))}

dir.Principal <-paste(dir.Principal,"/JAGS",sep="")

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
N <- length(x)


fich.datos.regresion <- file.path(dir.Principal,'datos', 'regresion', 'regresion.csv') 
write.csv(data.frame(X = x, Y = y),
          file = fich.datos.regresion ,
          row.names = FALSE)

png(file.path(dir.Principal, 'graficas', 'regresion', 'datos.png'))
plot(y~x, main ="DATOS GENERADOS SINTÉTICAMENTE PARA REGRESIÓN LINEAL")
dev.off()


# #########################################################
# PRIMERA OPCION
# GUARDAMOS EL MODELO EN UN FICHERO
# oror --> ORdenada en el ORigen 

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

fich.modelo.regresion = file.path(dir.Principal, 'modelos','regresion',"regresion.bugs")
fichero = file(fich.modelo.regresion)
writeLines(modelo, fichero)
close(fichero)


# #########################################################
# SEGUNDA OPCION
# GUARDAMOS EL MODELO EN UNA FUNCION 


modelo.bugs <- function()  {
  for (i in 1:N)
  {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- pendiente * x[i] + oror
  }
  
  pendiente ~ dnorm(0, 0.0001)
  oror ~ dnorm(0, 0.0001)
  
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 10000)
}


# #########################################################
# DOS OPCIONES PARA CARGAR EL MODELO
#     1 - LEYENDO EL FICHERO .BUGS
#     2 - USANDO UNA FUNCION
#
# DOS OPCIONES PARA VISUALIZAR LAS CADENAS MCMC
#     1 - CODA.SAMPLES
#     2 - AS.MCMC

datos.df <- read.csv(file.path(dir.Principal,'datos','regresion','regresion.csv'))

fichero.modelo = file.path(dir.Principal,
                           'modelos',
                           'regresion',
                           'regresion.bugs')


modelo.jags1 <- jags.model(file=fichero.modelo,
                               data = list('x' = with(datos.df, X),
                                           'y' = with(datos.df, Y),
                                           'N' = nrow(datos.df)),
                               n.chains = 4,
                               n.adapt = 1000)

modelo.jags1.samples <- coda.samples(modelo.jags1,
                             c('pendiente', 'oror', 'tau'),
                             1000)


modelo.jags2 <- jags(data=list('x' = with(datos.df, X),
                               'y' = with(datos.df, Y),
                               'N' = nrow(datos.df)), 
                           inits=NULL,
                           parameters.to.save = c("pendiente","oror","tau"), 
                           n.chains=4, 
                           n.iter=5000, 
                           n.burnin=1000, 
                           model.file=modelo.bugs)

modelo.jags2.mcmc <- as.mcmc(modelo.jags2)



# #########################################################
# VEMOS RESULTADOS Y COMPARAMOS CON UNA REGRESION LINEAL

summary(modelo.jags1.samples)
print(modelo.jags2)
lm(Y ~ X, data = datos.df)

plot(modelo.jags2)
plot(modelo.jags1.samples)


# #########################################################
# VISUALIZACION DE LAS CADENAS
# A PARTIR DEL MODELO JAGS

traceplot(modelo.jags2)

xyplot(modelo.jags2.mcmc )

# #########################################################
# DISTINTOS TIPOS DE TEST

gelman.plot(modelo.jags2.mcmc )
gelman.diag(modelo.jags2.mcmc )

geweke.diag(modelo.jags2.mcmc )
geweke.plot(modelo.jags2.mcmc )

raftery.diag(modelo.jags2.mcmc )
heidel.diag(modelo.jags2.mcmc )



# ###########################################################
# GGMCMC

jags.ggmcmc <- ggs(modelo.jags1.samples)

ggmcmc(jags.ggmcmc)

pdf(file = "analisis_ggmcmc_jags.pdf")

ggmcmc(jags.ggmcmc, file = NULL)

dev.off()


# ###########################################################
# intervalos HPD

ggs_caterpillar(jags.ggmcmc)




















