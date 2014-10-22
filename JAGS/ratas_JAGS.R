# #########################################################
#
# REGRESION - DATOS RATAS - JAGS
#
# EJEMPLOS DE BUGS. VOL I 
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
#
# Modelo en una funcion

ratas.model <- function()  {
		for( i in 1 : N ) {
			for( j in 1 : T ) {
				Y[i , j] ~ dnorm(mu[i , j],tau.c)
				mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
			}
			alpha[i] ~ dnorm(alpha.c,tau.alpha)
			beta[i] ~ dnorm(beta.c,tau.beta)
		}
		tau.c ~ dgamma(0.001,0.001)
		sigma <- 1 / sqrt(tau.c)
		alpha.c ~ dnorm(0.0,1.0E-6)	 
		sigma.alpha~ dunif(0,100)
		sigma.beta~ dunif(0,100)
		tau.alpha<-1/(sigma.alpha*sigma.alpha)
		tau.beta<-1/(sigma.beta*sigma.beta)
		beta.c ~ dnorm(0.0,1.0E-6)
		alpha0 <- alpha.c - xbar * beta.c	
	}

# #########################################################
#
# Datos

ratas.data  <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,	
		Y = structure(
			.Data =   c(151, 199, 246, 283, 320,
							 145, 199, 249, 293, 354,
							 147, 214, 263, 312, 328,
							 155, 200, 237, 272, 297,
							 135, 188, 230, 280, 323,
							 159, 210, 252, 298, 331,
							 141, 189, 231, 275, 305,
							 159, 201, 248, 297, 338,
							 177, 236, 285, 350, 376,
							 134, 182, 220, 260, 296,
							 160, 208, 261, 313, 352,
							 143, 188, 220, 273, 314,
							 154, 200, 244, 289, 325,
							 171, 221, 270, 326, 358,
							 163, 216, 242, 281, 312,
							 160, 207, 248, 288, 324,
							 142, 187, 234, 280, 316,
							 156, 203, 243, 283, 317,
							 157, 212, 259, 307, 336,
							 152, 203, 246, 286, 321,
							 154, 205, 253, 298, 334,
							 139, 190, 225, 267, 302,
							 146, 191, 229, 272, 302,
							 157, 211, 250, 285, 323,
							 132, 185, 237, 286, 331,
							 160, 207, 257, 303, 345,
							 169, 216, 261, 295, 333,
							 157, 205, 248, 289, 316,
							 137, 180, 219, 258, 291,
							 153, 200, 244, 286, 324),
						.Dim = c(30,5)))
						
# #########################################################
# Guardamos datos en variables

Y <- ratas.data$Y
T <- ratas.data$T
x <-  ratas.data$x
xbar <- ratas.data$xbar
N <- ratas.data$N

# #########################################################
# Nombres 

ratas.data  <- list("Y", "x", "T", "N", "xbar")

# #########################################################
# Parametros

ratas.params <- c("tau.c", "sigma", "alpha.c", "sigma.alpha", "sigma.beta", "tau.alpha", "tau.beta", "beta.c", "alpha0")


# #########################################################
#
# Valores iniciales

ratas.inits <- function(){
	list("alpha"=c(250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250), 
		"beta"=c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6), 
		"alpha.c"=c(150), "beta.c"=c(10), "tau.c"=c(1), "sigma.alpha"=c(1), "sigma.beta"=c(1))
}


# #########################################################
#
# Ejecucion de las cadenas

ratasfit <- jags(data=ratas.data, inits=ratas.inits, ratas.params, n.chains=2, n.iter=2000, n.burnin=1000, model.file=ratas.model)



print(ratasfit)

plot(ratasfit)

traceplot(ratasfit)

pdf("ratas.trace.pdf")
traceplot(ratasfit)
dev.off()

ratasfit.mcmc <- as.mcmc(ratasfit)

summary(ratasfit.mcmc)

xyplot(ratasfit.mcmc, layout=c(2,6), aspect="fill")


densityplot(ratasfit.mcmc, layout=c(2,6), aspect="fill") 


autocorr.plot(ratasfit.mcmc)

# Diagnosis

gelman.plot(ratasfit.mcmc)
geweke.diag(ratasfit.mcmc)
geweke.plot(ratasfit.mcmc)
raftery.diag(ratasfit.mcmc)
heidel.diag(ratasfit.mcmc)


ratasfit <- jags2(data=ratas.data, inits=ratas.inits, ratas.params, n.iter=5000, model.file=ratas.model)

print(ratasfit)
plot(ratasfit)
