
library(shiny)

# ###################################################
# Posterior Conjugado

runApp("/home/manel/tutorial_bayesiano/teorema")

# ###################################################
# Comparación dos Priors

runApp("/home/manel/tutorial_bayesiano/dosPriors")

# ###################################################
# Crea una pelicula avi para ver la evolución de los dos Priors

source("/home/manel/tutorial_bayesiano/creaPeliDosPriors.R")


# ###################################################
# Monte Carlo para hallar PI

runApp("/home/manel/tutorial_bayesiano/piMonteCarlo")

# ###################################################
# Metodo de Buffon para calcular PI

source("/home/manel/tutorial_bayesiano/bufon.R")


# ###################################################
# Metropolis simplificado

runApp("/home/manel/tutorial_bayesiano/metropolis")


# ###################################################
# Gibbs Sampler

runApp("/home/manel/tutorial_bayesiano/gibbs")




