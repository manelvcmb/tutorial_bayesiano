library(shiny)


shinyUI(fluidPage(
    
  titlePanel("METROPOLIS SIMPLIFICADO"),
  
 sidebarLayout(
    sidebarPanel(

      sliderInput("cad", 
                  "Número de Cadenas:", 
                   value = 1,
                   min = 1, 
                   max = 5),
      
      sliderInput("pru", 
                  "Número de Pruebas:", 
                  value = 1000,
                  min = 1000, 
                  max = 10000)
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("TS", plotOutput("ts")), 
                  tabPanel("Density", plotOutput("density"))
      )
    )
  )
))
