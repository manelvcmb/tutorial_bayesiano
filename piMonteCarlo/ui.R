library(shiny)


shinyUI(fluidPage(
    
  titlePanel("MONTECARLO PARA HALLAR EL NUMERO PI"),
  
 sidebarLayout(
    sidebarPanel(

      sliderInput("n", 
                  "Número de pruebas:", 
                   value = 10000,
                   min = 10000, 
                   max = 300000)
      
    ),
    
    mainPanel(
      plotOutput("plot")
      )
    )
  )
)
