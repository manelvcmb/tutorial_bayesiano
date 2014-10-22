library(shiny)


shinyUI(fluidPage(
    
  titlePanel("POSTERIOR CONJUGADO"),
  
 sidebarLayout(
    sidebarPanel(
  
      checkboxInput("historia", "Mostrar la Evolución del Posterior", FALSE),
      checkboxInput("posterior", "Mostrar Posterior", FALSE),
      checkboxInput("datos", "Mostrar Verosimilitud (datos)", FALSE),
      
      br(),
      
      sliderInput("n", 
                  "Número de observaciones:", 
                   value = 10,
                   min = 1, 
                   max = 100),
      
      br(),
      
      sliderInput("p", 
                  "Probabilidad p:", 
                  value = 0.5,
                  min = 0, 
                  max = 1),
      
      br(),
      
      sliderInput("a", 
                  " PRIOR: Parámetro a", 
                  value = 5,
                  min = 1, 
                  max = 100),
      
      br(),
      
      sliderInput("b", 
                  " PRIOR: Parámetro b", 
                  value = 5,
                  min = 1, 
                  max = 100)
    ),
    
    mainPanel(
      plotOutput("plot")
      )
    )
  )
)
