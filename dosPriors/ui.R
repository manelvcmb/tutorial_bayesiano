library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("POSTERIOR CON DIFERENTES PRIORS"),
  
 sidebarLayout(
    sidebarPanel(
     
      checkboxInput("historia", "Mostrar la Evolución del Posterior", FALSE),
      
      br(),
      
      sliderInput("n", 
                  "Número de observaciones:", 
                   value = 50,
                   min = 1, 
                   max = 1000),
      
      br(),
      
      sliderInput("p", 
                  "Probabilidad p:", 
                  value = 0.5,
                  min = 0, 
                  max = 1),
      
      br(),
      
      sliderInput("a_a", 
                  "PRIOR A: Parámetro a", 
                  value = 25,
                  min = 1, 
                  max = 100),
      
      br(),
      
      sliderInput("a_b", 
                  "PRIOR A: Parámetro b", 
                  value = 75,
                  min = 1, 
                  max = 100),
      
      br(),
      
      sliderInput("b_a", 
                  "PRIOR B: Parámetro a", 
                  value = 75,
                  min = 1, 
                  max = 100),
      
      br(),
      
      sliderInput("b_b", 
                  "PRIOR B: Parámetro b", 
                  value = 25,
                  min = 1, 
                  max = 100)
      
      
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      plotOutput("plot")
      )
    )
  )
)
