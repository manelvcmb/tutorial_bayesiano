library(shiny)


shinyUI(fluidPage(
    #h1("GIBBS SAMPLER"),
    absolutePanel(
      bottom = 20, right = 0, width = 300,
      draggable = TRUE,
      wellPanel(
        
        sliderInput("thin", 
                    "Número de Adelgazamiento(thinning):", 
                    value = 1,
                    min = 1, 
                    max = 10),
        
        sliderInput("pasos", 
                    "Número de Pasos:", 
                    value = 100,
                    min = 100, 
                    max = 10000)      
      ),
      style = "opacity: 0.92"
      ),
    absolutePanel(
      top = 0, left = 0, right = 0,
      fixed = TRUE,
      tabsetPanel(type = "tabs", 
                  tabPanel("2D", plotOutput("D2")), 
                  tabPanel("3D", plotOutput("D3"))
                  )
      )
     )
    )

