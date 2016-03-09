library(shiny)
library(networkD3)

shinyUI(fluidPage(
  titlePanel("CHONDRICHTHYES"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a IUCN category to display"),
      
      selectInput(inputId = "var", 
                  label = "CR = Critically endangered or EN = Endangered",
                  choices = list("CR", "EN"),
                  selected = "CR")

      ),
    
    mainPanel(
      radialNetworkOutput("force")
    )
  )
))