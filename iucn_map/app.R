#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

iucn_map=function(dataset){

  library(shiny)
  library(dplyr)
  library(tidyr)
  library(googleVis)
  library(readr)
  library(fish.ecol)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("IUCN Index MapApp"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        # Dropdown menu to pick a class
         selectInput(inputId="Class",
                     label="Chose a Class",
                     choices=c("CHONDRICHTHYES",
                               "ACTINOPTERYGII",
                               "MYXINI",
                               "SARCOPTERYGII"),
                     selected="CHONDRICHTHYES"),
         # Dropdown menu to select an index
         selectInput(inputId="index",
                     label="Choose an index",
                     choices=c("iucn index",
                               "total species (by category)"),
                     selected="iucn index"),
         # Dropdown menu to select a category
         selectInput(inputId="category",
                     label="Choose a category (only for ´total species´)",
                     choices=c("DD",
                               "LC",
                               "LR/nt",
                               "NT",
                               "VU",
                               "EN",
                               "CR"),
                     selected="CR")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        htmlOutput("gvis")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$gvis=renderGvis({
      ##### FISH ECOL STARTS HERE
     
     cat=c("DD","LC","LR/nt","NT","VU","EN","CR")
     ind=seq(1,7)
     cat2val=data.frame(Category=cat, index=ind)
     
     conversion <- read_csv(file="../Data/CountryConversion.csv")
     
     
     dataset2 = dataset %>% #generating the fishDataCountries output
       left_join(cat2val, by=c("Category")) %>% #joining with cat2val by Category
       rename(ISO3=CountryISO3) %>%             #rename common column with ISO3
       left_join(conversion, by=c("ISO3")) #joining with conversion by ISO3
      
     if (input$index=="total species (by category)"){
       dataset2=dataset2 %>%
         filter(Category==input$category)
     }
     
      dataset2 = dataset2 %>%
        filter(Class==input$Class) %>% #Filter by inputs from shiny app
        group_by(World_Bank_Country,ISO2) %>% #grouping by ISO3 column
        summarize(score=iucn_index(Category, type="b"), N=n()) #Calculates a score based on our new iucn_index function
      
      
      # Plot countries, I am using the mean as an example of the index that we want to show in this map (we have to create the index) 
      
      colnames(dataset2)=c("Country", "ISO2", "score", "N") #Fix colnames
      
      #Define type of output
      if(input$index=="total species (by category)"){
       show="N"
      }
      if (input$index=="iucn index"){
        show="score"
      }
      
      #### Create a geo chart 
        gvisGeoChart(dataset2, "ISO2", show, hovervar="Country",
                     options=list(gvis.editor="S&P",
                                  colorAxis="{colors:['#91BFDB', '#FC8D59']}"))

   })
})

# Run the application 
shinyApp(ui = ui, server = server)
}

library(devtools)
#install_github(repo="fish-ecol/fish.ecol") #So that the server recognizes our functions
library(fish.ecol)
load_all("../fish.ecol")
dataset=Extantify(dir="b")
iucn_map(dataset)

