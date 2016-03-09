library(shiny)
library(networkD3)
library(data.tree)
fish <- read.csv("data/taxdata.csv")

shinyServer(function(input, output) {
    output$force <- renderRadialNetwork({
    taxdata <- fish[fish$Category==input$var,]

    taxdata$pathString=paste(taxdata$Class,
                             taxdata$Order,
                             taxdata$Family,
                             taxdata$Genus,
                             taxdata$Scientific.Name,
                             sep="/")

    taxtree=as.Node(taxdata, mode="table", pathDelimiter="/", pathName="pathString")

    taxlist=ToListExplicit(taxtree, unname = TRUE) 

   
    radialNetwork(taxlist, fontSize=10, nodeColour="blue", textColour="black", opacity=1, linkColour="black")
  })
  
})