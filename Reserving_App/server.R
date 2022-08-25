
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  source("PD_Server.R", local=TRUE)$value 

})
