#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Historical Marine Heatwave Data in the Santa Barbara Channel"),
    tabPanel("Map of Fishery Relevant Sites",
             sidebarLayout(
                 sidebarPanel("WIDGETS"),
                 mainPanel("OUTPUT!")
             ) # end sidebarLayout
    ), #end tabPanel historical heatwave
    tabPanel("Site Data Summaries",
             sidebarLayout(
                 sidebarPanel("WIDGETS"),
                 mainPanel("OUTPUT!")
             ) # end sidebarLayout
    ), #end tabPanel Site Data Summaries
    tabPanel("Comparison of Site Temperature Profiles",
             sidebarLayout(
                 sidebarPanel("WIDGETS"),
                 mainPanel("OUTPUT!")
             ) # end sidebarLayout
    ) #end tabPanel Site Data Summaries)
) # end ui

# Define server 
server <- function(input, output) {

    output$distPlot <- renderPlot({
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
