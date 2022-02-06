library(shiny)
library(tidyverse)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch = "sandstone"),

    navbarPage("Historical Marine Heatwave Data in the Santa Barbara Channel",
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
),
# Define server 
    # Application title
    titlePanel("Old Faithful Geyser Data")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
