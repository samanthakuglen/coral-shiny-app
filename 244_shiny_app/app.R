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
                 sidebarPanel(radioButtons(inputId = "site_name",
                                           label = "Choose a site:",
                                           choices = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"))),
                 mainPanel("OUTPUT!")
             ) # end sidebarLayout
    ), #end tabPanel historical heatwave
    tabPanel("Site Data Summaries",
             sidebarLayout(
                 sidebarPanel(actionButton("action", label = "Explore Data"),
                              hr(),
                              fluidRow(column(2, verbatimTextOutput("value")))
                              ),
                 mainPanel("Data")
             ) # end sidebarLayout
    ), #end tabPanel Site Data Summaries
    tabPanel("Comparison of Site Temperature Profiles",
             sidebarLayout(
                 sidebarPanel("WIDGETS"),
                 mainPanel("OUTPUT!")
             ) # end sidebarLayout
    ) #end tabPanel Site Data Summaries)
)
)

# Define server logic 
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
