library(shiny)
library(tidyverse)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch = "sandstone"),
  
  navbarPage("Historical Marine Heatwave Data in the Santa Barbara Channel",
             tabPanel("Site Data Summary",
                      sidebarLayout(
                        sidebarPanel(actionButton("action", label = "Data Summary"),
                                     hr(),
                                     fluidRow(column(2, verbatimTextOutput("value"))),
                                     actionButton("action", label = "About the Sites"),
                                     hr(),
                                     fluidRow(column(2, verbatimTextOutput("value"))),
                                     actionButton("action", label = "Relevant Marine Invertebrates"),
                                     hr(),
                                     fluidRow(column(2, verbatimTextOutput("value")))
                        ),
                        mainPanel(HTML("Data summary: <br> In order to examine temporal and spatial patterns of temperature in giant kelp forests, Santa Barbara Long Term Ecological Research (SBC-LTER) has continuously measured ambient sea water temperature at nine reef sites located along the mainland coast of the Santa Barbara Channel and two sites on the North side of Santa Cruz Island since 2000. <br> <br>
                 
                 Methods for data collection: Submersible temperature loggers (Tidbit, Onset Computer Corporation, Bourne MA) were used to measure benthic temperature at nine coastal reef and two island sites in the Santa Barbara Channel, CA. 
                 At each site, two temperature loggers were secured to the seafloor at a depth of 7 m (MLLW). Each logger was programmed to record ambient temperature at an interval of 30 minutes. Note that the two loggers were staggered by 15 minutes so the final records reflect a temperature measurement every 15 minutes. The loggers were retrieved and replaced bi-annually, in the early summer and early winter. <br> <br>
                 
                 Purpose of the app:
                 Giant kelp forests are home to a wide range of organisms, however, abiotic factors, such as temperature, may affect species populations and biological processes on a scale of hours to years. Furthermore, marine heatwaves (MHWs) are predicted to increase drastically in frequency, duration, range, and intensity due to anthropogenic climate change. Studies have shown that MHWs can have vast consequences, including impacts on species abundances, biogeographic range shifts, physiology, and reduced fisheries landings. In order to help local researchers and stakeholders understand temporal and spatial patterns of oceanic temperatures, we have created this app for easy SBC-LTER temperature data visualization. <br> <br>  
                 
                 Data source: 
                 Reed, D., Miller, R. SBC LTER: Reef: Bottom Temperature: Continuous water temperature, ongoing since 2000 ver 26. Environmental Data Initiative. https://doi.org/10.6073/pasta/22ed009da1cf41cbf76490ab2c0c5949. Accessed 2022-02-06."
                        ))
                      ) # end sidebarLayout
             ), #end tabPanel Site Data Summaries
             tabPanel("Map of Fishery Relevant Sites",
                      sidebarLayout(
                        sidebarPanel(radioButtons(inputId = "site_name",
                                                  label = "Choose a site:",
                                                  choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                                  choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor")
                        )
                        ),
                        mainPanel("Site Location Maps! Incoming...",
                                  plotOutput(outputId = "site_map"))
                      ) # end sidebarLayout
             ), #end tabPanel historical heatwave
             tabPanel("Comparison of Site Temperature Profiles",
                      sidebarLayout(
                        sidebarPanel(dateRangeInput("dates", label = h3("Date range")),
                                     fluidRow(column(4, verbatimTextOutput("value"))),
                                     checkboxGroupInput(inputId = "site_name",
                                                        label = "Choose a site:",
                                                        choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                                        choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor")
                                     ),
                                     fluidRow(column(3, verbatimTextOutput("value")))
                        ),
                        mainPanel("Output Plot! Incoming...")
                      ) # end sidebarLayout Map Fish Sites
             ), #end tabPanel Site Temp Profiles
             tabPanel("Contact Information",
                      sidebarLayout(
                        sidebarPanel(actionButton("action", label = "Germán Silva"),
                                     hr(),
                                     fluidRow(column(2, verbatimTextOutput("value"))),
                                     actionButton("action", label = "Samantha Kuglen"),
                                     hr(),
                                     fluidRow(column(2, verbatimTextOutput("value"))),
                                     actionButton("action", label = "Erin de Leon Sanchez"),
                                     hr(),
                                     fluidRow(column(2, verbatimTextOutput("value")))
                        ),
                        mainPanel("We are graduate students at the University of California, Santa Barbara. We created this Shiny App for ESM 244 in the Winter of 2022. Feel free to contact us with any questions!"
                        )
                      ) # end sidebarLayout
             )
             
             
  )
)

# Define server logic 
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
