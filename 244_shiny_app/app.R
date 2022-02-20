library(shiny)
library(tidyverse)
library(bslib)
library(here)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch = "sandstone"),

    navbarPage("Historical Marine Heatwave Data in the Santa Barbara Channel",
    tabPanel("Site Data Summary",
            sidebarLayout(
                sidebarPanel(actionButton("action", label = "Explore Data"),
                            hr(),
                            fluidRow(column(2, verbatimTextOutput("value")))
                            ),
                mainPanel("Data summary: \nIn order to examine temporal and spatial patterns of temperature in giant kelp forests, Santa Barbara Long Term Ecological Research (SBC-LTER) has continuously measured ambient sea water temperature at nine reef sites located along the mainland coast of the Santa Barbara Channel and two sites on the North side of Santa Cruz Island since 2000. \n
                 
                 Methods for data collection: Submersible temperature loggers (Tidbit, Onset Computer Corporation, Bourne MA) were used to measure benthic temperature at nine coastal reef and two island sites in the Santa Barbara Channel, CA. 
                 At each site, two temperature loggers were secured to the seafloor at a depth of 7 m (MLLW). Each logger was programmed to record ambient temperature at an interval of 30 minutes. Note that the two loggers were staggered by 15 minutes so the final records reflect a temperature measurement every 15 minutes. The loggers were retrieved and replaced bi-annually, in the early summer and early winter. 
                 
                 Purpose of the app:
                 Giant kelp forests are home to a wide range of organisms, however, abiotic factors, such as temperature, may affect species populations and biological processes on a scale of hours to years. Furthermore, marine heatwaves (MHWs) are predicted to increase drastically in frequency, duration, range, and intensity due to anthropogenic climate change. Studies have shown that MHWs can have vast consequences, including impacts on species abundances, biogeographic range shifts, physiology, and reduced fisheries landings. In order to help local researchers and stakeholders understand temporal and spatial patterns of oceanic temperatures, we have created this app for easy SBC-LTER temperature data visualization.   
                 
                 Data source: 
                 Reed, D., Miller, R. SBC LTER: Reef: Bottom Temperature: Continuous water temperature, ongoing since 2000 ver 26. Environmental Data Initiative. https://doi.org/10.6073/pasta/22ed009da1cf41cbf76490ab2c0c5949. Accessed 2022-02-06."
                )
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
                 sidebarPanel(dateRangeInput(inputId = "date_select", label = h3("Date range")),
                            checkboxGroupInput(inputId = "site_code",
                                                label = "Choose a site (max 3 for best visibility):",
                                               choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                               choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor")
                            ),
                 ),
                 mainPanel(plotOutput(outputId = "temp_plot"))
             ) # end sidebarLayout Map Fish Sites
    ) #end tabPanel Site Temp Profiles
)
)

# Define server logic 
server <- function(input, output) {
  site_select <- reactive({
    read_csv("sbc_lter_temp_subset.csv") %>% 
      filter(SITE %in% input$site_code)
  })# end penguin_select reactive
  output$temp_plot <- renderPlot({
    ggthemr::ggthemr('dust')
    ggplot(data = site_select(), aes(x = DATE_LOCAL, y = avg_temp))+
      geom_line(aes(color = SITE, linetype = SITE))+
      gghighlight(unhighlighted_params = list(alpha = 0.5),
                  use_direct_label = FALSE)+
      facet_wrap(~SITE)+
      scale_x_date(limits = c(input$date_select))+
      labs(x = "Date",
           y = "Average Daily Temperature (°C)",
           color = "Site",
           linetype = "Site")+
      ggtitle("Average Daily Temperature for Selected Site(s) and Dates")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
