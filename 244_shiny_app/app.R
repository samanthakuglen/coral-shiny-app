# Attach packages 
library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(gghighlight)
library(yonder)

# Read in data for site map markers (Widget 2)
site_markers <- read_csv("site_locations_all.csv")

# Set bounding box coordinates for map (in Widget 2) and add red and blue map icons
sbLat <- 34.317664
sbLong <- -119.757643
sbZoom <- 9.48

red_icon <- makeIcon(
  iconUrl = "https://img.icons8.com/offices/72/marker.png",
  iconWidth = 40, iconHeight = 40)

blue_icon <- makeIcon(
  iconUrl = "https://img.icons8.com/ultraviolet/344/marker.png",
  iconWidth = 40, iconHeight = 40)

# Define UI for application 
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch = "sandstone"),
  
    # Homepage title
    navbarPage("Historical Marine Heatwave Data in the Santa Barbara Channel",
    
    # Widget 1: Site Info
    navbarMenu("About",
      tabPanel("The App",
               fluidRow(column(
                 jumbotron("Welcome!", "This app allows users to visualize benthic ocean temperature data collected at reef sites in the Santa Barbara Channel (SBC) by Santa Barbara Long Term Ecological Research (SBC LTER).",button=FALSE)),
                 br(),
                 br(),
               ),
               fluidRow(column(align="center", 
                               #imageOutput('home_image',inline = TRUE),
                               h4(HTML('Want to learn more about how these data were collected? Check out the <a href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.13" target="_blank">data repository</a>.'))
               )),
               br(),
               br(),
               HTML('<center><img src = "sbc.jpeg"></center>'),
               br(),
               br(),
               fluidRow(column(align="center",  
                               h5(HTML('Code and data used to create this Shiny app are available on <a href="https://github.com/samanthakuglen/esm-244-shiny-app" target="_blank">Github</a>.'))
              ))),  
      tabPanel("The Fisheries",
               h2("Check out the most popular local marine invertebrate seafood in the Santa Barbara Channel!"),
               sidebarLayout(
                 sidebarPanel("",
                    selectInput(inputId = "select_species", label = h3("Select box"), 
                    choices = list("California Spiny Lobster" = "California Spiny Lobster", "Red Sea Urchin" = "Red Sea Urchin", "Mediterranean Mussel" = "Mediterranean Mussel"), 
                    selected = "California Spiny Lobster"),
                imageOutput(outputId = "img")
              
                    ),
               mainPanel(
                   h3(p("Information about this species")),
                   # tags$head(tags$style(
                   #   type="text/css",
                   #   "#img img {max-width: 100%; width: 100%; height: auto}" #make image reactive to page size
                   # )),
                   # imageOutput("img")
               ))),
      tabPanel("The Authors",
               sidebarLayout(
                 sidebarPanel(
                   h2("Who are we?"),
                   p("We are graduate students at the University of California, Santa Barbara in ESM 244."),
                   br(),
                   img(src = "UC_Santa_Barbara_Wordmark_Navy_RGB.png", height = 70, width = 200),
                   br(),
                 ),
                 mainPanel(
                   h2("Germán Silva"),
                   h3(HTML('<a href= "https://german-sil.github.io/gds/" target="_blank">Personal website</a>')),
                   br(),
                   h2("Samantha Kuglen"),
                   h3(HTML('<a href= "https://samanthakuglen.github.io/" target="_blank">Personal website</a>')),
                   br(),
                   h2("Erin de Leon Sanchez"),
                   h3(HTML('<a href= "https://erindeleonsanchez.github.io/ESM-244-Website/" target="_blank">Personal website</a>'))
                 )
               )),
             ),
    
    # Widget 2: Map 
    tabPanel("Map of Fishery Relevant Sites",
             sidebarLayout(
                 sidebarPanel(radioButtons(inputId = "site_name",
                                           label = "Select a site to highlight:",
                                           choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                           choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor")
                                           )
                              ),
                 mainPanel(leafletOutput(outputId = "site_map"))
             ) # end sidebarLayout
    ), #end tabPanel historical heatwave
    
    # Widget 3: Temperature Time Series
    tabPanel("Comparison of Site Temperature Profiles",
             sidebarLayout(
                 sidebarPanel(dateRangeInput(inputId = "date_select", 
                                             label = h3("Date range"),
                                             start = "2002-08-01",
                                             end = "2021-07-26"),
                            checkboxGroupInput(inputId = "site_code",
                                               label = "Choose site(s):",
                                               choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                               choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor"),
                                               selected = c("ABUR", "AHND")
                            ),
                 ),
                 mainPanel(plotOutput(outputId = "temp_plot"))
             ) # end sidebarLayout Map Fish Sites
    ) #end tabPanel Site Temp Profiles
) # end navbarPage
) # end ui

# Define server logic 
server <- function(input, output) {
  
  # Widget 3: Reactive Input
  site_select <- reactive({
    read_csv("sbc_lter_temp_subset.csv") %>% 
      filter(SITE %in% input$site_code)
  }) # end site_select reactive
  
  # Widget 3: Output
  output$temp_plot <- renderPlot({
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
      ggtitle("Average Daily Temperature for Selected Site(s) and Dates")+
      theme(plot.title = element_text(color = "#5b4f41"), 
            plot.background = element_rect("white"), 
            panel.background = element_rect("#faf7f2"),
            panel.grid = element_line(linetype= "longdash", color = "#f0ece1"),
            axis.text = element_text(color = "#5b4f41"),
            axis.title = element_text(color = "#5b4f41"),
            strip.background = element_rect("white"),
            axis.line = element_line(color = "#5b4f41"))
  })
  
  # Widget 2: Reactive Input
  site_choose <- reactive({
    read_csv("site_locations.csv") %>%
      filter(site %in% input$site_name)
  })
  
  # Widget 3: Output 
  # All site markers appear on map in blue and the user input site appears in red
  output$site_map <- renderLeaflet({
    leaflet() %>%
      setView(lat = sbLat, lng = sbLong, zoom = sbZoom) %>%
      addTiles() %>%
      addMarkers(data = site_markers, ~long_all, ~lat_all, popup = ~site_all, label = ~site_all, icon = blue_icon) %>%
      addMarkers(data = site_choose(), ~long, ~lat, popup = ~site, label = ~site, icon = red_icon) %>% 
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  
  # Widget 1: fisheries - reactively produce image of selected species
  output$img <- renderImage({
  #   filename <- normalizePath(file.path('./www/', paste(input$n, ".jpeg", sep="")))
  #   
  #   list(src = filename,
  #        alt = paste("Image number", input$n))
  #   }, deleteFile = FALSE
  # )    
    if(input$select_species == "California Spiny Lobster"){            
    img(src = "image1.jpeg")
  }       
    else if(input$select_species == "Red Sea Urchin"){
    img(src = "image2.jpeg")
  }
    else if(input$select_species == "Mediterranean Mussel"){
    img(src = "image3.jpeg")
  }
  })
    
  
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
