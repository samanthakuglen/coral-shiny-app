# Attach packages 
library(shiny)
library(tidyverse)
library(bslib)
library(leaflet)
library(gghighlight)
library(yonder)
library(ggbeeswarm)

### Setup for Widget 2
# Read in data for site map markers 
site_markers <- read_csv("site_locations_all.csv")

# Set bounding box coordinates for map 
sbLat <- 34.317664
sbLong <- -119.757643
sbZoom <- 9.48

# Create red and blue map icons from online images
red_icon <- makeIcon(
  iconUrl = "https://img.icons8.com/offices/72/marker.png",
  iconWidth = 40, iconHeight = 40)

blue_icon <- makeIcon(
  iconUrl = "https://img.icons8.com/ultraviolet/344/marker.png",
  iconWidth = 40, iconHeight = 40)

### Define UI for application 
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bootswatch = "flatly"),
  
#   Homepage Title
    navbarPage("Historical Seawater Temperature Data in the Santa Barbara Channel",
    
#   Widget 1: Site About Info
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
               h3("Check out the most popular local invertebrate seafood in the Santa Barbara Channel!"),
               sidebarLayout(
                 sidebarPanel("",
                      actionLink("species1", "California Spiny Lobster"),
                      br(),
                      actionLink("species2", "Red Sea Urchin"), 
                      br(), 
                      actionLink("species3", "Mediterranean Mussel")
                    ),
               mainPanel(
                   imageOutput("display"),
                   textOutput("information"),
                   uiOutput("link")
               ))),
      tabPanel("The Data",
               fluidRow(column(
                 jumbotron("It is important to explore your data prior to analyzing it. One way that this can be accomplished through exploratory data visualization like histograms and box plots. By exploring data this way we are able to make decisions on what type of analysis approaches are appropriate for the data.",button=FALSE)),
                 br(),
                 br(),
               ),
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(inputId = "reef_code",
                                      label = h3("Choose Site(s):"),
                               choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                               choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor"),
                               selected = "ABUR") # end select box input
                 ), # end sidebar Panel
                 mainPanel(plotOutput(outputId = "boxplot"),
                           plotOutput(outputId = "histo_plot"))
               ) #end sidebar Layout
               ),# end tab panel
      tabPanel("The Authors",
               sidebarLayout(
                 sidebarPanel(
                   h2("Who are we?"),
                   p("We are graduate students at the University of California, Santa Barbara in ESM 244."),
                   br(),
                   img(src = "UC_Santa_Barbara_Wordmark_Navy_RGB.png", height = 70, width = 200)
                 ),
                 mainPanel(
                   h3(HTML('<a href= "https://german-sil.github.io/gds/" target="_blank">Germán Silva</a>')),
                   img(src = "german.jpg", height = 200, width = 150),
                   br(),
                   br(),
                   h3(HTML('<a href= "https://samanthakuglen.github.io/personal-website/" target="_blank">Samantha Kuglen</a>')),
                   img(src = "sam.jpg", height = 130, width = 150),
                   br(),
                   br(),
                   h3(HTML('<a href= "https://erindeleonsanchez.github.io/ESM-244-Website/" target="_blank">Erin de Leon Sanchez</a>')),
                   img(src = "erin.jpeg", height = 150, width = 200),
                   br(),
                   br(),
                   br(),
                 )
               )),
             ),
    
#   Widget 2: Map of Sites 
    tabPanel("Map of Fishery Relevant Sites",
             sidebarLayout(
                 sidebarPanel(radioButtons(inputId = "site_name",
                                           label = "Select a site to highlight:",
                                           choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                           choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor")
                                           )
                              ),
                 mainPanel(leafletOutput(outputId = "site_map"),
                           textOutput("site_description"))
             ) # end sidebarLayout
             ), #end tabPanel Map of Sites
    
#   Widget 3: Temperature Time Series
    tabPanel("Comparison of Site Temperature Profiles",
             sidebarLayout(
                 sidebarPanel(dateRangeInput(inputId = "date_select",
                                             label = h3("Date range"),
                                             start = "2002-08-01",
                                             end = "2021-07-26"),
                            checkboxGroupInput(inputId = "site_code",
                                               label = h3("Choose site(s):"),
                                               choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                               choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor"),
                                               selected = c("ABUR", "AHND")
                            ),
                 ),
                 mainPanel(plotOutput(outputId = "temp_plot"))
             ) # end sidebarLayout Map Fish Sites
    ), #end tabPanel Temp Time Series

#   Widget 4: HeatMap Visualizations
    tabPanel("Heatmap Visualizations",
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = "site_heatmap_choose",
                                         label = "Select a site:",
                                         choiceValues = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB", "IVEE", "MOHK", "NAPL", "SCDI", "SCTW"),
                                         choiceNames = c("Arroyo Burro", "Arroyo Hondo", "Arroyo Quemado", "Bulito", "Carpinteria", "Goleta Bay", "Isla Vista", "Mohawk", "Naples", "Santa Cruz Island, Diablo", "Santa Cruz Island, Twin Harbor")
                                         )
               ),
               mainPanel(plotOutput(outputId = "site_heatmap"))
        ) # end sidebarLayout Heatmap
    ) #end tabPanel Heatmap

) # end navbarPage
) # end ui

### Define server logic 
server <- function(input, output) {

#   Widget 1: Reactive Inputs - Action buttons for "About the fisheries"
    values <- reactiveValues(species_1 = 0, species_2 = 0, species_3 = 0)
    
    observeEvent(input$species1, {
      values$species_1 <- 1
      values$species_2 <- 0
      values$species_3 <- 0
      
    })
    
    observeEvent(input$species2, {
      values$species_1 <- 0
      values$species_2 <- 1
      values$species_3 <- 0
      
    })
    
    observeEvent(input$species3, {
      values$species_1 <- 0
      values$species_2 <- 0
      values$species_3 <- 1
      
    })
#   Widget 1a: "About the App" - Display Output
    output$display <- renderImage({
      if(values$species_1)
        return(list(
          src = "www/image1.jpeg", width = "80%", height = "90%",
          contentType = 'image/png'
        ))
      else
        if(values$species_2)
          return(list(
            src = "www/image2.jpeg", width = "80%", height = "90%",
            contentType = 'image/png'
          ))
      else
        if(values$species_3)
          return(list(
            src = "www/image3.jpeg", width = "80%", height = "90%",
            contentType = 'image/png'
          ))
      else{
        return(list(
          src = "www/uni.jpeg", width = "80%", height = "90%",
          contentType = 'image/png'))
      }
    }, deleteFile = FALSE)
    
#   Widget 1b: "The Fisheries" - Output Information
    output$information <- renderText(
      {
        if(values$species_1)
          paste("Visit California Sea Grant's page to learn more about the California Spiny Lobster!")
        else
          if(values$species_2)
            paste("Visit California Sea Grant's page to learn more about the Red Sea Urchin!")
        else
          if(values$species_3)
            paste("Visit California Sea Grant's page to learn more about the Mediterranean Mussel")
        else
          return(
            paste("The Santa Barbara Channel is one of the nation's richest sources of bountiful, sustainable and high-quality seafood. A local delicacy in Santa Barbara, CA is 'uni' or Red Sea Urchin. Check out local seasons and species at the link below. Photo Credit: Instagram @choisauceboss")
          )
      })
#   Widget 1c: "The Fisheries" - Output Links   
    # Define links
    url1 <- a("California Sea Grant: Seafood profile: California Spiny Lobster", href="https://caseagrant.ucsd.edu/seafood-profiles/california-spiny-lobster")
    url2 <- a("California Sea Grant: Seafood profile: Red Sea Urchin", href="https://caseagrant.ucsd.edu/seafood-profiles/red-sea-urchin")
    url3 <- a("California Sea Grant: Seafood profile: Red Sea Urchin", href="https://caseagrant.ucsd.edu/seafood-profiles/red-sea-urchin")
    url4 <- a("Commercial Fishermen of Santa Barbara", href="https://www.cfsb.info/species-seasons")
  
    output$link <- renderUI({
      if(values$species_1)
        tagList("URL link:", url1)
      else
        if(values$species_2)
          tagList("URL link:", url2)
      else
        if(values$species_3)
          tagList("URL link:", url3)
      else
        return(
          tagList("URL link:", url4)
        )
    })
    
    url <- a("California Sea Grant: California Seafood Profiles: California Spiny Lobster, href=https://caseagrant.ucsd.edu/seafood-profiles/california-spiny-lobster")
    
#   Widget 1d: "The Data" - Reactive Input
    reef_select <- reactive({
      read_csv("sbc_lter_temp_subset.csv") %>%
        filter(SITE %in% input$reef_code)
    }) 
    
#   Widget 1d: "The Data" - Reactive Output Boxplots
    output$boxplot <- renderPlot({
      ggplot(data = reef_select(), aes(x= SITE, y= avg_temp))+
        geom_boxplot(aes(color = SITE),
                     fill = NA,
                     width = 0.2)+
        labs(x = "Site Location",
             y = "Average Daily Temp (°C)",
             color = "Site",
             subtitle = "Data: SBC LTER Reef: Bottom Temperature (2002 - 2021)")+
        ggtitle("Boxplots of Temperatures at Selected Site(s)")+
        theme(plot.title = element_text(color = "#5b4f41", hjust = 0.5, size = 18),
              plot.subtitle = element_text(color = "#5b4f41", hjust = 0.5, size = 12),
              plot.background = element_rect("white"),
              panel.background = element_rect("#faf7f2"),
              panel.grid = element_line(linetype= "longdash", color = "#f0ece1"),
              axis.text = element_text(color = "#5b4f41", size = 12),
              axis.title = element_text(color = "#5b4f41", size = 15),
              strip.background = element_rect("white"),
              axis.line = element_line(color = "#5b4f41"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"))
    })
#   Widget 1e: "The Data" - Reactive Output Histograms
    output$histo_plot <- renderPlot({
      ggplot(data = reef_select(), aes(x= avg_temp))+
        geom_histogram(aes(color = SITE, fill = SITE), alpha = 0.7)+
        facet_wrap(~SITE)+
        labs(y = "Occurences",
             x = "Average Daily Temp (°C)",
             fill = "Site",
             color = "Site",
             subtitle = "Data: SBC LTER Reef: Bottom Temperature (2002 - 2021)")+
        ggtitle("Distribution of Temperatures at Selected Site(s)")+
        theme(plot.title = element_text(color = "#5b4f41", hjust = 0.5, size = 18),
              plot.subtitle = element_text(color = "#5b4f41", hjust = 0.5, size = 12),
              plot.background = element_rect("white"),
              panel.background = element_rect("#faf7f2"),
              panel.grid = element_line(linetype= "longdash", color = "#f0ece1"),
              axis.text = element_text(color = "#5b4f41", size = 12),
              axis.title = element_text(color = "#5b4f41", size = 15),
              strip.background = element_rect("white"),
              axis.line = element_line(color = "#5b4f41"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black")) +
        scale_alpha(guide = "none") # suppress duplicate legend from having fill and color
    })
    
#   Widget 2: "Map of Sites" - Reactive Input
    site_choose <- reactive({
      read_csv("site_locations.csv") %>%
        filter(site %in% input$site_name)
    })
    
    # Widget 2a: "Map of Sites" - Markers Output
    # All site markers appear on map in blue and the user input site appears in red
    output$site_map <- renderLeaflet({
      leaflet() %>%
        setView(lat = sbLat, lng = sbLong, zoom = sbZoom) %>%
        addTiles() %>%
        addMarkers(data = site_markers, ~long_all, ~lat_all, popup = ~site_all, label = ~site_all, icon = blue_icon) %>%
        addMarkers(data = site_choose(), ~long, ~lat, popup = ~site, label = ~site, icon = red_icon) %>%
        addProviderTiles(providers$Esri.WorldStreetMap)
    })
    
#   Widget 2b: "Map of Sites" - Site Descriptions Output
    output$site_description <- renderText({
      if(input$site_name == "ABUR") 
        paste("Site ABUR: Arroyo Burro Reef is located on the Santa Barbara Channel near the mouth of Arroyo Burro Creek and Beach. Depth ranges from 5.4 to 7 meters.")
      else
        if(input$site_name == "AHND")
          paste("Site AHND: Arroyo Hondo Reef is located on the Santa Barbara Channel near the east end of Gaviota State Park, CA. Depth ranges from -4.3m to -6.6 meters.")
      else
        if(input$site_name == "AQUE")
          paste("Site AQUE: Arroyo Quemado Reef depth range from 5.4 m to 10.7 m. Reference on Land is close to US101/Arroyo Quemada Ln.")
      else
        if(input$site_name == "BULL")
          paste("Site BULL: Bulito has three permanent transects: Transect I, III, VI. Depth Range from -5.5 to -7.3. Reference on land is close to Ranch Real road and Hollister Ranch road crossing section.")
      else
        if(input$site_name == "CARP")
          paste("Site CARP: Carpinteria Reef is located on the Santa Barbara Channel offshore of the Carpinteria Salt Marsh. Depth range is from -2.2 to -8.8 meters")
      else
        if(input$site_name == "GOLB")
          paste("Site GOLB: Goleta Bay is located on the Santa Barbara Channel east of Goleta Pier. Depth range is -4.2 to -5 meters.")
      else
        if(input$site_name == "MOHK")
          paste("Site MOHK: Mohawk Reef depth ranges from 4.5m to 6.0 m. Reference on land is Mohawk Rd / Edgewater Way.")
      else
        if(input$site_name == "IVEE")
          paste("Site IVEE: Isla Vista (IV) Reef is located on the Santa Barbara Channel near the University of California Santa Barbara, CA. Depth range is from -8.2 to -8.8 meters.")
      else
        if(input$site_name == "NAPL")
          paste("Site NAPL: Naples Reef is located on the Santa Barbara Channel near the community of Naples and Dos Pueblos Canyon, Santa Barbara County, CA. Depth ranges from -5.9 to -13.4 meters.")
      else
        if(input$site_name == "SCDI")
          paste("Site SCDI: Santa Cruz Island, Diablo")
      else
        if(input$site_name == "SCTW")
          paste("Site SCTW: Santa Cruz Island, Twin Harbor West")
    })
    
#   Widget 3: "Comparison of Site Temp Profiles" - Reactive Input 
    site_select <- reactive({
      read_csv("sbc_lter_temp_subset.csv") %>%
        filter(SITE %in% input$site_code)
    }) # end site_select reactive
    
#   Widget 3: "Comparison of Site Temp Profiles" - Output
    output$temp_plot <- renderPlot({
      ggplot(data = site_select(), aes(x = DATE_LOCAL, y = avg_temp))+
        geom_line(aes(color = SITE))+
        gghighlight(unhighlighted_params = list(alpha = 0.5),
                    use_direct_label = FALSE)+
        facet_wrap(~SITE)+
        scale_x_date(limits = c(input$date_select))+
        labs(x = "Date",
             y = "Average Daily Temperature (°C)",
             color = "Site",
             subtitle = "Data: SBC LTER Reef: Bottom Temperature (2002 - 2021)")+
        ggtitle("Average Daily Temperature for Selected Site(s) and Dates")+
        theme(plot.title = element_text(color = "#5b4f41", hjust = 0.5, size = 18),
              plot.subtitle = element_text(color = "#5b4f41", hjust = 0.5, size = 12),
              plot.background = element_rect("white"),
              panel.background = element_rect("#faf7f2"),
              panel.grid = element_line(linetype= "longdash", color = "#f0ece1"),
              axis.text = element_text(color = "#5b4f41", size = 12),
              axis.title = element_text(color = "#5b4f41", size = 15),
              strip.background = element_rect("white"),
              axis.line = element_line(color = "#5b4f41"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black")) 
    })
    
#   Widget 4: "Heatmap" - Reactive Input
    site_heatmap_select <- reactive({
      read_csv("sbc_heatmap.csv") %>%
        filter(SITE %in% input$site_heatmap_choose)
    })
    
#   Widget 4: "Heatmap" - Output 
    output$site_heatmap <- renderPlot({
      ggplot(data = site_heatmap_select(), aes(x=year, y=month)) +
        geom_tile(aes(fill = avg_temp)) +
        scale_fill_viridis_c(option = "magma") + 
        labs(x = "Year",
             y = "Month",
             fill = "Average Daily \nTemp (°C)",
             subtitle = "Data: SBC LTER Reef: Bottom Temperature (2002 - 2021)") +
        ggtitle("Heatmap Visualization By Month Per Year") +
        theme(plot.title = element_text(color = "#5b4f41", hjust = 0.5, size = 18),
              plot.subtitle = element_text(color = "#5b4f41", hjust = 0.5, size = 12),
              plot.background = element_rect("white"),
              panel.background = element_rect("#faf7f2"),
              panel.grid = element_line(linetype= "longdash", color = "#f0ece1"),
              axis.text = element_text(color = "#5b4f41", size = 12),
              axis.title = element_text(color = "#5b4f41", size = 15),
              strip.background = element_rect("white"),
              axis.line = element_line(color = "#5b4f41"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black")) 
    })
    

} #end server

### Run the application 
shinyApp(ui = ui, server = server)
