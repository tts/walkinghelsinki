library(shiny)
library(tidyverse)
library(leaflet)
library(sf)

#----------
# Districts 
#----------

distr_in_area <- readRDS("distr_in_area.RDS")

districts <- as.vector(sort(distr_in_area$Name.x))

# Districts in neighbour cities bordering Helsinki
Espoo_Vantaa <- c("Otaniemi", "Westend", "Ruukinranta", "Mäkkylä",
                  "Lintulaakso", "Uusmäki", "Ylästö", "Tikkurila",
                  "Pakkala", "Koivuhaka", "Viertola", "Kuninkaala",
                  "Vaarala", "Länsimäki")

districts <- districts[!districts %in% Espoo_Vantaa]

# Helsinki land area for the 'Where are we?' map 
hki <- distr_in_area %>% 
  filter(!Name.x %in% c("Ulkosaaret", "Miessaari", "Aluemeri",
                        "Länsisaaret", "Itäsaaret", Espoo_Vantaa))

#------------------------
# Park roads and trees
#------------------------

roads_in_distr_in_area <- readRDS("roads_in_distr_in_area.RDS")

trees_in_distr_in_area <- readRDS("trees_in_distr_in_area.RDS")

#---------------------
# Protected buildings
#---------------------

prot_build_in_distr_in_area <- readRDS("prot_build_in_distr_in_area.RDS")

#-------------------
# City bike stations
#-------------------

bikestations_in_distr_in_area <- readRDS("bikestations_in_distr_in_area.RDS")


#-------
# App
#-------

ui <- fluidPage(
  
  tags$h2(
    HTML("<span style='color:green'>Planted trees,</span> <span style='color:sienna'>park roads,</span> <span style='color:orange'>protected buildings,</span> 
         and <span style='color:yellow'>city bike stations</span> in Helsinki")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #333333;
        color: white;
      },
      .shiny-input-container {
        color: snow;
      }
      label.control-label {
        color: #5f9ea0;
      }"
    ))
  ),
  
  
  sidebarPanel(
    selectInput(inputId = "target",
                label = "District",
                choices = districts,
                selected = "Kluuvi"),
    plotOutput("hist"),
    HTML("<p></p><p><span style='color:black'>Click a station to find out the number of available bikes. Note that
         there are a few new stations opening up in summer 2021 with no info yet.</span></p>"),
    width = 3
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("District", 
               leafletOutput("dist", height = 800)),
      tabPanel("Where are we?", 
               leafletOutput("bigdist", height = 800))
    ),
    width = 9
  )
)

server <- function(input, output, session) {
  
  District <- reactive({
    distr_in_area %>%
      filter(Name.x == input$target)
  })
  
  Roads <- reactive({
    roads_in_distr_in_area %>% 
      filter(Name.x == input$target)
  })
  
  Trees <- reactive({
    trees_in_distr_in_area %>% 
      filter(Name.x == input$target)
  })
  
  Buildings <- reactive({
    prot_build_in_distr_in_area %>% 
      filter(Name.x == input$target)
  })
  
  Stations <- reactive({
    bikestations_in_distr_in_area %>% 
      filter(Name.x == input$target)
  })
  
  
  output$dist <- renderLeaflet({
    
      m <- leaflet(sf::st_zm(District())) %>%
        addTiles() %>%
        addPolygons(color = "steelblue2")

      if(nrow(Roads()) > 0) {
        m <- m %>%
          addPolylines(data = sf::st_zm(Roads()), color = "sienna", label = Roads()$length)
      }
      
      if(nrow(Trees()) > 0) {
        m <- m %>%
          addCircles(data = sf::st_zm(Trees()), color = "springgreen2", label = Trees()$nimi)
      }

      if(nrow(Buildings()) > 0) {
        m <- m %>%
          addPolygons(data = sf::st_zm(Buildings()), color = "darkorange", label = Buildings()$osoite)
      }

      if(nrow(Stations()) > 0) {
        m <- m %>% 
          addCircleMarkers(data = sf::st_zm(Stations()), color = "yellow", radius = 5, opacity = 0.6)
      }

      m
      
  })
  
  output$hist <- renderPlot({
    
    if(nrow(Roads()) > 0) {
      p <- hist(Roads()$length_n,
                breaks = 20,
                main = "Park road lengths",
                xlab = "m",
                ylab = "Count",
                las = 1)
      p
      
    }
    
  })
  
  output$bigdist <- renderLeaflet({

      m2 <- leaflet(sf::st_zm(hki)) %>%
        addTiles() %>%
        addPolygons(color = "steelblue2")

      m2 <- m2 %>%
        addPolygons(data = sf::st_zm(District()), color = "red", label = District()$Name.x)

      m2

  })
  
  
  observeEvent(input$dist_marker_click, {
     
    click <- input$dist_marker_click
    
    if(is.null(click)){
      return()
    } else {
      
      lo <- formatC(click$lat, digits = 5, format = "f")
      la <- formatC(click$lng, digits = 5, format = "f")
      
      bike_station_clicked <- Stations() %>%
        mutate(X = formatC(sf::st_coordinates(.)[,1], digits = 5, format = "f"),
               Y = formatC(sf::st_coordinates(.)[,2], digits = 5, format = "f")) %>%
        filter(X == la & Y == lo) %>%
        select(ID) %>%
        sf::st_drop_geometry(.) %>%
        as.character()
      
      res <- httr::POST(url = "https://api.digitransit.fi/routing/v1/routers/hsl/index/graphql",
                        body = paste0('{bikeRentalStation(id:"', bike_station_clicked, '"){name bikesAvailable}}'),
                        httr::add_headers(.headers = c("accept" = "application/json",
                                                       "Content-Type" = "application/graphql")))
      
      json <- jsonlite::fromJSON(httr::content(res, as = "text"))
      
      text <- paste0("Bikes available at ", json$data$bikeRentalStation$name, ": ",json$data$bikeRentalStation$bikesAvailable)
      
      dist <- leafletProxy("dist")
      
      dist %>% clearPopups() %>%
        addPopups(click$lng, click$lat, text)
      
      }
    
    })
  
  
}

shinyApp(ui, server)
