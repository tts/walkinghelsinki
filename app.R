library(shiny)
library(tidyverse)
library(leaflet)

#----------
# Districts 
#----------

distr_in_area <- readRDS("distr_in_area.RDS")

distr_in_area <- distr_in_area %>% 
  sf::st_transform(crs = 4326)

districts <- as.vector(sort(distr_in_area$Name.x))

Espoo_Vantaa <- c("Otaniemi", "Westend", "Ruukinranta", "Mäkkylä",
                  "Lintulaakso", "Uusmäki", "Ylästö", "Tikkurila",
                  "Pakkala", "Koivuhaka", "Viertola", "Kuninkaala",
                  "Vaarala", "Länsimäki")

districts <- districts[!districts %in% Espoo_Vantaa]

hki <- distr_in_area %>% 
  filter(!Name.x %in% c("Ulkosaaret", "Miessaari", "Aluemeri",
                        "Länsisaaret", "Itäsaaret"))

#------------------------
# Park roads and trees
#------------------------

roads_in_distr_in_area <- readRDS("roads_in_distr_in_area.RDS")
roads_in_distr_in_area <- roads_in_distr_in_area %>% 
  sf::st_transform(crs = 4326) 

trees_in_distr_in_area <- readRDS("trees_in_distr_in_area.RDS")
trees_in_distr_in_area <- trees_in_distr_in_area %>% 
  sf::st_transform(crs = 4326)

#---------------------
# Protected buildings
#---------------------

prot_build_in_distr_in_area <- readRDS("prot_build_in_distr_in_area.RDS")
prot_build_in_distr_in_area <- prot_build_in_distr_in_area %>% 
  sf::st_transform(crs = 4326)

#-------------------
# City Bike stations
#-------------------

bikestations_in_distr_in_area <- readRDS("bikestations_in_distr_in_area.RDS")
bikestations_in_distr_in_area <- bikestations_in_distr_in_area %>% 
  sf::st_transform(crs = 4326)

#-------
# App
#-------

ui <- fluidPage(
  
  tags$h2(
    HTML("<span style='color:sienna'>Park roads,</span> <span style='color:green'>planted trees,</span> <span style='color:orange'>protected buildings,</span> 
         and <span style='color:yellow'>biking stations</span> in Helsinki")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #505050;
        color: white;
      },
      .shiny-input-container {
        color: snow;
      }"))
  ),
  
  #titlePanel("Park roads, trees, protected buildings, and biking stations in Helsinki"),
  
  sidebarPanel(
    selectInput(inputId = "target",
                label = "District",
                choices = districts,
                selected = "Kluuvi"),
    plotOutput("hist"),
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