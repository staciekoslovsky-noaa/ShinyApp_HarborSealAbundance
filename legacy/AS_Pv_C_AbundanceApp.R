# Coastal Pv Surveys: Shiny web app to view harbor seal abundance
# S. Hardy, 26 June 2019
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# function for finding the locations inside the shapes we draw
findLocations <- function(shape, location_coordinates, location_id_colname){
  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  
  if(feature_type == "polygon") {
    # transform into a spatial polygon
    drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))
    
    # use 'over' from the sp package to identify selected locations
    selected_locs <- sp::over(location_coordinates, sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))))
    
    # get location ids
    x <- (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
    selected_loc_id <- as.character(x[[location_id_colname]])
    
    return(selected_loc_id)
   }
}

install_pkg("shiny")
install_pkg("leaflet")
install_pkg("leaflet.extras")
install_pkg("tidyverse")
install_pkg("RPostgres")
install_pkg("shinythemes")
install_pkg("htmltools")
install_pkg("sf")

# Connect to DB -------------------------------------------------
con <- RPostgres::dbConnect(Postgres(), 
                            dbname = Sys.getenv("pep_db"), 
                            host = Sys.getenv("pep_ip"), 
                            port = Sys.getenv("pep_port"),
                            user = Sys.getenv("pep_user"), 
                            password = rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))

# Get data from DB ---------------------------------------------
abund <- sf::st_read(con, query = "SELECT id, polyid, year, abund_est, abund_b95, abund_t95, trend_est, trend_b95, trend_t95, surveyed, stockname, geom FROM surv_pv_cst.geo_abundance_4shiny", geometry_column = "geom")
abund <- sf::st_set_crs(abund, 4326)
abund <- sf::st_shift_longitude(abund)
                               
abund$secondpolyid <- paste(as.character(abund$polyid), "_selectedLayer", sep="")

polys <- sf::st_read(con, query = "SELECT * FROM surv_pv_cst.geo_abundance_4shiny WHERE year = \'2018\'", geometry_column = "geom")
st_shift_longitude(polys)

RPostgres::dbDisconnect(con)

# Code for shiny app -------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Alaska Harbor Seal Abundance"),
  
  sidebarPanel(
    selectInput("stock", "Stock", unique(abund$stockname)),
    br(), br(),
    plotOutput("myfigure_effort")
  ),
  
  mainPanel(
    leafletOutput("mymap"),
    br(), br(),
    plotOutput("myfigure_abund"),
    br(), br(),
    plotOutput("myfigure_trend"))
  # sidebarPanel(
  #   ### User chooses the species to map
  #   selectInput("speciesInput", "Species",
  #               unique(fixInvase$Species))
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())
  
  # base map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #addProviderTiles(providers$Esri.OceanBasemap) %>%
      #setView(205, 57, zoom = 4) %>% 
      # addCircles(data = abund,
      #            radius = 1000,
      #            #lat = abund$latitude,
      #            #lng = abund$longitude,
      #            fillColor = "white",
      #            fillOpacity = 1,
      #            color = "navy",
      #            weight = 2,
      #            stroke = T,
      #            layerId = as.character(abund$polyid),
      #            highlightOptions = highlightOptions(color = "mediumseagreen",
      #                                                opacity = 1.0,
      #                                                weight = 2,
      #                                                bringToFront = TRUE)) %>%
      addPolygons(data = polys,
                  popup = ~htmltools::htmlEscape(paste("The selected survey unit is ", polys$polyid, " and is found in the ", polys$stockname, 
                                                       " stock. In 2018, the harbor seal abundance estimate for this survey unit is ", round(polys$abund_est, 2), " with a confidence",
                                                       " interval of ", round(polys$abund_b95, 2), "-", round(polys$abund_t95, 2), ", and the 8-year abundance trend is ",
                                                       round(polys$trend_est, 2), " seals per year.", sep = "")),
                  group = "stockname") %>%
      addLayersControl(
        overlayGroups = unique(polys$stockname),
        options = layersControlOptions(collapsed = FALSE)) %>%
      leafletOptions(worldCopyJump = TRUE) %>%
      addDrawToolbar(
        targetGroup = 'Selected',
        polylineOptions = FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        rectangleOptions = FALSE, #drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        circleOptions = FALSE, #drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        circleMarkerOptions = FALSE,
        #singleFeature = TRUE,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = TRUE, remove = TRUE))
  })
  
  output$myfigure_abund <- renderPlot(ggplot(abund, aes(x=year, y=abund_est)) + geom_bar(stat="identity"))
  output$myfigure_trend <- renderPlot(ggplot(abund, aes(x=year, y=trend_est)) + stat_summary(fun.y = sum, na.rm = TRUE, group = 3, color = 'black', geom ='line'))
  output$myfigure_effort <- renderPlot(ggplot(abund, aes(x=year, y=surveyed)) + geom_bar(stat= "identity"))
  
  ############################################### section three #################################################
  observeEvent(input$mymap_draw_new_feature,{
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature, location_coordinates = coordinates, location_id_colname = "polyid")
    
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    # subset data by polyids found
    selected <- subset(abund, polyid %in% data_of_click$clickedMarker)
    
    proxy <- leafletProxy("mymap")
    proxy %>% addCircles(data = selected,
                         radius = 1000,
                         lat = selected$latitude,
                         lng = selected$longitude,
                         fillColor = "wheat",
                         fillOpacity = 1,
                         color = "mediumseagreen",
                         weight = 3,
                         stroke = T,
                         layerId = as.character(selected$secondpolyid),
                         highlightOptions = highlightOptions(color = "hotpink",
                                                             opacity = 1.0,
                                                             weight = 2,
                                                             bringToFront = TRUE))
    
    output$myfigure_abund <- renderPlot(ggplot(selected, aes(x=year, y=abund_est)) + geom_bar(stat="identity"))
    output$myfigure_trend <- renderPlot(ggplot(selected, aes(x=year, y=trend_est)) + stat_summary(fun.y = sum, na.rm = TRUE, group = 3, color = 'black', geom ='line'))
    output$myfigure_effort <- renderPlot(ggplot(selected, aes(x=year, y=surveyed)) + geom_bar(stat= "identity"))
  })
  
  ############################################### section four ##################################################
  observeEvent(input$mymap_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    for(feature in input$mymap_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature, location_coordinates = coordinates, location_id_colname = "secondpolyid")
      
      # remove second layer representing selected locations
      proxy <- leafletProxy("mymap")
      proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
      
      first_layer_ids <- subset(abund, secondpolyid %in% bounded_layer_ids)$polyid
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker %in% first_layer_ids]
    }
    
    selected <- subset(abund, polyid %in% data_of_click$clickedMarker)
    if (nrow(selected) > 0) {
      output$myfigure_abund <- renderPlot(ggplot(selected, aes(x=year, y=abund_est)) + geom_bar(stat="identity"))
      output$myfigure_trend <- renderPlot(ggplot(selected, aes(x=year, y=trend_est)) + stat_summary(fun.y = sum, na.rm = TRUE, group = 3, color = 'black', geom ='line'))
      output$myfigure_effort <- renderPlot(ggplot(selected, aes(x=year, y=surveyed)) + geom_bar(stat= "identity"))
    } else {
      output$myfigure_abund <- renderPlot(ggplot(abund, aes(x=year, y=abund_est)) + geom_bar(stat="identity"))
      output$myfigure_trend <- renderPlot(ggplot(abund, aes(x=year, y=trend_est)) + stat_summary(fun.y = sum, na.rm = TRUE, group = 3, color = 'black', geom ='line'))
      output$myfigure_effort <- renderPlot(ggplot(abund, aes(x=year, y=surveyed)) + geom_bar(stat= "identity"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
