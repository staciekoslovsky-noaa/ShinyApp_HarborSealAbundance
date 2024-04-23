# Harbor Seal Abundance: Shiny App
# Original code written by Allison James
# Code updated and maintained by Stacie Koslovsky

## Get/create functions -----------------------------------------
source("C:\\Users\\Stacie.Hardy\\Work\\SMK\\GitHub\\ShinyApp_HarborSealAbundance\\HarborSealAbundance_Functions.R")

## Install library packages -----------------------------------------
install_pkg("tidyverse")
install_pkg("shiny")
install_pkg("shinythemes")
install_pkg("shinyWidgets")
install_pkg("leaflet")
install_pkg("leaflet.extras")
install_pkg("leaflet.extras2")
install_pkg("htmltools")
install_pkg("viridis")
install_pkg("highcharter")
install_pkg("scales")
install_pkg("geojsonio")
install_pkg("sf")
install_pkg("roll")

sf::sf_use_s2(FALSE)

## Get data from GitHub -----------------------------------------
# Stock polygons
url.stocks <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_HarborSealAbundance/main/Data/survey_stocks.geojson"
stock_polygons <- geojsonio::geojson_read(url.stocks, what = "sp") %>%
  sf::st_as_sf(crs = 4326)

# Haulout locations
url.haulout <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_HarborSealAbundance/main/Data/survey_haulout.geojson"
haulout <- geojsonio::geojson_read(url.haulout, what = "sp") %>%
  sf::st_as_sf(crs = 4326) 

# Poly metadata and last surveyed
url.poly_metadata <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_HarborSealAbundance/main/Data/poly_metadata.rda?raw-true"
load(url(url.poly_metadata))

url.last_surveyed <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_HarborSealAbundance/main/Data/last_surveyed.rda?raw-true"
load(url(url.last_surveyed))

# Abundance data cube
url.data_cube <- "C://smk/4app/data_cube.rda"
data_cube <- load_rdata(url.data_cube) 

# Survey polygons with most recent abundance estimates
url.survey_polygons <- "C://smk/4app/survey_polygons.geojson"
survey_polygons <- geojsonio::geojson_read(url.survey_polygons, what = "sp") %>%
  sf::st_as_sf(crs = 4326)

# Trend data
url.trend_linear_all <- "C://smk/4app/trend_linear_all.rda"
trend_linear_all <- load_rdata(url.trend_linear_all)

url.trend_linear_stock <- "C://smk/4app/trend_linear_stock.rda"
trend_linear_stock <- load_rdata(url.trend_linear_stock)

url.trend_linear_polyid <- "C://smk/4app/trend_linear_polyid.rda"
trend_linear_polyid <- load_rdata(url.trend_linear_polyid)

url.trend_prop_all <- "C://smk/4app/trend_prop_all.rda"
trend_prop_all <- load_rdata(url.trend_prop_all)

url.trend_prop_stock <- "C://smk/4app/trend_prop_stock.rda"
trend_prop_stock <- load_rdata(url.trend_prop_stock)

url.trend_prop_polyid <- "C://smk/4app/trend_prop_polyid.rda"
trend_prop_polyid <- load_rdata(url.trend_prop_polyid)



rm(url.poly_metadata, url.last_surveyed, url.data_cube, url.survey_polygons, url.stocks, url.haulout,
   url.trend_linear_all, url.trend_linear_stock, url.trend_linear_polyid,
   url.trend_prop_all, url.trend_prop_stock, url.trend_prop_polyid)

## Prepare survey_polygons and stock_polygons for map -----------------------------------------
# Get most_recent_year for data
most_recent_year <- max(data_cube$year)

## Move the polygons across the dateline so the Aleutians are not separated and calculate the midpoint of polygons to set the view of the map
# Move survey polygons and across dateline
survey_polygons$geometry <- (sf::st_geometry(survey_polygons) + c(360,90)) %% c(360) - c(0,90)
survey_polygons$centroid.x <- st_coordinates(sf::st_centroid(survey_polygons))[,1]
survey_polygons$centroid.y <- st_coordinates(sf::st_centroid(survey_polygons))[,2]

# Move stock polygons across dateline
stock_polygons$geometry <- (sf::st_geometry(stock_polygons) + c(360,90)) %% c(360) - c(0,90)

# Move haulout points across dateline
haulout$geometry <- (sf::st_geometry(haulout) + c(360,90)) %% c(360) - c(0,90)

# Calculate the center point of the centroids (x and y)
mean_x = (max(survey_polygons$centroid.x) + min(survey_polygons$centroid.x)) / 2
mean_y = (max(survey_polygons$centroid.y) + min(survey_polygons$centroid.y)) / 2


# Create default abundance and trend datasets for app --------------------------------------
abundance <- calculate_abundance(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'all', poly_metadata = poly_metadata) 

trend <- 
  (trend_linear_all %>% mutate(identifier = "all") %>% mutate(trend_type = "linear_all")) %>%
  rbind(trend_linear_stock %>% rename(identifier = stockname) %>% mutate(trend_type = "linear_stock")) %>%
  rbind(trend_linear_polyid %>% rename(identifier = polyid) %>% mutate(trend_type = "linear_polyid")) %>%
  rbind(trend_prop_all %>% mutate(identifier = "all") %>% mutate(trend_type = "prop_all")) %>%
  rbind(trend_prop_stock %>% rename(identifier = stockname) %>% mutate(trend_type = "prop_stock")) %>%
  rbind(trend_prop_polyid %>% rename(identifier = polyid) %>% mutate(trend_type = "prop_polyid"))

## Prepare information for ShinyApp -----------------------------------------

# Initialize the map 
map <- survey_polygons %>% 
  leaflet(
    options = leafletOptions(worldCopyJump = FALSE)
  ) %>% 
  addTiles()

# Initialize informational windows
introduction <- paste("This application allows users to explore over 20 years of harbor seal population abundance and trend information within Alaska. Harbor seals are 
  widely distributed throughout much of Alaska's near-coastal marine waters and are an important indicator of healthy ecosystems. The Alaska Fisheries Science Center 
  (AFSC) has conducted aerial surveys for harbor seals in Alaska nearly every year since 1998. These aerial survey counts along with statistical modeling that accounts 
  for population dynamics and the proportion of seals in the water during surveys allows for estimates of abundance and trend across different spatial and temporal scales. 
  
  <br/><br/>
  More information about our harbor seal research can be found ", a("here", href = "https://www.fisheries.noaa.gov/alaska/marine-mammal-protection/harbor-seal-research-alaska"), ".", sep = "")

instructions <- "This map displays polygons that represent survey units of harbor seals in Alaska, symbolized based on the 2018 abundance estimates; polygons with larger seal 
  populations are both darker in color and less transparent. Hover over the polygon for more specific information about that particular site. The larger gray polygons represent 
  each harbor seal stock. Hover over the polygon to get the name of the stock.
  
  <br/><br/>
  Two figures represent summary information for the survey units shown in the map. The figures represent summary information for all the survey units, until a filter is applied. 
  <ui><li>The <u><b>Abundance</b></u> figure displays the total harbor seal abundance, the 95th percentile confidence interval, and the associated survey effort for all or the filtered survey units. 
  </li><li>The <u><b>Trend</b></u> plot displays a predicted trend, the 95th percentile confidence interval, and the associated survey effort for all or the filtered survey units. The user can specify 
  the number of years of abundance data and thee type of abundance data (estimates or log of estimates) on which the trend should be calculated. 
  
  <br/><br/>
  Survey units (polygons) can be selected dynamically within the map, and the associated figures are updated dynamically when you click the Update Plot
  button after making the filter selection. Filter options are as follows:<br/>
  <ui><li><u><b>By Stock</b></u> - use the drop-down menu to filter the data by harbor seal stock.
  </li><li><u><b>By Survey Unit</b></u> - click on a single survey unit within the map.
  </li><li><u><b>By Polygon</b></u> - use the pentagon button in the map to start drawing a custom polygon. Use the trash can button in the map to delete your custom polygon. 
  Only one polygon can be drawn at a time. The centroid of the polygon must be encompassed within the drawn shape in order for it to be included in the filter.
  </li><li><u><b>By Circle</b></u> - use the circle button in the map to start drawing a circle at the starting point of interest. As the circle size changes, the radius of the circle 
  is displayed."

disclaimer <- "This is a prototype application. While the best efforts have been made to insure the highest quality, tools such as this are under constant development and are 
  subject to change. This application is developed and maintained by scientists at the NOAA Fisheries Alaska Fisheries Science Center and should not be construed as official 
  communication of NMFS, NOAA, or the U.S. Dept. of Commerce. Links and mentions of RStudio and Shiny should not be considered an endorsement by NOAA Fisheries or the U.S. 
  Federal Government."

contact_info <- "This application was developed by Allison James as part of a summer 2022 internship, jointly sponsored by UW CICOES and NOAA Fisheries. 
  
  <br/> <br/> 
  The application is maintained by Stacie Koslovsky (stacie.koslovsky@noaa.gov). 
  
  <br/> <br/> 
  For questions regarding the harbor seal aerial survey project, contact Josh London (josh.london@noaa.gov), and 
  for questions regarding the statistical methods used to calculate the harbor seal abundance estimates, contact Jay Ver Hoef (jay.verhoef@noaa.gov)."

data_access <- paste("The data we are using to power this application are publicly available for viewing and download. Links to each of these datasets are below: <br/>
  
  <ui><li>", a("Alaska Harbor Seal Aerial Survey Units", href = "https://www.arcgis.com/home/item.html?id=c63ccb17b9b144c4a529ee6a3d039665"), 
  "</li><li>", a("Alaska Harbor Seal Abundance 2018", href = "https://www.arcgis.com/home/item.html?id=e69222ad91564422aba9ee0d2e70bfe2"),
  "</li><li>", a("Alaska Harbor Seal Haul-Out Locations", href = "https://www.arcgis.com/home/item.html?id=2c6ca3e595024d3990127bfe061d7ed3"),
  sep = "")



## Define UI -----------------------------------------
ui <- fluidPage(theme = shinytheme("superhero"),
          
          # Hide warnings and errors within the app
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }",),
          
          titlePanel(fluidRow(
            column(6, "Alaska Harbor Seal Abundance"),
            column(6, img(src = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png", height = 90.8, class = "pull-right"))
          )),
          
          # First row: informational box and map
          fluidRow(
            column(3, tabsetPanel(
              tabPanel("Introduction", div(style = 'overflow-y:scroll;height:300px', HTML(introduction))),
              tabPanel("Disclaimers", div(style = 'overflow-y:scroll;height:300px', HTML(disclaimer))),
              tabPanel("Contact", div(style = 'overflow-y:scroll;height:300px', HTML(contact_info))),
              tabPanel("Data Access", div(style = 'overflow-y:scroll;height:300px', HTML(data_access))),
              type = "pills" #gives the highlighted tab a background color
            )
            ),
            column(9, leafletOutput(outputId = "map1"))
          ),
          br(),
          
          # Second row: inputs and plots
          fluidRow(
            
            # Column with the inputs
            column(3, tabsetPanel(
              tabPanel("Data Controls", verticalLayout(
                # selectInput(inputId = "num.years",
                #             label = div(style = "font-size:16px", "Number of Years for Trend"),
                #             choices = c(3, 5, 8),
                #             selected= 8),
                (radioGroupButtons(inputId = "trend.type",
                                   label = div(style = "font-size:16px", "Trend Type:"),
                                   choices = c("Linear", "Proportional"),
                                   selected = "Linear",
                                   direction = "vertical",
                                   status = "danger",
                                   individual = TRUE,
                                   size = "sm",
                                   checkIcon = list(yes = (icon("ok",lib = "glyphicon")),
                                                    no = icon("remove", lib = "glyphicon")))),
                br(),
                (radioGroupButtons(inputId = "filter",
                                   label = div(style = "font-size:16px", "Filter Data By:"),
                                   choices = c("Stock", "Survey Unit", "Polygon", "Circle"),
                                   selected = "Stock",
                                   direction = "vertical",
                                   status = "danger",
                                   individual = TRUE,
                                   size = "sm",
                                   checkIcon = list(yes = (icon("ok",lib = "glyphicon")),
                                                    no = icon("remove", lib = "glyphicon")))),
                br(),
                (selectInput(inputId = "stock.select",
                             label = div(style = "font-size:16px", "Select Stock (if applicable)"),
                             choices = c("All", sort(survey_polygons$stockname)),
                             multiple = FALSE,
                             selected = "All")),
                br(),
                (actionBttn(inputId = "update", 
                            label = "Update Plot",
                            style = "jelly",
                            size = "md",
                            color = "danger")),
                br(),
                (actionBttn(inputId = "default",
                            label = "Reset",
                            style = "jelly",
                            size = "sm",
                            color = "warning")))),
              tabPanel(title = "Instructions", div(style = 'overflow-y:scroll;height:400px', HTML(instructions))),
              type = "pills" # Selected tabs use the background fill color (which for some reason is orange)
            )),
            
            # Column with the plots (under different tabs)
            column(9, tabsetPanel(
              tabPanel("Abundance", highchartOutput(outputId = "plot1")),
              tabPanel("Trend", highchartOutput(outputId = "plot2")),
              type = "pills" # Selected tabs use the background fill color (which for some reason is orange)
            )
            )
          )
)

## Define server logic -----------------------------------------
server <- function(input, output, session) {
  
  # Map starts off at center, map.view represents current view
  map.view <- reactiveValues(values = c(mean_x, mean_y))
  
  ## Generate Leaflet map and polygons ##
  output$map1 <- renderLeaflet({
    
    abund_bins <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 12000) 
    
    #Create palette to color polygons
    pal <- colorBin(
      palette = "inferno",
      reverse = TRUE,
      domain = na.omit(survey_polygons$abund_est),
      bins = abund_bins,
      pretty = FALSE,
      na.color = "#00000000"
    )
    
    factpal <- colorFactor(
      palette = viridis_pal(option = "H")(nrow(stock_polygons)),
      domain = stock_polygons$stockname
    )
    
    map %>% 
      leaflet.extras::addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = '#f6d746', weight = 3),
                                            showArea = TRUE, metric = FALSE),
        rectangleOptions = FALSE, 
        #circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, 
                                         selectedPathOptions = FALSE, 
                                         remove = TRUE),
        singleFeature = TRUE)  %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(maxWidth = 250)) %>% 
      addPolygons(data = stock_polygons,
                  fillOpacity = 0.2,
                  opacity = 0.4,
                  fillColor = ~factpal(stockname), 
                  color = "gray",
                  weight = 2,
                  label = stock_polygons$stockname,
                  labelOptions = labelOptions(sticky = FALSE, noHide = FALSE, textOnly = TRUE, direction = "center")) %>% 
      addPolygons(data = survey_polygons,
                  layerId = ~polyid,
                  group = "stockname",
                  popup = ~htmltools::htmlEscape(paste("You have selected survey unit ", survey_polygons$polyid, 
                                                       ", found in the ", survey_polygons$stockname, 
                                                       " stock. In ", most_recent_year, 
                                                       ", the harbor seal abundance estimate for this survey unit was ", 
                                                       round(survey_polygons$abund_est, 2), " with a confidence interval of ", 
                                                       round(survey_polygons$abund_b95, 2), "-", round(survey_polygons$abund_t95, 2), 
                                                       ", and the 8-year trend in harbor seal abundance was ", round(survey_polygons$trend_est, 2), 
                                                       " seals per year. ", survey_polygons$survey_date, sep = "")),
                  weight = 1,
                  fillColor = ~pal(survey_polygons$abund_est),
                  color = ~pal(survey_polygons$abund_est),
                  opacity = 0.7,
                  fillOpacity = get_opacity(as.vector(survey_polygons$abund_est), abund_bins),
                  label = survey_polygons$polyid,
                  labelOptions = labelOptions(sticky = TRUE, noHide = FALSE, textOnly = TRUE, direction = "center")) %>% 
      addCircles(data = haulout,
                 fillColor = 'darkslategray',
                 color = 'darkslategray', 
                 weight = 2,
                 opacity = 0.8,
                 group = "haulout") %>%
      addLegend("bottomleft",
                pal = pal,
                values = abund_bins,
                title = "Abundance:") %>% 
      addControl(HTML(paste(most_recent_year, "Estimated Abundance")), position = "topright") %>% 
      
      groupOptions("haulout", zoomLevels = 8:20) %>%
      
      #Set view to the middle of all the polygons' coordinates
      setView(lng = map.view$values[1], lat = map.view$values[2], zoom = 4) 
    
  })
  
  # Reactive dataset and title for abundance plot
  plotted.abundance <- reactiveValues(values = abundance)
  title.abundance <- reactiveVal(value = "in All Stocks")
  
  # Reactive dataset and title for trend plot
  plotted.trend <- reactiveValues(values = trend %>% filter(trend_type == "linear_all"))
  title.trend <- reactiveVal(value = "in All Stocks")
  title.trend.type <- reactiveVal(value = "Linear")
  yaxis.trend <- reactiveVal(value = "Trend (# of Seals)")
  
  # Reactive value that zooms map to centroid of selected features
  zoom.to.stock <- reactiveVal(value = FALSE)
  
  
  ## Update reactive dataset when button is pressed ##
  observeEvent(input$update, {
    title.trend.type(input$trend.type)
    yaxis.trend(ifelse(input$trend.type == "Linear", "Trend (# of Seals)", "Trend (% of Population)"))
    
    # Process data if "stock" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(input$filter == "Stock"){
      # Update the dataset to be filtered by the selected stock
      if(input$stock.select != "All"){
        stock_filter <- input$stock.select
        
        plotted.abundance$values <- calculate_abundance(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'stock', poly_metadata = poly_metadata, filter = stock_filter) 
        title.abundance(paste("in the", input$stock.select, "Stock"))
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_stock" else trend_type == "prop_stock") %>%
          filter(identifier == stock_filter)
        title.trend(paste("in the", input$stock.select, "Stock"))

        
        currently_plotted_ids <- survey_polygons %>%
          filter(stockname == stock_filter)
      }
      else {
        plotted.abundance$values <- abundance 
        title.abundance("in All Stocks")
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
        title.trend("in All Stocks")
      }
      zoom.to.stock(TRUE)
    }
    
    # Process data if "polygon" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else if(input$filter == "Polygon"){
      
      # If there is no drawn shape, revert to default data (otherwise there is a fatal error and the R session is aborted)
      if(is.null(input$map1_draw_new_feature) || (input$map1_draw_new_feature$properties$feature_type == "circle")){
        plotted.abundance$values <- abundance 
        title.abundance("in All Stocks")
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
        title.trend("in All Stocks")
      }
      else {
        # Select based on drawn polygon
        drawn <- input$map1_draw_new_feature
        polygon_coordinates <- do.call(rbind, lapply(drawn$geometry$coordinates[[1]], function(x){c(x[[1]][1],x[[2]][1])}))
        drawn_polygon <- data.frame(lat = polygon_coordinates[, 2], long = polygon_coordinates[, 1]) %>%
          st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        found_in_bounds <- st_join(sf::st_set_crs(drawn_polygon, 4326), sf::st_set_crs(survey_polygons, 4326))
        
        poly_filter <- found_in_bounds$polyid
        
        # Subset data by polyids within polygon
        plotted.abundance$values <- calculate_abundance(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'poly_in_list', poly_metadata = poly_metadata, filter = poly_filter) %>%
          left_join(trend_linear_stock, by = "year")
        title.abundance("in the Selected Survey Units")
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
        title.trend("in All Stocks")
        
        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% poly_filter) 
      }
      zoom.to.stock(TRUE)
    }
    
    # Process data if "circle" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else if(input$filter == "Circle"){
      
      # If there is no drawn circle, revert to default data (otherwise there is a fatal error and the r session is aborted)
      if(is.null(input$map1_draw_new_feature) || (input$map1_draw_new_feature$properties$feature_type != "circle")){
        plotted.abundance$values <- abundance 
        title.abundance("in All Stocks")
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
        title.trend("in All Stocks")
      }
      else {
        # Select based on drawn circle
        drawn <- input$map1_draw_new_feature
        
        drawn_circle <- data.frame(lat = drawn$geometry$coordinates[[2]], long = drawn$geometry$coordinates[[1]]) %>%
          sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
          sf::st_transform(crs = 32606) %>%
          sf::st_buffer(dist = drawn$properties$radius) %>%
          sf::st_transform(4326) 
        drawn_circle$geometry <- (sf::st_geometry(drawn_circle) + c(360,90)) %% c(360) - c(0,90)
        
        found_in_bounds <- st_join(sf::st_set_crs(drawn_circle, 4326), sf::st_set_crs(survey_polygons, 4326))
        
        circle_filter <- found_in_bounds$polyid
        
        # Calculate custom abundance estimates for app
        plotted.abundance$values <- calculate_abundance(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'poly_in_list', poly_metadata = poly_metadata, filter = circle_filter) 
        title.abundance("Within the Given Radius")
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
        title.trend("in All Stocks")
        
        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% circle_filter) 
      }
      zoom.to.stock(TRUE)
    }
    
    # Process data if "survey unit" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else if (input$filter == "Survey Unit"){
      if(is.null(input$map1_shape_click$id)){
        plotted.abundance$values <- abundance 
        title.abundance("in All Stocks")
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
        title.trend("in All Stocks")
      }
      else {
        singlePoly_filter <- input$map1_shape_click$id
        
        # Calculate custom abundance estimates for app
        plotted.abundance$values <- calculate_abundance(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'poly_in_list', poly_metadata = poly_metadata, filter = singlePoly_filter) 
        title.abundance(paste("in Survey Unit:", singlePoly_filter))
        
        plotted.trend$values <- trend %>%
          filter(if (input$trend.type == "Linear") trend_type == "linear_polyid" else trend_type == "prop_polyid") %>%
          filter(identifier == singlePoly_filter )
        title.trend(paste("in Survey Unit:", singlePoly_filter))
        
        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% singlePoly_filter) 
      }
      zoom.to.stock(TRUE)
    }
    
    # Map survey_polygons where the ids match the ones in plotted.abundance$values
    leafletProxy("map1") %>% 
      clearGroup(group = "lines")
    
    if (exists("currently_plotted_ids") == TRUE){
      leafletProxy("map1") %>% 
        addPolylines(data = currently_plotted_ids,
                     fillOpacity = 0,
                     opacity = 0.4,
                     color = "#59FAFC",
                     weight = 3,
                     #layerId = ~polyid
                     group = "lines")
    }

    if(zoom.to.stock()){
      if (exists("currently_plotted_ids") == TRUE){
        View(currently_plotted_ids)
        
        subset_x_centroid <- mean(currently_plotted_ids$centroid.x)
        subset_y_centroid <- mean(currently_plotted_ids$centroid.y)
        
        leafletProxy("map1") %>% 
          setView(subset_x_centroid, subset_y_centroid, zoom = 6)
      } else
        leafletProxy("map1") %>% 
          setView(lng = map.view$values[1], lat = map.view$values[2], zoom = 4) 
    }
  })
  
  observeEvent(input$default,{
    
    plotted.abundance$values <- abundance 
    title.abundance("in All Stocks")
    
    plotted.trend$values <- trend %>%
      filter(if (input$trend.type == "Linear") trend_type == "linear_all" else trend_type == "prop_all")
    title.trend("in All Stocks")
    title.trend.type <- input$trend.type
    yaxis.trend <- ifelse(input$trend.type == "Linear", "Trend (# of Seals)", "Trend (% of Population)")

    leafletProxy("map1") %>% 
      setView(mean_x, mean_y, zoom = 4) %>% 
      clearGroup(group = "lines")
    
  })
  
  ## Plots -----------------------------------------
  # A caption for the chart is available if we'd be interested in including that
  # Data labels in plotOptions -> line -> dataLabels
  
  # Plot for abundance
  output$plot1 <- renderHighchart({
    abund_subset <- plotted.abundance$values

    highchart() %>%
      hc_xAxis(title = list(text = "Year", style = list(color = "#FFFFFF",  fontSize = "16px")),
               labels = list(style = list(color = "#FFFFFF"))) %>%
      hc_yAxis_multiples(list(title = list(text = "# of Harbor Seals", style = list(color = "#FFFFFF", fontSize = "16px")),
                              labels = list(format = '{value}', style = list(color = "#FFFFFF")),
                              min = min(abund_subset$abund_b95),
                              max = max(abund_subset$abund_t95),
                              showFirstLabel = TRUE,
                              showLastLabel = TRUE,
                              opposite = FALSE),
                         list(title = list(text = "% of Harbor Seals Surveyed", style = list(color = "#FFFFFF", fontSize = "16px")),
                              min = 0,
                              max = 100,
                              labels = list(format = "{value}%", style = list(color = "#FFFFFF")),
                              showLastLabel = TRUE, 
                              opposite = TRUE)) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(data = abund_subset,
                    type = "column",
                    hcaes(x = year, y = effort),
                    color = "#855278",
                    name = "% of Harbor Seals Surveyed",
                    opacity = 0.5,
                    yAxis = 1) %>%
      hc_add_series(data = abund_subset,
                    type = "arearange",
                    hcaes(x = year, low = abund_b95, high = abund_t95),
                    color = "#E6C6B5",
                    opacity = 0.4,
                    name = "95th Percentile Confidence Interval") %>%
      hc_add_series(data = abund_subset,
                    type="line",
                    lineWidth = 5,
                    hcaes(x = year, y = abund_est),
                    color = "#E55C30", #"#ED6925",
                    name = "Estimated Abundance") %>%
      hc_title(text = paste("Estimated Harbor Seal Abundance by Year", title.abundance()),
               style = list(color = "#FFFFFF")) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF"))
  })
  
  #Plot for trend
  output$plot2 <- renderHighchart({
    trend_subset <- plotted.trend$values
    print(yaxis.trend())
    
    highchart() %>%
     hc_xAxis(title = list(text = "Year", style = list(color = "#FFFFFF",  fontSize = "16px")),
               labels = list(style = list(color = "#FFFFFF"))) %>%
      hc_yAxis(title = list(text = yaxis.trend(), style = list(color = "#FFFFFF", fontSize = "16px")),
               labels = list(format = '{value}', style = list(color = "#FFFFFF")),
               min = min(trend_subset$trend_b95),
               max = max(trend_subset$trend_t95),
               showFirstLabel = TRUE,
               showLastLabel = TRUE,
               opposite = FALSE) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(data = trend_subset,
                    type = "arearange",
                    hcaes(x = year, low = trend_b95, high = trend_t95),
                    color = "#E6C6B5",
                    opacity = 0.5,
                    name = "95th Percentile Confidence Interval") %>%
      hc_add_series(data = trend_subset,
                    type="line",
                    lineWidth = 5,
                    hcaes(x = year, y = trend_est),
                    color = "#E55C30",
                    name = "Trend") %>%
      hc_title(text = paste("8-Year ", title.trend.type(), " Trend in Estimated Harbor Seal Abundance By Year ", title.trend(), sep = ""),
               style = list(color = "#FFFFFF")) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF"))
  })
}

## Run the application -----------------------------------------
shinyApp(ui = ui, server = server)
