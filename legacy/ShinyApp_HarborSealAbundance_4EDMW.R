# Coastal Harbor Seal Abundance: Shiny App
# Original code written by Allison James
# Code updated and maintained by Stacie Hardy

## Create functions -----------------------------------------

# Function to install packages needed
install_pkg <- function(x){
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Function to check if the selected plot has ever been surveyed
has_been_surveyed <- function(x){
  new_vector <- c()
  
  for (element in x){
    if(!is.na(element) && !is.null(element) && element != "NA"){
      element <- paste("This survey unit was last surveyed on ", element, ".", sep = "")
      new_vector <- new_vector %>% append(element)
    }
    else{
      #print("we got a null!")
      element <- "This survey unit has not been surveyed."
      new_vector <- new_vector %>% append(element)
      #print(element)
    }
  }
  return(new_vector)
}

# Function to check if a point is inside of a given polygon
is_inside_polygon <- function(x, y, point.x, point.y){
  inside = FALSE
  if (sp::point.in.polygon(point.x, point.y, x, y)){
    inside = TRUE
  }
  return(inside)
}

# Function to assign opacity values to polygons based on abundance
get_opacity <- function(x, bins){
  opacity_vector <- c()
  
  for (element in x){
    if(element < bins[1]){
      opacity_vector <- opacity_vector %>% append(0.01)
    }
    else if(element < bins[2]){
      opacity_vector <- opacity_vector %>% append(0.3)
    }
    else if(element < bins[3]){
      opacity_vector <- opacity_vector %>% append(0.4)
    }
    else if(element < bins[4]){
      opacity_vector <- opacity_vector %>% append(0.5)
    }
    else if(element < bins[5]){
      opacity_vector <- opacity_vector %>% append(0.6)
    }
    else if(element < bins[6]){
      opacity_vector <- opacity_vector %>% append(0.7)
    }
    else if(element < bins[7]){
      opacity_vector <- opacity_vector %>% append(0.8)
    }
    else {
      opacity_vector <- opacity_vector %>% append(0.9)
    }
  }
  return(opacity_vector)
}



## Install library packages -----------------------------------------
install_pkg("shiny")
install_pkg("shinythemes")
install_pkg("shinyWidgets")
install_pkg("leaflet")
install_pkg("tidyverse")
install_pkg("sp")
install_pkg("geojsonio")
install_pkg("htmltools")
install_pkg("leaflet.extras")
install_pkg("leaflet.extras2")
install_pkg("viridis")
install_pkg("highcharter")
install_pkg("scales")
install_pkg("rgeos")
install_pkg("spatialrisk")
install_pkg("sf")

# Alternate way to download spatialrisk
### install_pkg("devtools")
### install_github("MHaringa/spatialrisk")



## Get and process data from GitHub -----------------------------------------

# Abundance data
url.abund <- "https://raw.githubusercontent.com/StacieKozHardy/CoastalPv_AbundanceApp/main/Data/geo_abundance_4shiny.csv"
abundance <- read.csv(url(url.abund)) %>%
  filter(!is.na(year)) %>%
  mutate(year  = as.numeric(year),
         abund_est = as.numeric(abund_est),
         abund_b95 = as.numeric(abund_b95),
         abund_t95 = as.numeric(abund_t95),
         trend_est = as.numeric(trend_est),
         trend_b95 = as.numeric(trend_b95),
         trend_t95 = as.numeric(trend_t95)) %>%
  rename(abundance_id = id)
  
# Filters the data so that it's the most recent year while removing NA values
most_recent_year <- max(abundance$year, na.rm = TRUE)

# Create dataset of abundance from most-recent year and includes field for abundance by stock
abundance_filtered <- abundance %>%
  filter(year == most_recent_year) %>%
  group_by(stockname) %>% 
  mutate("total.abund" = sum(abund_est)) %>%
  ungroup()

# Geojson file with polygons data
url.poly <- "https://raw.githubusercontent.com/StacieKozHardy/CoastalPv_AbundanceApp/main/Data/survey_polygons.geojson"
survey_polygons <- geojson_read(url.poly, what = "sp") %>%
  sf::st_as_sf() %>%
  rename(polygon_id = id)

# Geojson file with the stocks
url.stocks <- "https://raw.githubusercontent.com/StacieKozHardy/CoastalPv_AbundanceApp/main/Data/survey_stocks.geojson"
stock_polygons <- geojson_read(url.stocks, what = "sp") %>%
  sf::st_as_sf()



## Prepare survey_polygons and stock_polygons for map -----------------------------------------

# Join the polygons data with the most recent abundance estimates
survey_polygons <- survey_polygons %>% 
  select(-stockname) %>%
  left_join(abundance_filtered, by = "polyid") %>% 
  mutate("survey_date" = has_been_surveyed(as.vector(survey_polygons$last_surveyed)))

# Subset the data so that plots without abundance data are not included
survey_polygons <- survey_polygons[!is.na(survey_polygons$abund_est), ]



## Move the polygons across the dateline so the Aleutians are not separated -----------------------------------------
# And calculate the midpoint of polygons to set the view of the map

# Move survey polygons across dateline
survey_polygons$geometry <- (sf::st_geometry(survey_polygons) + c(360,90)) %% c(360) - c(0,90)
survey_polygons$centroid.x <- st_coordinates(sf::st_centroid(survey_polygons))[,1]
survey_polygons$centroid.y <- st_coordinates(sf::st_centroid(survey_polygons))[,2]

# Move stock polygons across dateline
stock_polygons$geometry <- (sf::st_geometry(stock_polygons) + c(360,90)) %% c(360) - c(0,90)

# Convert back to sp layers
survey_polygons <- as(survey_polygons, Class = "Spatial")
stock_polygons  <- as(stock_polygons, Class = "Spatial")

# # Create new columns with coordinates of each polygon's centroid
# survey_polygons$centroid.x <- survey_polygons@polygons[[j]]@labpt[1]
# survey_polygons$centroid.y <- survey_polygons@polygons[[j]]@labpt[2])

# Calculate the center point of the centroids (x and y)
mean_x = (max(survey_polygons$centroid.x) + min(survey_polygons$centroid.x)) / 2
mean_y = (max(survey_polygons$centroid.y) + min(survey_polygons$centroid.y)) / 2




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
  The application is maintained by Stacie Hardy (stacie.hardy@noaa.gov). 
  
  <br/> <br/> 
  For questions regarding the harbor seal aerial survey project, contact Josh London (josh.london@noaa.gov), and 
  for questions regarding the statistical methods used to calculate the harobr seal abundance estimates, contact Jay Ver Hoef (jay.verhoef@noaa.gov)."


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
                selectInput(inputId = "trend.input.data",
                            label = div(style = "font-size:16px", "Input Data for Trend"),
                            choices = c("Abundance", "Log(Abundance)"),
                            selected = "Abundance"),
                selectInput(inputId = "num.years",
                            label = div(style = "font-size:16px", "Number of Years for Trend"),
                            choices = c(5, 8, 10, 15), #can be changed later
                            selected= 8),
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
                             choices = c("All", survey_polygons@data$stockname),
                             multiple = TRUE,
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
  
  #map starts off at center, map.view represents current view
  map.view <- reactiveValues(values = c(mean_x, mean_y))
  
  
  ####Generate Leaflet map and polygons####
  output$map1 <- renderLeaflet({
    
    abund_bins <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 12000) 
    
    #Create palette to color polygons
    pal <- colorBin(
      palette = "inferno",
      reverse = TRUE,
      domain = na.omit(survey_polygons@data$abund_est),
      bins = abund_bins,
      pretty = FALSE,
      na.color = "#00000000"
    )
    
    factpal <- colorFactor(
      palette = "mako", 
      domain = stock_polygons@data$stockname
      )
    
    map %>% 
      addDrawToolbar(
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
                  fillOpacity = 0.1,
                  opacity = 0.4,
                  fillColor = ~factpal(stock_polygons@data$stockname), #"black",
                  color = "gray",
                  weight = 2,
                  label = stock_polygons@data$stockname,
                  labelOptions = labelOptions(sticky = FALSE, noHide = FALSE, textOnly = TRUE, direction = "center")) %>% 
      addPolygons(data = survey_polygons,
                  layerId = ~polyid,
                  group = "stockname",
                  popup = ~htmltools::htmlEscape(paste("You have selected the following survey unit: ", survey_polygons@data$polyid, 
                                                       ". It is found in the ", survey_polygons@data$stockname, 
                                                       " stock. In ", most_recent_year, 
                                                       ", the harbor seal abundance estimate for this survey unit was ", 
                                                       round(survey_polygons@data$abund_est, 2), " with a confidence interval of ", 
                                                       round(survey_polygons@data$abund_b95, 2), "-", round(survey_polygons@data$abund_t95, 2), 
                                                       ", and the 8-year trend in harbor seal abundance was ", round(survey_polygons@data$trend_est, 2), 
                                                       " seals per year. ", survey_polygons@data$survey_date, sep = "")),
                  weight = 1,
                  fillColor = ~pal(survey_polygons@data$abund_est),
                  color = ~pal(survey_polygons@data$abund_est),
                  opacity = 0.7,
                  fillOpacity = get_opacity(as.vector(survey_polygons@data$abund_est), abund_bins),
                  label = survey_polygons@data$polyid,
                  labelOptions = labelOptions(sticky = TRUE, noHide = FALSE, textOnly = TRUE, direction = "center")) %>% 
      addLegend("bottomleft",
                pal = pal,
                values = abund_bins,
                title = "Abundance:") %>% 
      addControl(HTML(paste(most_recent_year, "Estimated Abundance")), position = "topright") %>% 
      
      #Set view to the middle of all the polygons' coordinates
      setView(lng = map.view$values[1], lat = map.view$values[2], zoom = 4) 
    
  })
  
  
  #Reactive dataset for abundance and effort plots
  plotted.data <- reactiveValues(values = abundance)
  
  #Reactive dataset for trend plot - aimed towards future implementations of trend inputs
  trend.data <- reactiveValues(values = abundance)
  
  #reactive value that determines the title of the plots:
  #When the user filters by stock, it displays the stock.
  #when the user filters by shape, it says "in the selected polygons"
  title.label <- reactiveVal(value = "in All Stocks")
  
  zoom.to.stock <- reactiveVal(value = FALSE)
  
  
  #### Update reactive dataset when button is pressed ####
  observeEvent(input$update, {
    # Print("button pressed!")
    if(input$filter == "Stock"){
      
      # Update the dataset to be filtered by the selected stock
      if(input$stock.select != "All"){
        plotted.data$values <- abundance %>% filter(stockname == input$stock.select)
        
        
        #code for changing dataset for trend: (have to repeat in each if/else block)
        
        #if (input$trend.input.data == Abundance){
        #trend.data$values <- abundance %>% filter(stockname == input$stock.select)
        #}
        # else{
        #   trend.data$values <- otherdataset %>% filter(stockname == input$stock.select
        # }
        
        title.label(paste("in the", input$stock.select, "Stock"))
      }
      else{
        plotted.data$values <- abundance
        title.label("in All Stocks")
      }
      zoom.to.stock(TRUE)
    }
    
    else if(input$filter == "Polygon"){
      
      #If there is no drawn shape, revert to default data
      #(otherwise there is a fatal error and the r session is aborted)
      if(is.null(input$map1_draw_new_feature) || (input$map1_draw_new_feature$properties$feature_type == "circle")){
        plotted.data$values <- abundance
        title.label("in All Stocks")
      }
      else{
        
        feature <- input$map1_draw_new_feature
        #print(feature)
        
        feature_x <- c()
        feature_y <- c()
        
        #Obtain the coordinates of the polygon
        for (i in 1:length(feature$geometry$coordinates[[1]])){
          feature_x <- append(feature_x, feature$geometry$coordinates[[1]][[i]][[1]])
          feature_y <- append(feature_y, feature$geometry$coordinates[[1]][[i]][[2]])
        }
        
        #Check if the centroid of each existing polygon is inside of the new one
        overlapping_ids <- c()
        for(j in 1:length(survey_polygons@polygons)){
          centroid_x <- survey_polygons@polygons[[j]]@labpt[1]
          centroid_y <- survey_polygons@polygons[[j]]@labpt[2]
          if(is_inside_polygon(feature_x, feature_y, centroid_x, centroid_y)){
            # essentially uses the coordinate list to create a new polygon (on a canvas, not spatial), then checks if the point is in that polygon
            overlapping_ids <- append(overlapping_ids, survey_polygons@data$polygon_id[j])
          }
        }
        
        #Subset the data accordingly
        overlapping_data <- subset(survey_polygons@data, survey_polygons@data$polygon_id %in% overlapping_ids)
        
        
        plotted.data$values <- subset(abundance, abundance$polyid %in% overlapping_data$polyid)
        #create reactive dataset for the trend plot itself- change based on what user has selected
        
        title.label("in the Selected Survey Units")
      }
      zoom.to.stock(TRUE)
    }
    else if(input$filter == "Circle"){
      
      #If there is no drawn shape, revert to default data
      #(otherwise there is a fatal error and the r session is aborted)
      if(is.null(input$map1_draw_new_feature) || (input$map1_draw_new_feature$properties$feature_type != "circle")){
        plotted.data$values <- abundance
        title.label("in All Stocks")
      }
      else{
        
        #drawn feature's attributes
        feature <- input$map1_draw_new_feature
        feature_x <- feature$geometry$coordinates[[1]]
        feature_y <- feature$geometry$coordinates[[2]]
        radius = feature$properties$radius
        
        
        # #data frame of the centroids
        coords <- data.frame(lat = survey_polygons@data$centroid.y, lon = survey_polygons@data$centroid.x)
        # View(coords)
        
        
        # the circle's location
        me <- data.frame(lat = feature_y, lon = feature_x)
        
        #obtaining the coordinates within radius of the circle
        overlapping <- spatialrisk::points_in_circle(coords, me$lon[1], me$lat[1], radius = radius)
        #View(overlapping)
        
        #Subset the data accordingly
        overlapping_data <- subset(survey_polygons@data, survey_polygons@data$centroid.y %in% overlapping$lat)
        
        #update dataset
        plotted.data$values <- subset(abundance, abundance$polyid %in% overlapping_data$polyid)
        
        title.label("Within the Given Radius")
      }
      zoom.to.stock(TRUE)
    }
    else if (input$filter == "Survey Unit"){
      if(is.null(input$map1_shape_click$id)){
        plotted.data$values <- abundance
        title.label("in All Stocks")
      }
      else{
        plotted.data$values <- subset(abundance, abundance$polyid == input$map1_shape_click$id)
        
        title.label(paste("in Survey Unit:", input$map1_shape_click$id))
      }
      zoom.to.stock(TRUE)
    }
    
    #survey_polygons subset where the ids match the ones in plotted.data$values
    
    leafletProxy("map1") %>% 
      clearGroup(group = "lines")
    
    currently_plotted_ids <- subset(survey_polygons, survey_polygons@data$polyid %in% plotted.data$values$polyid)
    
    
    leafletProxy("map1") %>% 
      addPolylines(data = currently_plotted_ids,
                   fillOpacity = 0,
                   opacity = 0.4,
                   color = "#59FAFC",
                   weight = 3,
                   #layerId = ~polyid
                   group = "lines")
    
    if(zoom.to.stock()){
      
      View(currently_plotted_ids)
      
      subset_x_centroid <- mean(currently_plotted_ids@data$centroid.x)
      subset_y_centroid <- mean(currently_plotted_ids@data$centroid.y)
      
      
      leafletProxy("map1") %>% 
        setView(subset_x_centroid, subset_y_centroid, zoom = 6)
    }
    
    
  })
  
  observeEvent(input$default,{
    
    plotted.data$values <- abundance
    title.label("in All Stocks")
    # map.view$values <- c(mean_x, mean_y)
    leafletProxy("map1") %>% 
      setView(mean_x, mean_y, zoom = 4) %>% 
      clearGroup(group = "lines")
    #c ould try to put any drawn shapes in a group and clear that group
    
  })
  
  ## Plots -----------------------------------------
  
  # Plot for abundance
  output$plot1 <- renderHighchart({
    
    abund_subset <- plotted.data$values %>%
      filter(!is.na(year)) %>%
      group_by(year) %>% 
      summarise("total.abund" = sum(abund_est), 
                "low.abund" = sum(abund_b95), 
                "high.abund" = sum(abund_t95),
                "effort" = sum(surveyed)) 
    
    # inferno_colors <-  unique(pal(survey_polygons@data$abund_est))
    
    # A caption for the chart is available if we'd be interested in including that
    # Data labels in plotOptions -> line -> dataLabels
    
    highchart() %>%
      hc_xAxis(title = list(text = "Year", style = list(color = "#FFFFFF",  fontSize = "16px")),
               labels = list(style = list(color = "#FFFFFF"))) %>%
      hc_yAxis_multiples(list(title = list(text = "# of Harbor Seals", style = list(color = "#FFFFFF", fontSize = "16px")),
                              labels = list(format = '{value}', style = list(color = "#FFFFFF")),
                              min = min(abund_subset$low.abund),
                              max = max(abund_subset$high.abund),
                              showFirstLabel = TRUE,
                              showLastLabel = TRUE,
                              opposite = FALSE),
                         list(title = list(text = "# of Survey Units Flown", style = list(color = "#FFFFFF", fontSize = "16px")),
                              min = 0,
                              max = max(abund_subset$effort),
                              labels = list(format = "{value}", style = list(color = "#FFFFFF")),
                              showLastLabel = TRUE, 
                              opposite = TRUE)) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(data = abund_subset,
                    type = "column",
                    hcaes(x = year, y = effort),
                    color = "#855278",
                    name = "Survey Effort",
                    opacity = 0.8,
                    yAxis = 1) %>%
      hc_add_series(data = abund_subset,
                    type = "arearange",
                    hcaes(x = year, low = low.abund, high = high.abund),
                    color = "#E6C6B5",
                    opacity = 0.25,
                    name = "95th Percentile Confidence Interval") %>%
      hc_add_series(data = abund_subset,
                    type="line",
                    lineWidth = 4,
                    hcaes(x = year, y = total.abund),
                    color = "#E55C30", #"#ED6925",
                    name = "Estimated Abundance") %>%
      hc_title(text = paste("Estimated Harbor Seal Abundance by Year", title.label()),
               style = list(color = "#FFFFFF")) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF"))
  })
  
  #Plot for trend
  output$plot2 <- renderHighchart({
    
    # Could have been done at the same time as the other filter but helps isolate problems when they arise
    trend_subset <- plotted.data$values %>% 
      dplyr::filter(!is.na(trend_est)) %>%
      group_by(year) %>% 
      summarise("total.trend" = sum(trend_est), 
                "low.trend" = sum(trend_b95), 
                "high.trend" = sum(trend_t95),
                "effort" = sum(surveyed))
    
    highchart() %>%
      hc_xAxis(title = list(text = "Year", style = list(color = "#FFFFFF",  fontSize = "16px")),
               labels = list(style = list(color = "#FFFFFF"))) %>%
      hc_yAxis_multiples(list(title = list(text = "Trend", style = list(color = "#FFFFFF", fontSize = "16px")),
                              labels = list(format = '{value}', style = list(color = "#FFFFFF")),
                              min = min(trend_subset$low.trend),
                              max = max(trend_subset$high.trend),
                              showFirstLabel = TRUE,
                              showLastLabel = TRUE,
                              opposite = FALSE),
                         list(title = list(text = "# of Survey Units Flown", style = list(color = "#FFFFFF", fontSize = "16px")),
                              min = 0,
                              max = max(trend_subset$effort),
                              labels = list(format = "{value}", style = list(color = "#FFFFFF")),
                              showLastLabel = TRUE, 
                              opposite = TRUE)) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(data = trend_subset,
                    type = "column",
                    hcaes(x = year, y = effort),
                    color = "#855278",
                    name = "Survey Effort",
                    opacity = 0.8,
                    yAxis = 1) %>%
      hc_add_series(data = trend_subset,
                    type = "arearange",
                    hcaes(x = year, low = low.trend, high = high.trend),
                    color = "#E6C6B5",
                    opacity = 0.25,
                    name = "95th Percentile Confidence Interval") %>%
      hc_add_series(data = trend_subset,
                    type="line",
                    hcaes(x = year, y = total.trend),
                    color = "#E55C30",
                    name = "Trend") %>%
      hc_title(text = paste(input$num.years,"-Year Trend in Estimated Harbor Seal Abundance By Year ", title.label(), sep = ""),
               style = list(color = "#FFFFFF")) %>% 
      hc_legend(itemStyle = list(color = "#FFFFFF"))
    
  })
}

## Run the application -----------------------------------------
shinyApp(ui = ui, server = server)
