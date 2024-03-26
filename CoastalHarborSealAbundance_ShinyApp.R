# Harbor Seal Abundance: Shiny App
# Original code written by Allison James
# Code updated and maintained by Stacie Koslovsky


## Create functions -----------------------------------------

# Function to install packages needed
install_pkg <- function(x){
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Function to return RData file with user-specified name
load_rdata <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

calculate_ci <- function(subset, ci_type, group_by_ci, select_ci){
  subset_abund_ci <- subset %>%
    group_by_at(group_by_ci) %>%
    arrange(abund) %>%
    mutate(rank = sequence(n())) %>%
    ungroup() %>%
    filter(rank == 25 | rank == 975) %>%
    select(!select_ci) %>%
    unique() %>%
    pivot_wider(names_from = rank, values_from = abund) %>%
    rename(!!(paste0(as.name(ci_type), "_b95")) := "25",
           !!(paste0(as.name(ci_type), "_t95")) := "975")
}

# Function to process data cube to desired output
calculate_abundance_trend <- function(data_cube, subset_type, group_by_var, most_recent_year = NULL, poly_metadata = NULL, filter = NULL) {
  # Filter data for the abundance/trend estimates
  if(subset_type == "most_recent"){
    subset <- data_cube %>%
      filter(year == most_recent_year)
  }
  if(subset_type == "all"){
    subset <- data_cube
  }
  if(subset_type == "stock"){
    subset <- data_cube %>%
      filter(stockname == filter)
  }
  if(subset_type == "poly_in_list"){
    subset <- data_cube %>%
      filter(polyid %in% filter) 
  }
  
  # Create list of polys being processed based on the subset_type
  polys <- unique(subset$polyid)
  
  # Create subset of poly_metadata based on the data selected above (for creating surveyed abundance estimate for each year)
  if(subset_type != "most_recent") {
    subset_effort <- poly_metadata %>%
      filter(polyid %in% polys)
    
    subset_abund_effort <- subset %>%
      inner_join(subset_effort, by = c("polyid", "year")) %>%
      group_by_at(group_by_var) %>%
      summarise(abund = sum(abund)) %>%
      ungroup() %>%
      group_by(year) %>%
      summarise(abund_surveyed = mean(abund))
  }


  # Create summary dataset
  subset <- subset %>%
    group_by_at(group_by_var) %>%
    summarise(abund = sum(abund)) %>%
    ungroup()
  
  if(subset_type == "most_recent"){
    subset_abund_ci <- calculate_ci(subset, ci_type = "abund", group_by_ci = c("polyid", "year"), select_ci = c("cube", "year")) 
    
    subset_abund <- subset %>%
      group_by(polyid) %>%
      summarise(abund_est = mean(abund)) %>%
      left_join(subset_abund_ci, by = "polyid")

    # Add trend?
  } else {
    subset_abund_ci <- calculate_ci(subset, ci_type = "abund", group_by_ci = c("year"), select_ci = c("cube")) 
    
    subset_abund <- subset %>%
      group_by(year) %>%
      summarise(abund_est = mean(abund)) %>%
      left_join(subset_abund_ci, by = "year") 
    
    # Add trend?
  }
  
  # subset_trend <- subset %>%
  #   arrange(cube, year) %>%
  #   mutate(trend_est1 = roll::roll_lm(x = as.matrix(rep.int(1, nrow(subset))),
  #                                    y = as.matrix(abund), 
  #                                    width = 1, 
  #                                    intercept = FALSE)$coefficients) %>%
  #   mutate(trend_est8 = roll::roll_lm(x = as.matrix(rep.int(1, nrow(subset))), #1:8, #as.matrix(year - min(subset$year)),
  #                                     y = as.matrix(abund), 
  #                                     #weights = 8:1,
  #                                     width = 8, 
  #                                     intercept = FALSE)$coefficients) %>%
  #   mutate(trend_test = 100*(exp(trend_est8)-1))
  #
  # subset_trend <- subset %>%
  #   arrange(cube, year) %>%
  #   mutate(trend_est = as.numeric(0))
  # 
  # min_year <- min(subset_trend$year) + 7
  # 
  # for (i in 8:nrow(subset_trend)){
  #   # Skip first 8 years in each group 
  #   if(subset_trend$year[i] < min_year) next
  #   
  #   # Calculate trend
  #   trend_value <- 100 * exp(coef(lm(I(log(y))~x, data.frame(x = as.matrix(1:8), y = as.matrix(subset_trend$abund[(i-7):i]))))[2] - 1)
  #   subset_trend$trend_est[i] <- trend_value
  #   }
  # 
  # test <- roll::roll_lm(
  #   x         = as.matrix(subset$year),
  #   y         = as.matrix(subset$abund), 
  #   width     = 8, 
  #   intercept = FALSE
  # )$coefficients
  
  # Temporary fix for trend data...
  subset_trend <- subset_abund %>%
    rename(trend_est = abund_est,
           trend_b95 = abund_b95,
           trend_t95 = abund_t95)
  
  # Get survey effort data
  if(subset_type == "most_recent"){
    subset_summ <- subset_abund %>%
      left_join(subset_trend, by = c("polyid")) 
  } else {
    subset_summ <- subset_abund %>%
      left_join(subset_trend, by = "year") %>%
      left_join(subset_abund_effort, by = c("year")) %>%
      mutate(abund_surveyed = ifelse(is.na(abund_surveyed), 0, abund_surveyed)) %>%
      mutate(effort = round(abund_surveyed * 100 / abund_est, 2))
  }
  
  return(subset_summ)
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


## Get and process data from GitHub -----------------------------------------

# Poly metadata
url.poly_metadata <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/CoastalPv_AbundanceApp/main/Data/tbl_effort_4shiny.csv"
poly_metadata <- read.csv(url(url.poly_metadata)) %>%
  rename(year = effort_year) %>%
  filter(year != 'NULL' & year != 1111) %>%
  mutate(year = as.numeric(year)) %>%
  select(polyid, year, surveyed, last_surveyed) 

last_surveyed <- poly_metadata %>%
  select(polyid, last_surveyed) %>%
  unique()

poly_metadata <- poly_metadata %>%
  select(polyid, year, surveyed)

# Abundance data cube
url.data_cube <- "C://smk/akpv_datacube.rda"
data_cube <- load_rdata(url.data_cube) %>% 
  data.frame() %>%
  rownames_to_column() %>%
  rename(polyid = rowname) %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year",
    values_to = "abund") %>%
  mutate(cube = sub('.*\\.', '', year)) %>%
  mutate(cube = ifelse(substring(cube, 1, 1) == "X", 0, cube),
         year = as.numeric(substring(year, 2, 5))) 

# Geojson file with polygons data
url.poly <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/CoastalPv_AbundanceApp/main/Data/survey_polygons.geojson"
survey_polygons <- geojson_read(url.poly, what = "sp") %>%
  sf::st_as_sf(crs = 4326) %>%
  select(-stockid, -trendpoly, -station, -distance_km, -iliamna, -glacier_name, -behm_canal) %>%
  rename(polygon_id = id) %>%
  left_join(last_surveyed, by = "polyid")

# Geojson file with the stocks
url.stocks <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/CoastalPv_AbundanceApp/main/Data/survey_stocks.geojson"
stock_polygons <- geojson_read(url.stocks, what = "sp") %>%
  sf::st_as_sf(crs = 4326)

rm(url.data_cube, url.poly, url.poly_metadata, url.stocks, last_surveyed)

stocknames <- survey_polygons %>%
  select(polyid, stockname) %>%
  st_drop_geometry()

## Prepare survey_polygons and stock_polygons for map -----------------------------------------
# Filter the data cube so that it's the most recent year while removing NA values
most_recent_year <- max(data_cube$year)

# Create dataset of abundance from most-recent year
abundance_most_recent <- calculate_abundance_trend(data_cube = data_cube, group_by_var = c('polyid', 'cube', 'year'), subset_type = 'most_recent', most_recent_year = most_recent_year) %>% 
  left_join(poly_metadata %>% filter(year == most_recent_year), by = "polyid") 

# Join the polygons data with the most recent abundance estimates
survey_polygons <- survey_polygons %>% 
  left_join(abundance_most_recent, by = "polyid") %>% 
  mutate(surveyed = ifelse(is.na(surveyed), 0, surveyed)) %>%
  mutate(survey_date = ifelse(is.na(last_surveyed), 
                              "This survey unit has not been surveyed.",
                              paste0("This survey unit was last surveyed on ", last_surveyed, "."))) %>%
  filter(!is.na(abund_est))

# Add stockname to data_cube (after most recent abundance estimates are generated to avoid having the field twice)
data_cube <- data_cube %>%
  left_join(stocknames, by = "polyid")

rm(abundance_most_recent, stocknames)

## Move the polygons across the dateline so the Aleutians are not separated 
# And calculate the midpoint of polygons to set the view of the map

# Move survey polygons across dateline
survey_polygons$geometry <- (sf::st_geometry(survey_polygons) + c(360,90)) %% c(360) - c(0,90)
survey_polygons$centroid.x <- st_coordinates(sf::st_centroid(survey_polygons))[,1]
survey_polygons$centroid.y <- st_coordinates(sf::st_centroid(survey_polygons))[,2]

# Move stock polygons across dateline
stock_polygons$geometry <- (sf::st_geometry(stock_polygons) + c(360,90)) %% c(360) - c(0,90)

# Calculate the center point of the centroids (x and y)
mean_x = (max(survey_polygons$centroid.x) + min(survey_polygons$centroid.x)) / 2
mean_y = (max(survey_polygons$centroid.y) + min(survey_polygons$centroid.y)) / 2


# Create default abundance dataset for app --------------------------------------
abundance <- calculate_abundance_trend(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'all', poly_metadata = poly_metadata) 


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
                selectInput(inputId = "num.years",
                            label = div(style = "font-size:16px", "Number of Years for Trend"),
                            choices = c(3, 5, 8),
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
              # tabPanel("Trend", highchartOutput(outputId = "plot2")),
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
      domain = na.omit(survey_polygons$abund_est),
      bins = abund_bins,
      pretty = FALSE,
      na.color = "#00000000"
    )
    
    factpal <- colorFactor(
      palette = "mako", 
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
                  fillOpacity = 0.1,
                  opacity = 0.4,
                  fillColor = ~factpal(stock_polygons$stockname), #"black",
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
      addLegend("bottomleft",
                pal = pal,
                values = abund_bins,
                title = "Abundance:") %>% 
      addControl(HTML(paste(most_recent_year, "Estimated Abundance")), position = "topright") %>% 
      
      #Set view to the middle of all the polygons' coordinates
      setView(lng = map.view$values[1], lat = map.view$values[2], zoom = 4) 
    
  })
  
  
  # Reactive dataset for abundance and effort plots
  plotted.data <- reactiveValues(values = abundance)
  
  # Reactive dataset for trend plot - aimed towards future implementations of trend inputs
  trend.data <- reactiveValues(values = abundance)
  
  # Reactive value that determines the title of the plots
  title.label <- reactiveVal(value = "in All Stocks")
  
  # Reactive value that zooms map to centroid of selected features
  zoom.to.stock <- reactiveVal(value = FALSE)
  
  
  #### Update reactive dataset when button is pressed 
  observeEvent(input$update, {
    
    # Process data if "stock" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(input$filter == "Stock"){
      # Update the dataset to be filtered by the selected stock
      if(input$stock.select != "All"){
        stock_filter <- input$stock.select
        
        plotted.data$values <- calculate_abundance_trend(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'stock', poly_metadata = poly_metadata, filter = stock_filter)
        
        currently_plotted_ids <- survey_polygons %>%
          filter(stockname == stock_filter)
        
        title.label(paste("in the", input$stock.select, "Stock"))
      }
      else {
        #currently_plotted_ids <- survey_polygons
        plotted.data$values <- abundance
        title.label("in All Stocks")
      }
      zoom.to.stock(TRUE)
    }
    
    # Process data if "polygon" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else if(input$filter == "Polygon"){
      
      # If there is no drawn shape, revert to default data (otherwise there is a fatal error and the R session is aborted)
      if(is.null(input$map1_draw_new_feature) || (input$map1_draw_new_feature$properties$feature_type == "circle")){
        #currently_plotted_ids <- survey_polygons
        plotted.data$values <- abundance
        title.label("in All Stocks")
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
        plotted.data$values <- calculate_abundance_trend(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'poly_in_list', poly_metadata = poly_metadata, filter = poly_filter) 
        
        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% poly_filter) 
        
        title.label("in the Selected Survey Units")
      }
      zoom.to.stock(TRUE)
    }
    
    # Process data if "circle" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else if(input$filter == "Circle"){
      
      # If there is no drawn circle, revert to default data (otherwise there is a fatal error and the r session is aborted)
      if(is.null(input$map1_draw_new_feature) || (input$map1_draw_new_feature$properties$feature_type != "circle")){
        #currently_plotted_ids <- survey_polygons
        plotted.data$values <- abundance
        title.label("in All Stocks")
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
        plotted.data$values <- calculate_abundance_trend(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'poly_in_list', poly_metadata = poly_metadata, filter = circle_filter) 
        
        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% circle_filter) 
        
        title.label("Within the Given Radius")
      }
      zoom.to.stock(TRUE)
    }
    
    # Process data if "survey unit" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    else if (input$filter == "Survey Unit"){
      if(is.null(input$map1_shape_click$id)){
        #currently_plotted_ids <- survey_polygons
        plotted.data$values <- abundance
        title.label("in All Stocks")
      }
      else {
        singlePoly_filter <- input$map1_shape_click$id
        
        # Calculate custom abundance estimates for app
        plotted.data$values <- calculate_abundance_trend(data_cube = data_cube, group_by_var = c('cube', 'year'), subset_type = 'poly_in_list', poly_metadata = poly_metadata, filter = singlePoly_filter)
        
        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% singlePoly_filter) 
        
        title.label(paste("in Survey Unit:", singlePoly_filter))
      }
      zoom.to.stock(TRUE)
    }
    
    # Map survey_polygons where the ids match the ones in plotted.data$values
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
    
    plotted.data$values <- abundance
    title.label("in All Stocks")

    leafletProxy("map1") %>% 
      setView(mean_x, mean_y, zoom = 4) %>% 
      clearGroup(group = "lines")
    
  })
  
  ## Plots -----------------------------------------
  
  # Plot for abundance
  output$plot1 <- renderHighchart({
    abund_subset <- plotted.data$values

    # inferno_colors <-  unique(pal(survey_polygons$abund_est))
    
    # A caption for the chart is available if we'd be interested in including that
    # Data labels in plotOptions -> line -> dataLabels
    
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
      hc_title(text = paste("Estimated Harbor Seal Abundance by Year", title.label()),
               style = list(color = "#FFFFFF")) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF"))
  })
  
  # #Plot for trend
  # output$plot2 <- renderHighchart({
  #   
  #   # Could have been done at the same time as the other filter but helps isolate problems when they arise
  #   trend_subset <- plotted.data$values
  #   
  #   highchart() %>%
  #     hc_xAxis(title = list(text = "Year", style = list(color = "#FFFFFF",  fontSize = "16px")),
  #              labels = list(style = list(color = "#FFFFFF"))) %>%
  #     hc_yAxis_multiples(list(title = list(text = "Trend", style = list(color = "#FFFFFF", fontSize = "16px")),
  #                             labels = list(format = '{value}', style = list(color = "#FFFFFF")),
  #                             min = min(trend_subset$trend_b95),
  #                             max = max(trend_subset$trend_t95),
  #                             showFirstLabel = TRUE,
  #                             showLastLabel = TRUE,
  #                             opposite = FALSE),
  #                        list(title = list(text = "# of Survey Units Flown", style = list(color = "#FFFFFF", fontSize = "16px")),
  #                             min = 0,
  #                             max = max(trend_subset$effort),
  #                             labels = list(format = "{value}", style = list(color = "#FFFFFF")),
  #                             showLastLabel = TRUE, 
  #                             opposite = TRUE)) %>%
  #     hc_plotOptions(column = list(stacking = "normal")) %>%
  #     hc_add_series(data = trend_subset,
  #                   type = "column",
  #                   hcaes(x = year, y = effort),
  #                   color = "#855278",
  #                   name = "Survey Effort",
  #                   opacity = 0.8,
  #                   yAxis = 1) %>%
  #     hc_add_series(data = trend_subset,
  #                   type = "arearange",
  #                   hcaes(x = year, low = trend_b95, high = trend_t95),
  #                   color = "#E6C6B5",
  #                   opacity = 0.25,
  #                   name = "95th Percentile Confidence Interval") %>%
  #     hc_add_series(data = trend_subset,
  #                   type="line",
  #                   hcaes(x = year, y = trend_est),
  #                   color = "#E55C30",
  #                   name = "Trend") %>%
  #     hc_title(text = paste(input$num.years,"-Year Trend in Estimated Harbor Seal Abundance By Year ", title.label(), sep = ""),
  #              style = list(color = "#FFFFFF")) %>% 
  #     hc_legend(itemStyle = list(color = "#FFFFFF"))
  #   
  # })
}

## Run the application -----------------------------------------
shinyApp(ui = ui, server = server)