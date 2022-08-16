
#### Create functions ####

# Function to install packages needed
install_pkg <- function(x){
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

#Function to check if the selected plot has ever been surveyed
has_been_surveyed <- function(x){
  
  new_vector <- c()
  
  for (element in x){
    if(!is.na(element) && !is.null(element) && element != "NA"){
      element <- paste("This plot was last surveyed on", element)
      new_vector <- new_vector %>% append(element)
    }
    else{
      #print("we got a null!")
      element <- "This plot has not been surveyed"
      new_vector <- new_vector %>% append(element)
      #print(element)
    }
  }
  return(new_vector)
  
}

#Check if a point is inside of a given polygon
is_inside_polygon <- function(x, y, point.x, point.y){
  inside = FALSE
  if (sp::point.in.polygon(point.x, point.y, x, y)){
    inside = TRUE
  }
  return(inside)
}

#Assign opacity values to polygons based on abundance
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


#### Install and library packages ####

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

#alternate way to download spatialrisk
# install_pkg("devtools")
# install_github("MHaringa/spatialrisk")



#### Download files ####

#csv with abundance data
url.abund <- "https://raw.githubusercontent.com/StacieKozHardy/CoastalPv_AbundanceApp/main/Data/geo_abundance_4shiny.csv"
abundance <- read.csv(url(url.abund))

#Make the following columns numeric for future calculations:
abundance$year <- as.numeric(abundance$year) 
abundance$abund_est <- as.numeric(abundance$abund_est)
abundance$trend_est <- as.numeric(abundance$trend_est)
abundance$abund_b95 <- as.numeric(abundance$abund_b95)
abundance$abund_t95 <- as.numeric(abundance$abund_t95)
abundance$trend_b95 <- as.numeric(abundance$trend_b95)
abundance$trend_t95 <- as.numeric(abundance$trend_t95)

#filters the data so that it's the most recent year while removing NA values
most_recent_year <- max(abundance$year, na.rm = TRUE)

abundance_filtered <- abundance %>%
  filter(year == most_recent_year) 

#Create column with total abundance by stock
abundance_filtered <- abundance_filtered %>%
  group_by(stockname) %>% 
  mutate("total.abund" = sum(abund_est))

#Geojson file with polygons data
url.poly <- "https://raw.githubusercontent.com/StacieKozHardy/CoastalPv_AbundanceApp/main/Data/survey_polygons.geojson"
survey_polygons<- geojson_read(url.poly,
                               what = "sp")


#Geojson file with the stocks
url.stocks <- "https://raw.githubusercontent.com/StacieKozHardy/CoastalPv_AbundanceApp/main/Data/survey_stocks.geojson"
stock_polygons <- geojson_read(url.stocks,
                               what = "sp")



#### Full join, Leaflet setup####

#Join the polygons data with the abundance data from the most recent year
survey_polygons@data <- survey_polygons@data %>% 
  left_join(abundance_filtered, by = "polyid")

survey_polygons@data <- survey_polygons@data %>% 
  mutate("survey_date" = has_been_surveyed(as.vector(survey_polygons@data$last_surveyed)))

#subset the data so that plots without abundance data are not included
poly_subset <- survey_polygons[!is.na(survey_polygons@data$abund_est), ]
#View(poly_subset@data)

####Move the polygons across the dateline so the Aleutians are not separated####
#and calculate the middle of the polygons to set the view of the map

x_vals <- c() #x value of each polygon's centroid
y_vals <- c() #y value of each polygon's centroid
x_stocks <- c()
y_stocks <- c()


##might be source of long runtime##
for(j in 1:length(poly_subset@polygons)){
  for(k in 1:length(poly_subset@polygons[[j]]@Polygons)){
    #Move the polygons (besides the Aleutians)
    for (i in 1:length(poly_subset@polygons[[j]]@Polygons[[k]]@coords[,1])){
      if (poly_subset@polygons[[j]]@Polygons[[k]]@coords[i, 1] < 0){
        poly_subset@polygons[[j]]@Polygons[[k]]@coords[i, 1] <- poly_subset@polygons[[j]]@Polygons[[k]]@coords[i, 1] + 360
        #if the polygon's longitude is below 0, add 360 degrees to it to move it across the map
      }
    }
    }
    #Append values of the centroids to a list for future use
    if(poly_subset@polygons[[j]]@labpt[1] < 0){
      #Move centroids 360 degrees again
      x_vals <- append(x_vals, poly_subset@polygons[[j]]@labpt[1] + 360)
      #Move centroids across the map as well
      poly_subset@polygons[[j]]@labpt[1] <- poly_subset@polygons[[j]]@labpt[1] + 360
    }
    else{
      x_vals <- append(x_vals, poly_subset@polygons[[j]]@labpt[1])
    }
  
    y_vals <- append(y_vals, poly_subset@polygons[[j]]@labpt[2])
}

#new columns with coordinates of each polygon's centroid
    poly_subset@data <- poly_subset@data %>% 
      mutate("centroid.x" = as.vector(x_vals),
             "centroid.y" = as.vector(y_vals))


for(j in 1:length(stock_polygons@polygons)){
  for(k in 1:length(stock_polygons@polygons[[j]]@Polygons)){
    #Move the polygons (besides the Aleutians)
    for (i in 1:length(stock_polygons@polygons[[j]]@Polygons[[k]]@coords[,1])){
      if (stock_polygons@polygons[[j]]@Polygons[[k]]@coords[i, 1] < 0){
        stock_polygons@polygons[[j]]@Polygons[[k]]@coords[i, 1] <- stock_polygons@polygons[[j]]@Polygons[[k]]@coords[i, 1] + 360
        #if the polygon's longitude is below 0, add 360 degrees to it to move it across the map
      }
    }
    #Append values of the centroids to a list for future use
    if(stock_polygons@polygons[[j]]@Polygons[[k]]@labpt[1] < 0){
      #Move centroids 360 degrees again
      x_stocks <- append(x_stocks, stock_polygons@polygons[[j]]@Polygons[[k]]@labpt[1] + 360)
      #Move centroids across the map as well
      stock_polygons@polygons[[j]]@Polygons[[k]]@labpt[1] <- stock_polygons@polygons[[j]]@Polygons[[k]]@labpt[1] + 360
    }
    else{
      x_stocks <- append(x_stocks, stock_polygons@polygons[[j]]@Polygons[[k]]@labpt[1])
    }
    y_stocks <- append(y_stocks, stock_polygons@polygons[[j]]@Polygons[[k]]@labpt[2])
  }
}

#Calculate the middle x value of the centroids
min_x = min(x_vals)
max_x = max(x_vals)
mean_x = (max_x + min_x) / 2

#Calculate the middle y value of the centroids
min_y = min(y_vals)
max_y = max(y_vals)
mean_y = (max_y + min_y) / 2

#Initialize the map
map <- poly_subset %>% 
  leaflet(
    options = leafletOptions(worldCopyJump = FALSE)
    ) %>% 
  addTiles()



#Initialize informational windows
introduction <- "This application allows users to explore over 20 years of harbor seal population abundance and trend information within Alaska. Harbor seals are widely distributed throughout much of Alaska’s near-coastal marine waters and are an important indicator of healthy ecosystems. The Alaska Fisheries Science Center (AFSC) has conducted aerial surveys for harbor seals in Alaska nearly every year since 1998. These aerial survey counts along with statistical modeling that accounts for population dynamics and the proportion of seals in the water during surveys allows for estimates of abundance and trend across different spatial and temporal scales."


instructions <- "This map displays polygons that represent survey units of harbor seals in Alaska, symbolized based on the 2018 abundance estimates; polygons with larger seal populations are both darker in color and less transparent. Hover over the polygon for more specific information about that particular site. The larger gray polygons represent each harbor seal stock. Hover over the polygon to get the name of the stock.<br/><br/>
          Three figures represent summary information for the survey units shown in the map. By default, the figures represent summary information for all the survey units, until a filter is applied. 
          <ui><li>The “Abundance” figure displays the total harbor seal abundance and 95th percentile confidence interval for all or the filtered survey units. 
          </li><li>The “Trend” plot displays a predicted trend (and 95th percentile confidence interval) in abundance. The user can determine the number of years to be taken into consideration, as well as whether the trend should be calculated on a logarithmic scale. 
          </li><li>The “Effort” plot displays the total number of survey units that were surveyed by year in the selected polygons.<br/><br/>
          Survey units (polygons) can be selected dynamically within the map, and the three associated figures are updated dynamically when you click the “Update Plot” button after making the filter selection. Filter options are as follows:<br/>
         <ui><li>By Stock - use the drop-down menu to filter the data by harbor seal stock.
         </li><li>By Individual Survey Unit- click a single survey unit within the map.
         </li><li>By Custom Selection - use the pentagon button in the map to start drawing a custom polygon. Use the trash can button in the map to delete your custom polygon. Only one polygon can be drawn at a time. Note, the centroid of the polygon must be encompassed within the drawn shape in order for it to be included in the filter.
         </li><li>By Radius - use the circle button in the map to start drawing a circle at the starting point of interest. As the circle size changes, the radius of the circle is displayed."

disclaimer <- "This is a prototype application. While the best efforts have been made to insure the highest quality, tools such as this are under constant development and are subject to change. This application is developed and maintained by scientists at the NOAA Fisheries Alaska Fisheries Science Center and should not be construed as official communication of NMFS, NOAA, or the U.S. Dept. of Commerce. Links and mentions of RStudio and Shiny should not be considered an endorsement by NOAA Fisheries or the U.S. Federal Government."

contact_info <- ""


#### Define UI ####
ui <- fluidPage(theme = shinytheme("superhero"),

                #Hide warnings and errors within the app
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",),
                
                titlePanel(fluidRow(
                  column(4, "Alaska Harbor Seal Abundance"),
                  column(4, h4("https://www.fisheries.noaa.gov/alaska/marine-mammal-protection/harbor-seal-research-alaska", align = "center")),
                  column(4,  img(src = "noaa_logo.png", height = 90.8, width = 209, class = "pull-right"))
                  #noaa_logo.png must be in same working directory as the app.R file
                )),
                #first row: informational box and map
                fluidRow(
                  column(4, tabsetPanel(
                    tabPanel("Introduction", HTML(introduction)),
                    tabPanel(title = "Instructions", div(style = 'overflow-y:scroll;height:350px', HTML(instructions))),
                    tabPanel("Disclaimers", HTML(disclaimer)),
                    tabPanel("Contact Information", HTML(contact_info)),
                    type = "pills" #gives the highlighted tab a background color
                    )
                    ),
                  column(8, leafletOutput(outputId = "map1"))
                  ),
                br(),
                #second row: inputs and plots
                fluidRow(
                  
                  #column with the inputs
                  column(3, verticalLayout(
                    (radioGroupButtons(inputId = "filter",
                                       label = div(style = "font-size:20px", "Filter data by:"),
                                       choices = c("Stock", "Individual Survey Unit", "Custom Selection", "Radius"),
                                       selected = "Stock",
                                       direction = "vertical",
                                       status = "danger",
                                       individual = TRUE,
                                       size = "normal",
                                       checkIcon = list(yes = (icon("ok",lib = "glyphicon")),
                                                        no = icon("remove", lib = "glyphicon")))),
                    br(),
                    (selectInput(inputId = "stock.select",
                                 label = div(style = "font-size:20px", "Choose stock to filter by:"),
                                 choices = c("All", poly_subset@data$stockname.x),
                                 selected = "All")),
                    br(),
                    (actionBttn(inputId = "update", 
                                label = "Update Plot",
                                style = "jelly",
                                size = "lg",
                                color = "danger")),
                    br(),
                    (actionBttn(inputId = "default",
                                label = "Reset",
                                style = "jelly",
                                size = "lg",
                                color = "warning"))
                    ),
                    offset = 1),
                  
                  #column with the plots (under different tabs)
                  column(8, tabsetPanel(
                    #can be changed back to being a plot output
                    tabPanel("Abundance", highchartOutput(outputId = "plot1")),
                    tabPanel("Trend", highchartOutput(outputId = "plot2")),
                    #tabPanel("Effort", highchartOutput(outputId = "plot3")),
                    type = "pills" #selected tabs use the background fill color (which for some reason is orange)
                      )
                    )
                  ),
                fluidRow(
                  column(3, verticalLayout(
                    #trend-related inputs:
                    selectInput(inputId = "trend.input.data",
                                label = div(style = "font-size:20px", "Trend Input Data"),
                                choices = c("Abundance", "Log(Abundance)"),
                                selected = "Abundance"),
                    selectInput(inputId = "num.years",
                                label = div(style = "font-size:20px", "Number of years"),
                                choices = c(5, 8, 10, 15), #can be changed later
                                selected= 8)
                  ), 
                  offset = 1),
                  #effort plot
                  column(8, highchartOutput(outputId = "plot3"))
                  
                )
)
              








#### Define server logic ####
server <- function(input, output, session) {
  
  #map starts off at center, map.view represents current view
  map.view <- reactiveValues(values = c(mean_x, mean_y))
  
  
  ####Generate Leaflet map and polygons####
    output$map1 <- renderLeaflet({
      
      abund_bins <- c(0, 10, 100, 250, 500, 1000, 2500, Inf)
      
      #Create palette to color polygons
      pal <- colorBin(
        palette = "inferno",
        reverse = TRUE,
        domain = na.omit(poly_subset@data$abund_est),
        bins = abund_bins,
        pretty = FALSE,
        na.color = "#00000000"
      )
      
      
      map %>% 
        addDrawToolbar(
          targetGroup = 'draw',
          polylineOptions = FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3),
                                              showArea = TRUE, metric = FALSE),
          rectangleOptions = FALSE, 
          #circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = FALSE, 
                                           selectedPathOptions = TRUE, 
                                           remove = TRUE),
          singleFeature = TRUE)  %>%
        addScaleBar(position = "bottomright",
                    options = scaleBarOptions(maxWidth = 250)) %>% 
        addPolygons(data = stock_polygons,
                    fillOpacity = 0.1,
                    opacity = 0.4,
                    fillColor = "gray",
                    color = "black",
                    weight = 2,
                    #sticky, textOnly, noHide
                    #label = stock_polygons@data$stockname,
                    labelOptions = labelOptions(sticky = FALSE, noHide = TRUE)) %>% 
        addPolygons(data = poly_subset,
                    layerId = ~polyid,
                    group = "stockname",
                    popup = ~htmltools::htmlEscape(paste("The selected survey unit is ", poly_subset@data$polyid, " and is found in the ", poly_subset@data$stockname.x, 
                                                         " stock. In ", most_recent_year, " the harbor seal abundance estimate for this survey unit is ", round(poly_subset@data$abund_est, 2), " with a confidence",
                                                         " interval of ", round(poly_subset@data$abund_b95, 2), "-", round(poly_subset@data$abund_t95, 2), ", and the 8-year abundance trend is ",
                                                         round(poly_subset@data$trend_est, 2), " seals per year.", sep = "")),
                    #label = poly_subset@data$stockname, 
                    weight = 1,
                    fillColor = ~pal(poly_subset@data$abund_est),
                    color = ~pal(poly_subset@data$abund_est),
                    opacity = 0.7,
                    fillOpacity = get_opacity(as.vector(poly_subset@data$abund_est), 
                                              abund_bins)) %>% 
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
      #print("button pressed!")
      if(input$filter == "Stock"){
        #Update the dataset to be filtered by the selected stock
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
      ###else if
      else if(input$filter == "Custom Selection"){
        
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
          for(j in 1:length(poly_subset@polygons)){
              centroid_x <- poly_subset@polygons[[j]]@labpt[1]
              centroid_y <- poly_subset@polygons[[j]]@labpt[2]
              if(is_inside_polygon(feature_x, feature_y, centroid_x, centroid_y)){
                #essentially uses the coordinate list to create a new polygon (on a canvas, not spatial), then checks if the point is in that polygon
                overlapping_ids <- append(overlapping_ids, poly_subset@data$id.x[j])
                }
              }
          
          #Subset the data accordingly
          overlapping_data <- subset(poly_subset@data, poly_subset@data$id.x %in% overlapping_ids)
          
          
          plotted.data$values <- subset(abundance, abundance$polyid %in% overlapping_data$polyid)
          #create reactive dataset for the trend plot itself- change based on what user has selected
          
          title.label("in the Selected Polygons")
        }
        zoom.to.stock(FALSE)
      }
      else if(input$filter == "Radius"){
        
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
          coords <- data.frame(lat = poly_subset@data$centroid.y, lon = poly_subset@data$centroid.x)
          # View(coords)
          
           
          # the circle's location
          me <- data.frame(lat = feature_y, lon = feature_x)
          
          #obtaining the coordinates within radius of the circle
          overlapping <- spatialrisk::points_in_circle(coords, me$lon[1], me$lat[1], radius = radius)
          #View(overlapping)
          
          #Subset the data accordingly
          overlapping_data <- subset(poly_subset@data, poly_subset@data$centroid.y %in% overlapping$lat)
          
          #update dataset
          plotted.data$values <- subset(abundance, abundance$polyid %in% overlapping_data$polyid)
          
          title.label("Within the Given Radius")
        }
        zoom.to.stock(FALSE)
      }
      else if (input$filter == "Individual Survey Unit"){
        if(is.null(input$map1_shape_click$id)){
          plotted.data$values <- abundance
          title.label("in All Stocks")
        }
        else{
          plotted.data$values <- subset(abundance, abundance$polyid == input$map1_shape_click$id)
          
          title.label(paste("in Polygon", input$map1_shape_click$id))
        }
        zoom.to.stock(FALSE)
      }
      
      #poly_subset subset where the ids match the ones in plotted.data$values
      
      
      
      leafletProxy("map1") %>% 
        clearGroup(group = "lines")
      
      currently_plotted_ids <- subset(poly_subset, poly_subset@data$polyid %in% plotted.data$values$polyid)
      
      
      leafletProxy("map1") %>% 
        addPolylines(data = currently_plotted_ids,
                     fillOpacity = 0,
                     opacity = 0.4,
                     color = "black",
                     weight = 3,
                     #layerId = ~polyid
                     group = "lines")
      
      if(zoom.to.stock()){
        
        View(currently_plotted_ids)
        
        subset_x_centroid <- mean(currently_plotted_ids@data$centroid.x)
        subset_y_centroid <- mean(currently_plotted_ids@data$centroid.y)
        
        
        leafletProxy("map1") %>% 
          setView(subset_x_centroid, subset_y_centroid, zoom = 5)
      }
      
      
      
      })
    
    observeEvent(input$default,{
      
      plotted.data$values <- abundance
      title.label("in All Stocks")
      #map.view$values <- c(mean_x, mean_y)
      leafletProxy("map1") %>% 
        setView(mean_x, mean_y, zoom = 4) %>% 
        clearGroup(group = "lines")
      #could try to put any drawn shapes in a group and clear that group
      
    })
    
    #### Plots ####
    
    #Plot for abundance
    output$plot1 <- renderHighchart({
      
     abund_subset <- plotted.data$values %>%
        group_by(year) %>% 
        summarise("total.abund" = sum(abund_est), "low.abund" = sum(abund_b95), "high.abund" = sum(abund_t95))
      
     #inferno_colors <-  unique(pal(poly_subset@data$abund_est))
     
     
     #a caption for the chart is available if we'd be interested in including that
     #data labels in plotOptions -> line -> dataLabels
     
      highchart() %>%
        hc_add_series(data = abund_subset,
                      type = "arearange",
                      mapping = (hcaes(x = year, low = low.abund, high = high.abund)),
                      color = "#FFFFFF",
                      name = "Confidence Interval") %>% 
        hc_add_series(data = abund_subset,
                      mapping = (hcaes(x = year, y = total.abund)), 
                      color = "#ED6925",
                      type = "line",
                      name = "Abundance",
                      #name changes the little icon that allows you to click a series on and off
                      lineWidth = 4,
                      marker = list(radius = 7)) %>% 
        hc_title(text = paste("Estimated Harbor Seal Abundance by Year", title.label()),
                 style = list(color = "#FFFFFF")) %>% 
        hc_xAxis(title = list(text = "Year", 
                              style = list(color = "#FFFFFF", 
                                           fontSize = "16px")),
                 labels = list(style = list(color = "#FFFFFF"))) %>% 
        hc_yAxis(title = list(text = "Abundance", 
                              style = list(color = "#FFFFFF", 
                                           fontSize = "16px")),
                 labels = list(style = list(color = "#FFFFFF")),
                 maxPadding = 0,
                 minPadding = 0,
                 max = max(abund_subset$high.abund),
                 endOnTick = FALSE,
                 tickAmount = 4) %>% 
        hc_legend(itemStyle = list(color = "#FFFFFF")) 
    
      })
    
    #Plot for trend
    output$plot2 <- renderHighchart({
      
      
      #could have been done at the same time as the other filter but helps isolate problems when they arise
      trend_subset <- plotted.data$values %>% 
        dplyr::filter(year > 2002) #trend data starts in 2002
      
      #trend_subset <- abundance %>% filter(year > 2002)
      
      trend_subset <- trend_subset %>%
        group_by(year) %>% 
        summarise("total.trend" = sum(trend_est), "low.trend" = sum(trend_b95), "high.trend" = sum(trend_t95))
      
      
        highchart() %>%
          hc_add_series(data = trend_subset, 
                        type = "arearange", 
                        mapping = (hcaes(x = year, low = low.trend, high = high.trend)),
                        color = "#FFFFFF",
                        name = "Confidence Interval") %>% 
          hc_add_series(data = trend_subset,
                        mapping = (hcaes(x = year, y = total.trend)),
                        type = "line",
                        color = "#ED6925",
                        name = "Trend",
                        lineWidth = 4,
                        marker = list(radius = 7)) %>% 
          hc_title(text = paste(input$num.years," -Year Trend in Estimated Harbor Seal Abundance By Year ", title.label(), sep = ""),
                   style = list(color = "#FFFFFF")) %>% 
          hc_xAxis(title = list(text = "Year", 
                                style = list(color = "#FFFFFF", 
                                             fontSize = "16px")),
                   labels = list(style = list(color = "#FFFFFF"))) %>% 
          hc_yAxis(title = list(text = "Trend", 
                                style = list(color = "#FFFFFF", 
                                             fontSize = "16px")),
                   labels = list(style = list(color = "#FFFFFF")),
                   #maxPadding = 0,
                   minPadding = 0,
                   max = max(trend_subset$high.trend),
                   endOnTick = FALSE) %>% 
          hc_legend(itemStyle = list(color = "#FFFFFF"))
       
    })
    
    #Plot for effort (total number of surveys per year in the selected polygons)
    output$plot3 <- renderHighchart({
      
      effort_subset <- plotted.data$values %>% 
        dplyr::filter(year > 2002)
      #currently no survey data before 2002
      
      effort_subset <- effort_subset %>%
        group_by(year) %>% 
        summarise("effort" = sum(surveyed))
      
      highchart() %>% 
        hc_add_series(data = effort_subset,
                      type = "column", 
                      mapping = (hcaes(x = year, y = effort)),
                      color = "#ED6925",
                      name = "Total surveys") %>% 
        hc_title(text = paste("Number of Polygons Surveyed by Year", title.label()),
                 style = list(color = "#FFFFFF")) %>% 
        hc_xAxis(title = list(text = "Year", 
                              style = list(color = "#FFFFFF", 
                                           fontSize = "16px")),
                 labels = list(style = list(color = "#FFFFFF"))) %>% 
        hc_yAxis(title = list(text = "Total Surveys", 
                              style = list(color = "#FFFFFF", 
                                           fontSize = "16px")),
                 labels = list(style = list(color = "#FFFFFF")),
                 #maxPadding = 0,
                 minPadding = 0,
                 max = max(effort_subset$effort),
                 endOnTick = FALSE) %>% 
        hc_legend(itemStyle = list(color = "#FFFFFF"))
    
      
    })
   
   
}


#### Run the application ####
shinyApp(ui = ui, server = server)

