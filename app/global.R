## Install library packages -----------------------------------------
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(viridis)
library(highcharter)
library(scales)
library(geojsonio)
library(sf)

sf::sf_use_s2(FALSE)

repo_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "harbor_seal_shared.R"))
paths <- harbor_seal_paths(repo_root)
assert_app_artifacts_exist(paths)

## Runtime helpers -----------------------------------------

# Function to assign opacity values to polygons based on abundance
get_opacity <- function(x, bins) {
  opacity_vector <- c()

  for (element in x) {
    if (element < bins[1]) {
      opacity_vector <- opacity_vector %>% append(0.01)
    } else if (element < bins[2]) {
      opacity_vector <- opacity_vector %>% append(0.3)
    } else if (element < bins[3]) {
      opacity_vector <- opacity_vector %>% append(0.4)
    } else if (element < bins[4]) {
      opacity_vector <- opacity_vector %>% append(0.5)
    } else if (element < bins[5]) {
      opacity_vector <- opacity_vector %>% append(0.6)
    } else if (element < bins[6]) {
      opacity_vector <- opacity_vector %>% append(0.7)
    } else if (element < bins[7]) {
      opacity_vector <- opacity_vector %>% append(0.8)
    } else {
      opacity_vector <- opacity_vector %>% append(0.9)
    }
  }
  return(opacity_vector)
}

## Load data -----------------------------------------
stock_polygons  <- sf::st_read(paths$stock_polygons, quiet = TRUE) %>% sf::st_transform(4326)
haulout         <- sf::st_read(paths$haulout, quiet = TRUE) %>% sf::st_transform(4326)
survey_polygons <- sf::st_read(paths$survey_polygons, quiet = TRUE) %>% sf::st_transform(4326)

poly_metadata       <- load_rdata(paths$poly_metadata)
data_cube           <- load_rdata(paths$data_cube)
trend_linear_all    <- load_rdata(paths$trend_linear_all)
trend_linear_stock  <- load_rdata(paths$trend_linear_stock)
trend_linear_polyid <- load_rdata(paths$trend_linear_polyid)
trend_prop_all      <- load_rdata(paths$trend_prop_all)
trend_prop_stock    <- load_rdata(paths$trend_prop_stock)
trend_prop_polyid   <- load_rdata(paths$trend_prop_polyid)

message("All data loaded into memory")

## Prepare geometries -----------------------------------------
most_recent_year <- max(data_cube$year)

# 2. Shift longitudes cleanly using native sf objects
survey_polygons <- sf::st_shift_longitude(survey_polygons)
stock_polygons  <- sf::st_shift_longitude(stock_polygons)
haulout         <- sf::st_shift_longitude(haulout)

# 3. Calculate Centroids Safely
centroids <- sf::st_coordinates(sf::st_centroid(survey_polygons))
survey_polygons$centroid.x <- centroids[, 1]
survey_polygons$centroid.y <- centroids[, 2]

# 4. Extract Bounding Box and provide a Fail-Safe for Leaflet Center
bbox <- sf::st_bbox(survey_polygons)

if (any(is.na(bbox)) || length(bbox) == 0) {
  # Default fallback map center if data happens to load completely empty
  mean_x <- 180 
  mean_y <- 60
} else {
  mean_x <- as.numeric((bbox["xmax"] + bbox["xmin"]) / 2)
  mean_y <- as.numeric((bbox["ymax"] + bbox["ymin"]) / 2)
}

# Create field to store information provided in popup for survey_polygons
survey_polygons <- survey_polygons %>%
  mutate(p_positive = as.numeric(ifelse(is.na(p_positive), 0, p_positive))) %>%
  mutate(
    popup_text = ifelse(
      is.na(iliamna), # change to iliamna == 'N' after next running of PrepData4App
      ifelse(
        abund_est == 0,
        paste0(
          "You have selected survey unit ",
          polyid,
          ", found in the ",
          stockname,
          " stock. Harbor seals have not been observed in this survey unit. ",
          survey_date
        ),
        paste0(
          "You have selected survey unit ",
          polyid,
          ", found in the ",
          stockname,
          " stock. In ",
          most_recent_year,
          ", the harbor seal abundance estimate for this survey unit was ",
          round(abund_est, 2),
          " with a confidence interval of ",
          round(abund_b95, 2),
          "-",
          round(abund_t95, 2),
          ". The current 8-year trend in harbor seal abundance was based on abundance estimates from ",
          most_recent_year - 8,
          "-",
          most_recent_year,
          " and was estimated as ",
          round(trend_est, 2),
          " seals per year; the probability of ",
          ifelse(
            p_positive >= 0.50,
            paste0("population increase was ", p_positive, ". "),
            paste0("population decline was ", 1 - p_positive, ". ")
          ),
          survey_date
        )
      ),
      "The counts for harbor seals in survey units at Iliamna Lake are analyzed in a process separate from the rest of survey area. More information can be found
      in the resources provided in Data Access section."
    )
  )

message("survey_polygons dataset created")

# Create default abundance and trend datasets for app --------------------------------------
abundance <- calculate_abundance(
  data_cube = data_cube,
  group_by_var = c('cube', 'year'),
  subset_type = 'all',
  poly_metadata = poly_metadata
)

trend <-
  (trend_linear_all %>%
    mutate(identifier = "all") %>%
    mutate(trend_type = "linear_all")) %>%
  rbind(
    trend_linear_stock %>%
      rename(identifier = stockname) %>%
      mutate(trend_type = "linear_stock")
  ) %>%
  rbind(
    trend_linear_polyid %>%
      rename(identifier = polyid) %>%
      mutate(trend_type = "linear_polyid")
  ) %>%
  rbind(
    trend_prop_all %>%
      mutate(identifier = "all") %>%
      mutate(trend_type = "prop_all")
  ) %>%
  rbind(
    trend_prop_stock %>%
      rename(identifier = stockname) %>%
      mutate(trend_type = "prop_stock")
  ) %>%
  rbind(
    trend_prop_polyid %>%
      rename(identifier = polyid) %>%
      mutate(trend_type = "prop_polyid")
  )

message("abundance and trend layers are created")

## Prepare information for ShinyApp -----------------------------------------

# Initialize the map
map <- survey_polygons %>%
  leaflet(
    options = leafletOptions(worldCopyJump = FALSE)
  ) %>%
  addTiles()

# Initialize informational windows
introduction <- div(p(
  "This application allows users to explore over 20 years of harbor seal population abundance and trend information within Alaska. Harbor seals are
  found throughout much of Alaska's near-coastal marine waters and are an important indicator of healthy ecosystems. The Alaska Fisheries Science Center
  (AFSC) has conducted aerial surveys for harbor seals in Alaska nearly every year since 1998. These aerial survey counts along with statistical modeling that accounts
  for population dynamics and the proportion of seals in the water during surveys allows for estimates of abundance and trend across different spatial and temporal scales.

  
  More information about our harbor seal research can be found ",
  a(
    "here",
    href = "https://www.fisheries.noaa.gov/alaska/marine-mammal-protection/harbor-seal-research-alaska"
  ),
  "."
))

instructions <- div(p("This map displays polygons that represent survey units of harbor seals in Alaska, symbolized based on the most recent abundance estimates; polygons with larger seal
  populations are both darker in color and less transparent. Hover over the survey unit polygon for more specific information about that particular site. The larger gray polygons represent
  each harbor seal stock. Hover over the stock polygon to get the name of the stock.

  
  Two figures represent summary information for the survey units shown in the map. The figures represent summary information for all the survey units, until a filter is applied.",
  tags$ul(
    tags$li("The ", strong("Abundance"), " figure displays the total harbor seal abundance, the 95th percentile confidence interval, and the associated survey effort for all or the filtered survey units."),
    tags$li("The", strong("Trend"), " plot displays a predicted trend, the 95th percentile confidence interval, and the associated survey effort for all or the filtered survey units. The user can specify
            the number of years of abundance data and thee type of abundance data (estimates or log of estimates) on which the trend should be calculated.
            ")
  ),
  
  "Survey units (polygons) can be selected dynamically within the map, and the associated figures are updated dynamically when you click the \"Update Plot\"
  button after making the filter selection. Filter options are as follows:", 
  tags$ul(
    tags$li(strong("By Stock"), " - use the drop-down menu to filter the data by harbor seal stock."),
    tags$li(strong("By Survey Unit"), " - click on a single survey unit (polygon) within the map."),
    tags$li(strong("By Custom Polygon"), " - use the pentagon button in the map to start drawing a user-defined custom polygon. Use the trash can button in the map to delete your custom polygon.
            Only one polygon can be drawn at a time. The centroid of each survey unit must be encompassed within the drawn shape in order for it to be included in the filter."),
    tags$li(strong("By Custom Circle"), " - use the circle button in the map to start drawing a circle at the starting point of interest. As the circle size changes, the radius of the circle
  is displayed.")
  )
))

disclaimer <- "This is a prototype application. While the best efforts have been made to insure the highest quality, tools such as this are under constant development and are
  subject to change. This application is developed and maintained by scientists at the NOAA Fisheries Alaska Fisheries Science Center and should not be construed as official
  communication of NMFS, NOAA, or the U.S. Dept. of Commerce. Links and mentions of RStudio and Shiny should not be considered an endorsement by NOAA Fisheries or the U.S.
  Federal Government."

contact_info <- div(p("This application was developed by Allison James as part of a summer 2022 internship, jointly sponsored by UW CICOES and NOAA Fisheries.", 
                br(),
                "The application is maintained by Stacie Koslovsky (stacie.koslovsky@noaa.gov).",
                br(),
                "For questions regarding the harbor seal aerial survey project, contact Josh London (josh.london@noaa.gov), and
                for questions regarding the statistical methods used to calculate the harbor seal abundance estimates, contact Brett McClintock (brett.mcclintock@noaa.gov)."))

data_access <- div(p( "The data we are using to power this application are publicly available for viewing and download. Links to each of these datasets are below: ",
  tags$ul(
    tags$li(a(
    "Alaska Harbor Seal Aerial Survey Units",
    href = "https://www.arcgis.com/home/item.html?id=c63ccb17b9b144c4a529ee6a3d039665"
    )),
    tags$li(
      a(
        "Alaska Harbor Seal Abundance",
        href = "https://www.arcgis.com/home/item.html?id=e69222ad91564422aba9ee0d2e70bfe2"
      )
    ),
    tags$li(
      a(
        "Alaska Harbor Seal Haul-Out Locations",
        href = "https://www.arcgis.com/home/item.html?id=2c6ca3e595024d3990127bfe061d7ed3"
      )
    )
  ),
  "For more information about abundance estimates for the Iliamna Lake survey units, please refer to the following resources:",
  tags$ul(
    tags$li(
      a(
        "2018 Boveng et al. report",
        href = "https://onlinelibrary.wiley.com/doi/full/10.1111/risa.12988"
      )
    ),
    tags$li(
       a(
        "1984-2013 dataset",
        href = "https://catalog.data.gov/dataset/a-dataset-of-aerial-survey-counts-of-harbor-seals-in-iliamna-lake-alaska-1984-20133"
      )
    )
  )
))
