## Install library packages -----------------------------------------
# basic shiny packages needed
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

# packages for spatial visualization on map
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

# tools for data processing
library(sf)
library(tidyverse)

# misc. tools for figure visualization
library(viridis)
library(highcharter)
library(scales)


# other app tools
library(htmltools)
library(roll)
library(arrow)


sf::sf_use_s2(FALSE)

## Load functions -----------------------------------------
source("functions.R")

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
stock_polygons <- readRDS("data/stock_polygons.rds")
haulout <- readRDS("data/survey_haulout.rds")
survey_polygons <- readRDS("../not_to_share/4app/survey_polygons.rds")
load(
  "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/poly_metadata.rda"
)
load(
  "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/abundance.rda"
)
load(
  "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/trend.rda"
)
data_cube_ds <- open_dataset(
  "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/data_cube_dataset"
)

message("All data loaded into memory")

## Prepare data for app -----------------------------------------
most_recent_year <- max(abundance$year)

# Extract Bounding Box and provide a Fail-Safe for Leaflet Center
bbox <- sf::st_bbox(survey_polygons)

if (any(is.na(bbox)) || length(bbox) == 0) {
  # Default fallback map center if data happens to load completely empty
  mean_x <- 180
  mean_y <- 60
} else {
  mean_x <- as.numeric((bbox["xmax"] + bbox["xmin"]) / 2)
  mean_y <- as.numeric((bbox["ymax"] + bbox["ymin"]) / 2)
}

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

instructions <- div(p(
  "This map displays polygons that represent survey units of harbor seals in Alaska, symbolized based on the most recent abundance estimates; polygons with larger seal
  populations are both darker in color and less transparent. Hover over the survey unit polygon for more specific information about that particular site. The larger gray polygons represent
  each harbor seal stock. Hover over the stock polygon to get the name of the stock.

  
  Two figures represent summary information for the survey units shown in the map. The figures represent summary information for all the survey units, until a filter is applied.",
  tags$ul(
    tags$li(
      "The ",
      strong("Abundance"),
      " figure displays the total harbor seal abundance, the 95th percentile confidence interval, and the associated survey effort for all or the filtered survey units."
    ),
    tags$li(
      "The",
      strong("Trend"),
      " plot displays a predicted trend, the 95th percentile confidence interval, and the associated survey effort for all or the filtered survey units. The user can specify
            the number of years of abundance data and thee type of abundance data (estimates or log of estimates) on which the trend should be calculated.
            "
    )
  ),

  "Survey units (polygons) can be selected dynamically within the map, and the associated figures are updated dynamically when you click the \"Update Plot\"
  button after making the filter selection. Filter options are as follows:",
  tags$ul(
    tags$li(
      strong("By Stock"),
      " - use the drop-down menu to filter the data by harbor seal stock."
    ),
    tags$li(
      strong("By Survey Unit"),
      " - click on a single survey unit (polygon) within the map."
    ),
    tags$li(
      strong("By Custom Polygon"),
      " - use the pentagon button in the map to start drawing a user-defined custom polygon. Use the trash can button in the map to delete your custom polygon.
            Only one polygon can be drawn at a time. The centroid of each survey unit must be encompassed within the drawn shape in order for it to be included in the filter."
    ),
    tags$li(
      strong("By Custom Circle"),
      " - use the circle button in the map to start drawing a circle at the starting point of interest. As the circle size changes, the radius of the circle
  is displayed."
    )
  )
))

disclaimer <- "This is a prototype application. While the best efforts have been made to insure the highest quality, tools such as this are under constant development and are
  subject to change. This application is developed and maintained by scientists at the NOAA Fisheries Alaska Fisheries Science Center and should not be construed as official
  communication of NMFS, NOAA, or the U.S. Dept. of Commerce. Links and mentions of RStudio and Shiny should not be considered an endorsement by NOAA Fisheries or the U.S.
  Federal Government."

contact_info <- div(p(
  "This application was developed by Allison James as part of a summer 2022 internship, jointly sponsored by UW CICOES and NOAA Fisheries.",
  br(),
  "The application is maintained by Stacie Koslovsky (stacie.koslovsky@noaa.gov).",
  br(),
  "For questions regarding the harbor seal aerial survey project, contact Josh London (josh.london@noaa.gov), and
                for questions regarding the statistical methods used to calculate the harbor seal abundance estimates, contact Brett McClintock (brett.mcclintock@noaa.gov)."
))

data_access <- div(p(
  "The data we are using to power this application are publicly available for viewing and download. Links to each of these datasets are below: ",
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
