# Prep data for Harbor Seal Abundance Shiny App

## Get functions -----------------------------------------
source("C:\\Users\\Stacie.Hardy\\Work\\SMK\\GitHub\\ShinyApp_HarborSealAbundance\\HarborSealAbundance_Functions.R")
install_pkg("tidyverse")
install_pkg("RPostgreSQL")
install_pkg("geojsonio")

## Process data ---------------------------------------------
setwd("C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/Data")

# Connect to DB and get starting data
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password =  Sys.getenv("admin_pw"))

# CREATE stock_polygons ~~~~~~~~~~~~~~~~~~~
stock_polygons <- sf::st_read(con, query = "SELECT * FROM stock.geo_dist_pv", geometry_column = "geom") %>%
# EXPORT stock_polygons
geojsonio::geojson_write(stock_polygons, geometry = "polygon", file = "survey_stocks.geojson")

# CREATE haulout ~~~~~~~~~~~~~~~~~~~
haulout <- sf::st_read(con, query = "SELECT * FROM surv_pv_cst.geo_haulout_20220414", geometry_column = "geom") %>%
  select(name) 
# EXPORT haulout
geojsonio::geojson_write(haulout, geometry = "polygon", file = "survey_haulout.geojson")

# CREATE poly_metadata and last_surveyed ~~~~~~~~~~~~~~~~~~~
tbl_effort_4shiny <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM surv_pv_cst.tbl_effort_4Shiny") %>%
  rename(year = effort_year) %>%
  filter(year != 'NULL' & year != 1111) %>%
  mutate(year = as.numeric(year)) %>%
  select(polyid, year, surveyed, last_surveyed) 

poly_metadata <- tbl_effort_4shiny %>%
  select(polyid, year, surveyed)
# EXPORT poly_metadata
save(poly_metadata, file = "poly_metadata.rda")

last_surveyed <- tbl_effort_4shiny %>%
  select(polyid, last_surveyed) %>%
  unique()
# EXPORT last_surveyed
save(last_surveyed, file = "last_surveyed.rda")



# START survey_poly (before joined to abundance data) ~~~~~~~~~~~~~~~~~~~
#url.poly <- "https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_HarborSealAbundance/main/Data/survey_polygons.geojson"
#survey_polygons <- geojsonio::geojson_read(url.poly, what = "sp") %>%
survey_polygons <- sf::st_read(con, query = "SELECT * FROM surv_pv_cst.geo_polys", geometry_column = "geom") %>%
  sf::st_as_sf(crs = 4326) %>%
  select(-stockid, -trendpoly, -station, -distance_km, -iliamna, -glacier_name, -behm_canal) %>%
  rename(polygon_id = id) %>%
  left_join(last_surveyed, by = "polyid")

# Create Stocknames for data processing steps: create table for joining to abundance data cube based on survey polygon data
stocknames <- survey_polygons %>%
  select(polyid, stockname) %>%
  st_drop_geometry()



# CREATE data_cube ~~~~~~~~~~~~~~~~~~~
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
         year = as.numeric(substring(year, 2, 5))) %>%
  left_join(stocknames, by = "polyid")

# EXPORT data_cube
save(data_cube, file = "C://smk/4app/data_cube.rda") # Update to wd folder once data are shareable



# CREATE survey_polygons (with most recent abundance) ~~~~~~~~~~~~~~~~~~~
most_recent_year <- max(data_cube$year)

# Create dataset of abundance from most-recent year
abundance_most_recent <- calculate_abundance(data_cube = data_cube, group_by_var = c('polyid', 'cube', 'year'), subset_type = 'most_recent', most_recent_year = most_recent_year) %>% 
  left_join(poly_metadata %>% filter(year == most_recent_year), by = "polyid") 

# Join the polygons data with the most recent abundance estimates
survey_polygons <- survey_polygons %>% 
  left_join(abundance_most_recent, by = "polyid") %>% 
  mutate(surveyed = ifelse(is.na(surveyed), 0, surveyed)) %>%
  mutate(survey_date = ifelse(is.na(last_surveyed), 
                              "This survey unit has not been surveyed.",
                              paste0("This survey unit was last surveyed on ", last_surveyed, "."))) %>%
  filter(!is.na(abund_est)) %>%
  select(polyid, stockname, abund_est, abund_b95, abund_t95, trend_est, trend_b95, trend_t95, survey_date, geom)

# EXPORT survey_polygons
geojsonio::geojson_write(survey_polygons, geometry = "polygon", file = "C://smk/4app/survey_polygons.geojson") # Update to wd folder once data are shareable

# Trend data (overall)

# Trend data (by stock)

# Trend data (by polyid)
