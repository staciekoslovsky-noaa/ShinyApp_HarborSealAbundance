# Prep data for Harbor Seal Abundance Shiny App

## Get functions -----------------------------------------
source("C:\\Users\\Stacie.Hardy\\Work\\SMK\\GitHub\\ShinyApp_HarborSealAbundance\\HarborSealAbundance_Functions.R")
install_pkg("tidyverse")
install_pkg("RPostgreSQL")
install_pkg("geojsonio")
install_pkg("sf")

## Process data ---------------------------------------------
setwd("C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/Data")

# Connect to DB and get starting data
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password =  Sys.getenv("admin_pw"))

# # CREATE stock_polygons ~~~~~~~~~~~~~~~~~~~ EXPORT IS NOT WORKING CORRECTLY....
# stock_polygons <- sf::st_read(con, query = "SELECT * FROM stock.geo_dist_pv", geometry_column = "geom") 
# # EXPORT stock_polygons
# geojsonio::geojson_write(stock_polygons, geometry = "polygon", file = "survey_stocks.geojson")

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
stock_names <- survey_polygons %>%
  select(polyid, stockname) %>%
  st_drop_geometry()

stock_ids <- unique(stock_names$stockname) # for trend calculations

RPostgreSQL::dbDisconnect(con)
rm(con)



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
  left_join(stock_names, by = "polyid")

# EXPORT data_cube
save(data_cube, file = "C://smk/4app/data_cube.rda") # Update to wd folder once data are shareable





# CREATE trend tables ~~~~~~~~~~~~~~~~~~~
data_cube_4trend <- load_rdata(url.data_cube) 
rm(url.data_cube)
data_cube_polys <- rownames(data_cube_4trend[[1]])
year_first <- min(data_cube$year)
year_last <- max(data_cube$year)

# EXPORT trend_linear_all
trend_linear_all <- calculate_trend(data_cube_4trend, trend_type = "linear", group_by = "all", group_list = "NA", year_first, year_last) 
save(trend_linear_all, file = "C://smk/4app/trend_linear_all.rda") # Update to wd folder once data are shareable

# EXPORT trend_linear_stock
trend_linear_stock <- calculate_trend(data_cube_4trend, trend_type = "linear", group_by = "stock", group_list = stock_ids, year_first, year_last) 
save(trend_linear_stock, file = "C://smk/4app/trend_linear_stock.rda") # Update to wd folder once data are shareable

# EXPORT trend_linear_polyid
trend_linear_polyid <- calculate_trend(data_cube_4trend, trend_type = "linear", group_by = "polyid", group_list = data_cube_polys, year_first, year_last) 
save(trend_linear_polyid, file = "C://smk/4app/trend_linear_polyid.rda") # Update to wd folder once data are shareable

# EXPORT trend_prop_all
trend_prop_all <- calculate_trend(data_cube_4trend, trend_type = "proportional", group_by = "all", group_list = "NA", year_first, year_last) 
save(trend_prop_all, file = "C://smk/4app/trend_prop_all.rda") # Update to wd folder once data are shareable

# EXPORT trend_prop_stock
trend_prop_stock <- calculate_trend(data_cube_4trend, trend_type = "proportional", group_by = "stock", group_list = stock_ids, year_first, year_last)
save(trend_prop_stock, file = "C://smk/4app/trend_prop_stock.rda") # Update to wd folder once data are shareable

# EXPORT trend_prop_polyid
trend_prop_polyid <- calculate_trend(data_cube_4trend, trend_type = "proportional", group_by = "polyid", group_list = data_cube_polys, year_first, year_last)
###### CHANGE TO trend_prop_polyid once code is working!
save(trend_linear_polyid, file = "C://smk/4app/trend_prop_polyid.rda") # Update to wd folder once data are shareable




# CREATE survey_polygons (with most recent abundance + trend) ~~~~~~~~~~~~~~~~~~~

# Create dataset of abundance from most-recent year
abundance_most_recent <- calculate_abundance(data_cube = data_cube, group_by_var = c('polyid', 'cube', 'year'), subset_type = 'most_recent', most_recent_year = year_last) %>% 
  left_join(poly_metadata %>% filter(year == year_last), by = "polyid") 

# Join the polygons data with the most recent abundance estimates
survey_polygons <- survey_polygons %>% 
  left_join(abundance_most_recent, by = "polyid") %>% 
  left_join(trend_prop_polyid %>% filter (year == year_last), by = "polyid") %>%
  mutate(surveyed = ifelse(is.na(surveyed), 0, surveyed)) %>%
  mutate(survey_date = ifelse(is.na(last_surveyed), 
                              "This survey unit has not been surveyed.",
                              paste0("This survey unit was last surveyed on ", last_surveyed, "."))) %>%
  filter(!is.na(abund_est)) %>%
  select(polyid, stockname, abund_est, abund_b95, abund_t95, trend_est, trend_b95, trend_t95, survey_date, geom)

# EXPORT survey_polygons
geojsonio::geojson_write(survey_polygons, geometry = "polygon", file = "C://smk/4app/survey_polygons.geojson") # Update to wd folder once data are shareable



# Clean up workspace
rm(abundance_most_recent, data_cube_4trend, stock_names, tbl_effort_4shiny, trend, trend_matrix, trend_temp, 
   data_cube_polys, g, group_by, group_list, i, maxi, n_years, stock_ids, trend_length, year_first, year_last, pop, trend_type)
