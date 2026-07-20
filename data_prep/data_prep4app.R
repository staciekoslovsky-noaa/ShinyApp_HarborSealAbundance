# Prep data for Harbor Seal Abundance Shiny App

## Get functions -----------------------------------------
source(
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\GitHub\\ShinyApp_HarborSealAbundance\\app\\functions.R"
)
install_pkg("tidyverse")
install_pkg("RPostgreSQL")
install_pkg("geojsonio")
install_pkg("sf")

## Process data ---------------------------------------------
setwd("C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/data")

# Connect to DB and get starting data
con <- RPostgreSQL::dbConnect(
  PostgreSQL(),
  dbname = Sys.getenv("pep_db"),
  host = Sys.getenv("pep_ip"),
  user = Sys.getenv("pep_admin"),
  password = Sys.getenv("admin_pw")
)

# # CREATE stock_polygons ~~~~~~~~~~~~~~~~~~~
stock_polygons <- sf::st_read(
  con,
  query = "SELECT * FROM stock.geo_dist_pv",
  geometry_column = "geom"
) %>%
  sf::st_transform(4326) %>%
  sf::st_shift_longitude()

# EXPORT stock_polygons
geojsonio::geojson_write(
  stock_polygons,
  geometry = "polygon",
  file = "survey_stocks.geojson"
)

# CREATE haulout ~~~~~~~~~~~~~~~~~~~
haulout <- sf::st_read(
  con,
  query = "SELECT * FROM surv_pv_cst.geo_haulout_20220414",
  geometry_column = "geom"
) %>%
  select(name) %>%
  sf::st_transform(4326) %>%
  sf::st_shift_longitude()

# EXPORT haulout
geojsonio::geojson_write(
  haulout,
  geometry = "point",
  file = "survey_haulout.geojson"
)


# CREATE poly_metadata and last_surveyed ~~~~~~~~~~~~~~~~~~~
tbl_effort_4shiny <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT * FROM surv_pv_cst.tbl_effort_4Shiny"
) %>%
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
survey_polygons <- sf::st_read(
  con,
  query = "SELECT * FROM surv_pv_cst.geo_polys",
  geometry_column = "geom"
) %>%
  sf::st_as_sf(crs = 4326) %>%
  select(
    -stockid,
    -trendpoly,
    -station,
    -distance_km, #-iliamna, -glacier_name,
    -behm_canal
  ) %>%
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
url.data_cube <- "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/akpv_datacube.rda"
data_cube <- load_rdata(url.data_cube) %>%
  data.frame() %>%
  rownames_to_column() %>%
  rename(polyid = rowname) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "abund"
  ) %>%
  mutate(cube = sub('.*\\.', '', year)) %>%
  mutate(
    cube = ifelse(substring(cube, 1, 1) == "X", 0, cube),
    year = as.numeric(substring(year, 2, 5))
  ) %>%
  left_join(stock_names, by = "polyid")

# EXPORT data_cube
save(
  data_cube,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/data_cube.rda"
) # Update to wd folder once data are shareable


# CREATE trend tables ~~~~~~~~~~~~~~~~~~~
data_cube_4trend <- load_rdata(url.data_cube)
rm(url.data_cube)
data_cube_polys <- rownames(data_cube_4trend[[1]])
year_first <- min(data_cube$year)
year_last <- max(data_cube$year)

# CREATE trend for p(increase|decrease)
n_years <- year_last - year_first + 1
pop <- matrix(NA, nrow = 1000, ncol = n_years)
maxi <- n_years
trend_length <- 8

trend_p_positive <- data.frame(polyid = character(), p_positive = numeric())

for (p in 1:length(data_cube_polys)) {
  # takes 4-5 hours to run
  print(p)
  pop <- matrix(
    unlist(lapply(data_cube_4trend, function(x) {
      x[data_cube_polys[p], ]
    })),
    nrow = 1000,
    ncol = n_years
  )
  trend_matrix <- generate_trend_matrix(
    trend_type = "linear",
    maxi,
    trend_length,
    pop
  )

  trend_p_temp <- data.frame(last_trend = trend_matrix[, 20]) %>%
    filter(last_trend >= 0) %>%
    count() %>%
    mutate(polyid = data_cube_polys[p], p_positive = n / 1000) %>%
    select(-n)

  trend_p_positive <- trend_p_positive %>%
    rbind(trend_p_temp)
}

# EXPORT trend_linear_all
trend_linear_all <- calculate_trend(
  data_cube_4trend,
  trend_type = "linear",
  group_by = "all",
  group_list = "NA",
  year_first,
  year_last
)
save(
  trend_linear_all,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/trend_linear_all.rda"
) # Update to wd folder once data are shareable

# EXPORT trend_linear_stock
trend_linear_stock <- calculate_trend(
  data_cube_4trend,
  trend_type = "linear",
  group_by = "stock",
  group_list = stock_ids,
  year_first,
  year_last
)
save(
  trend_linear_stock,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/trend_linear_stock.rda"
) # Update to wd folder once data are shareable

# EXPORT trend_linear_polyid (takes several hours to run)
trend_linear_polyid <- calculate_trend(
  data_cube_4trend,
  trend_type = "linear",
  group_by = "polyid",
  group_list = data_cube_polys,
  year_first,
  year_last
)
save(
  trend_linear_polyid,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/trend_linear_polyid.rda"
) # Update to wd folder once data are shareable

# EXPORT trend_prop_all
trend_prop_all <- calculate_trend(
  data_cube_4trend,
  trend_type = "proportional",
  group_by = "all",
  group_list = "NA",
  year_first,
  year_last
)
save(
  trend_prop_all,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/trend_prop_all.rda"
) # Update to wd folder once data are shareable

# EXPORT trend_prop_stock
trend_prop_stock <- calculate_trend(
  data_cube_4trend,
  trend_type = "proportional",
  group_by = "stock",
  group_list = stock_ids,
  year_first,
  year_last
)
save(
  trend_prop_stock,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/trend_prop_stock.rda"
) # Update to wd folder once data are shareable

# EXPORT trend_prop_polyid (takes several hours to run) ## Rerun after feedback from Brett!
trend_prop_polyid <- calculate_trend(
  data_cube_4trend,
  trend_type = "proportional",
  group_by = "polyid",
  group_list = data_cube_polys,
  year_first,
  year_last
)
save(
  trend_prop_polyid,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/trend_prop_polyid.rda"
) # Update to wd folder once data are shareable

# CREATE default abundance dataset ~~~~~~~~~~~~~~~~~~~
abundance <- calculate_abundance(
  data_cube = data_cube,
  group_by_var = c('cube', 'year'),
  subset_type = 'all',
  poly_metadata = poly_metadata
)
save(
  abundance,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/default_abundance.rda"
) # Update to wd folder once data are shareable


# CREATE default trend dataset ~~~~~~~~~~~~~~~~~~~
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
save(
  trend,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/default_trend.rda"
) # Update to wd folder once data are shareable


# CREATE survey_polygons (with most recent abundance + trend) ~~~~~~~~~~~~~~~~~~~

# Create dataset of abundance from most-recent year
abundance_most_recent <- calculate_abundance(
  data_cube = data_cube,
  group_by_var = c('polyid', 'cube', 'year'),
  subset_type = 'most_recent',
  most_recent_year = year_last
) %>%
  left_join(poly_metadata %>% filter(year == year_last), by = "polyid")

# Join the polygons data with the most recent abundance estimates
survey_polygons <- survey_polygons %>%
  left_join(abundance_most_recent, by = "polyid") %>%
  left_join(trend_prop_polyid %>% filter(year == year_last), by = "polyid") %>%
  left_join(trend_p_positive, by = "polyid") %>%
  mutate(surveyed = ifelse(is.na(surveyed), 0, surveyed)) %>%
  mutate(
    survey_date = ifelse(
      is.na(last_surveyed),
      "This survey unit has not been surveyed.",
      paste0("This survey unit was last surveyed on ", last_surveyed, ".")
    )
  ) %>%
  mutate(abund_est = ifelse(is.na(abund_est), 0, abund_est)) %>%
  mutate(abund_b95 = ifelse(is.na(abund_b95), 0, abund_b95)) %>%
  mutate(abund_t95 = ifelse(is.na(abund_t95), 0, abund_t95)) %>%
  mutate(trend_est = ifelse(is.na(trend_est), 0, trend_est)) %>%
  mutate(trend_b95 = ifelse(is.na(trend_b95), 0, trend_b95)) %>%
  mutate(trend_t95 = ifelse(is.na(trend_t95), 0, trend_t95)) %>%
  filter(!is.na(abund_est)) %>%
  select(
    polyid,
    stockname,
    abund_est,
    abund_b95,
    abund_t95,
    trend_est,
    trend_b95,
    trend_t95,
    survey_date,
    iliamna,
    glacier_name,
    p_positive,
    geom
  ) %>%
  sf::st_transform(4326) %>%
  sf::st_shift_longitude() %>%
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

# Calculate centroids and assign to survey_polygons
centroids <- sf::st_coordinates(sf::st_centroid(survey_polygons))
survey_polygons <- survey_polygons %>%
  mutate(
    centroid.x = centroids[, 1],
    centroid.y = centroids[, 2]
  )

# EXPORT survey_polygons
geojsonio::geojson_write(
  survey_polygons,
  geometry = "polygon",
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/ShinyApp_HarborSealAbundance/not_to_share/4app/survey_polygons.geojson"
) # Update to wd folder once data are shareable


# Clean up workspace
rm(
  abundance_most_recent,
  data_cube_4trend,
  stock_names,
  tbl_effort_4shiny,
  trend,
  trend_matrix,
  trend_temp,
  data_cube_polys,
  g,
  group_by,
  group_list,
  i,
  maxi,
  n_years,
  stock_ids,
  trend_length,
  year_first,
  year_last,
  pop,
  trend_type
)
