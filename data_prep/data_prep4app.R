# Prep data for Harbor Seal Abundance Shiny App

## Get functions -----------------------------------------
source(
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\GitHub\\shiny_app_pv_abundance\\app\\functions.R"
)
library("tidyverse")
library("RPostgreSQL")
library("sf")
library("arrow")

## Process data ---------------------------------------------
setwd(
  "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/app/data"
)
sf::sf_use_s2(FALSE)

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
  sf::st_transform(3338) %>%
  sf::st_simplify() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_shift_longitude()

# EXPORT stock_polygons
saveRDS(
  stock_polygons,
  file = "stock_polygons.rds"
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
saveRDS(
  haulout,
  file = "survey_haulout.rds"
)

# Create poly_metadata and last_surveyed ~~~~~~~~~~~~~~~~~~~
# For use in memory
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
save(
  poly_metadata,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/poly_metadata.rda"
) # Update to wd folder once data are shareable

last_surveyed <- tbl_effort_4shiny %>%
  select(polyid, last_surveyed) %>%
  unique()


# START survey_poly (before joined to abundance data) ~~~~~~~~~~~~~~~~~~~
survey_polygons <- sf::st_read(
  con,
  query = "SELECT * FROM surv_pv_cst.geo_polys",
  geometry_column = "geom"
) %>%
  sf::st_transform(3338) %>%
  sf::st_simplify() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_shift_longitude() %>%
  select(
    -stockid,
    -trendpoly,
    -station,
    -distance_km, #-iliamna, -glacier_name,
    -behm_canal
  ) %>%
  rename(polygon_id = id) %>%
  left_join(last_surveyed, by = "polyid")

# Create stocknames for data processing steps: create table for joining to abundance data cube based on survey polygon data
stock_names <- survey_polygons %>%
  select(polyid, stockname) %>%
  st_drop_geometry()

# Create glacial names list
glacial_polys <- survey_polygons %>%
  filter(!is.na(glacier_name)) %>%
  select(polyid) %>%
  st_drop_geometry()

stock_ids <- unique(stock_names$stockname) # for trend calculations

RPostgreSQL::dbDisconnect(con)
rm(con)


# CREATE data_cube ~~~~~~~~~~~~~~~~~~~
load(
  "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/akpv_datacube.rda"
)
data_cube <- akpv_datacube %>%
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
rm(akpv_datacube)

# EXPORT data_cube (to parquet format for access from the application)
write_dataset(
  data_cube,
  path = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/data_cube_dataset",
  format = "parquet",
  partitioning = "stockname"
) # Update to wd folder once data are shareable


# Prepare trend data ~~~~~~~~~~~~~~~~~~~
data_cube_polys <- unique(data_cube$polyid)
year_first <- min(data_cube$year)
year_last <- max(data_cube$year)
n_years <- year_last - year_first + 1
n_sims <- length(data_cube_4trend)
pop <- matrix(NA, nrow = n_sims, ncol = n_years)
maxi <- n_years
trend_length <- 8

# CREATE trend for p(increase|decrease)
p_positive <- numeric(length(data_cube_polys))

for (p in seq_along(data_cube_polys)) {
  print(p)

  pop <- vapply(
    data_cube_4trend,
    function(x) x[data_cube_polys[p], ],
    numeric(n_years)
  )

  pop <- t(pop) # rows = simulations, cols = years

  trend_matrix <- generate_trend_matrix(
    trend_type = "linear",
    maxi = maxi,
    trend_length = trend_length,
    pop = pop
  )

  p_positive[p] <- mean(trend_matrix[, 20] >= 0, na.rm = TRUE)
}

trend_p_positive <- data.frame(
  polyid = data_cube_polys,
  p_positive = p_positive
)

rm(maxi, n_years, pop, trend_length, p, trend_matrix)

# CREATE trend datasets
# trend_linear_all
trend_linear_all <- calculate_trend(
  data_cube = data_cube,
  trend_type = "linear",
  group_by = "all",
  year_first = year_first,
  year_last = year_last
)

# trend_linear_stock
trend_linear_stock <- calculate_trend(
  data_cube = data_cube,
  trend_type = "linear",
  group_by = "stock",
  group_list = sort(unique(data_cube$stockname)),
  year_first = year_first,
  year_last = year_last
)

# trend_linear_polyid
trend_linear_polyid <- calculate_trend(
  data_cube = data_cube,
  trend_type = "linear",
  group_by = "polyid",
  group_list = sort(unique(data_cube$polyid)),
  year_first = year_first,
  year_last = year_last
)

# trend_linear_glacial
trend_linear_glacial <- calculate_trend(
  data_cube = data_cube %>% filter(polyid %in% glacial_polys$polyid),
  trend_type = "linear",
  group_by = "all",
  year_first = year_first,
  year_last = year_last
)

# trend_prop_all
trend_prop_all <- calculate_trend(
  data_cube = data_cube,
  trend_type = "proportional",
  group_by = "all",
  year_first = year_first,
  year_last = year_last
)

# trend_prop_stock
trend_prop_stock <- calculate_trend(
  data_cube = data_cube,
  trend_type = "proportional",
  group_by = "stock",
  group_list = sort(unique(data_cube$stockname)),
  year_first = year_first,
  year_last = year_last
)

# trend_prop_polyid
trend_prop_polyid <- calculate_trend(
  data_cube = data_cube,
  trend_type = "proportional",
  group_by = "polyid",
  group_list = sort(unique(data_cube$polyid)),
  year_first = year_first,
  year_last = year_last
)

# trend_prop_glacial
trend_prop_glacial <- calculate_trend(
  data_cube = data_cube %>% filter(polyid %in% glacial_polys$polyid),
  trend_type = "proportional",
  group_by = "all",
  year_first = year_first,
  year_last = year_last
)

# EXPORT trend dataset
trend <- rbind(
  trend_linear_all %>%
    mutate(identifier = "all") %>%
    mutate(trend_type = "linear_all"),
  trend_linear_stock %>%
    rename(identifier = stockname) %>%
    mutate(trend_type = "linear_stock"),
  trend_linear_polyid %>%
    rename(identifier = polyid) %>%
    mutate(trend_type = "linear_polyid"),
  trend_linear_glacial %>%
    rename(identifier = "glacial") %>%
    mutate(trend_type = "linear_glacial"),
  trend_prop_all %>%
    mutate(identifier = "all") %>%
    mutate(trend_type = "prop_all"),
  trend_prop_stock %>%
    rename(identifier = stockname) %>%
    mutate(trend_type = "prop_stock"),
  trend_prop_polyid %>%
    rename(identifier = polyid) %>%
    mutate(trend_type = "prop_polyid"),
  trend_prop_glacial %>%
    rename(identifier = "glacial") %>%
    mutate(trend_type = "prop_glacial")
)
save(
  trend,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/trend.rda"
) # Update to wd folder once data are shareable


# CREATE abundance datasets ~~~~~~~~~~~~~~~~~~~
abundance_all <- calculate_abundance(
  data_cube = data_cube,
  group_by = "all",
  poly_metadata = poly_metadata
)

abundance_stock <- calculate_abundance(
  data_cube = data_cube,
  group_by = "stock",
  group_list = sort(unique(data_cube$stockname)),
  poly_metadata = poly_metadata
)

abundance_polyid <- calculate_abundance(
  data_cube = data_cube,
  group_by = "polyid",
  group_list = sort(unique(data_cube$polyid)),
  poly_metadata = poly_metadata
)

abundance_glacial <- calculate_abundance(
  data_cube = data_cube %>% filter(polyid %in% glacial_polys$polyid),
  group_by = "all",
  poly_metadata = poly_metadata
)

# EXPORT abundance dataset
abundance <- rbind(
  abundance_all %>%
    mutate(abundance_type = "all") %>%
    mutate(identifier = "all"),
  abundance_stock %>%
    mutate(abundance_type = "stock") %>%
    rename(identifier = stockname),
  abundance_polyid %>%
    mutate(abundance_type = "polyid") %>%
    rename(identifier = polyid),
  abundance_glacial %>%
    mutate(abundance_type = "glacial") %>%
    rename(identifier = "glacial")
)
save(
  abundance,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/abundance.rda"
) # Update to wd folder once data are shareable


# CREATE survey_polygons (with most recent abundance + trend) ~~~~~~~~~~~~~~~~~~~

# Create dataset of abundance from most-recent year
abundance_most_recent <- abundance_polyid %>%
  filter(year == year_last) %>%
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
  mutate(p_positive = as.numeric(ifelse(is.na(p_positive), 0, p_positive))) %>%
  mutate(
    popup_text = ifelse(
      iliamna == 'N',
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

# EXPORT survey_polygons (CHANGE MAPPING LOCATION ONCE APP IS SHAREABLE)
saveRDS(
  survey_polygons,
  file = "C:/Users/Stacie.Hardy/Work/SMK/GitHub/shiny_app_pv_abundance/not_to_share/4app/survey_polygons.rds"
)

# Clean up workspace
rm(
  abundance_most_recent,
  abundance_all,
  abundance_polyid,
  abundance_stock,
  trend_linear_all,
  trend_linear_stock,
  trend_linear_polyid,
  trend_prop_all,
  trend_prop_stock,
  trend_prop_polyid,
  trend_p_positive,
  centroids,
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
  n_sims,
  p_positive,
  stock_ids,
  trend_length,
  year_first,
  year_last,
  pop,
  trend_type
)
