# Prep data for Harbor Seal Abundance Shiny App

install_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE)) {
      stop("Package not found")
    }
  }
}

required_packages <- c("tidyverse", "RPostgreSQL", "geojsonio", "sf")
invisible(lapply(required_packages, install_pkg))
invisible(lapply(required_packages, library, character.only = TRUE))

repo_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "harbor_seal_shared.R"))

paths <- harbor_seal_paths(repo_root)
ensure_directory(paths$app_data_dir)
ensure_directory(paths$app_artifacts_dir)
assert_file_exists(paths$data_cube_input, "Data cube input")

message("Writing shared app data to: ", paths$app_data_dir)
message("Writing derived app artifacts to: ", paths$app_artifacts_dir)

connect_to_pep_db <- function() {
  RPostgreSQL::dbConnect(
    RPostgreSQL::PostgreSQL(),
    dbname = Sys.getenv("pep_db"),
    host = Sys.getenv("pep_ip"),
    user = Sys.getenv("pep_admin"),
    password = Sys.getenv("admin_pw")
  )
}

con <- connect_to_pep_db()
on.exit(RPostgreSQL::dbDisconnect(con), add = TRUE)

# CREATE stock_polygons ~~~~~~~~~~~~~~~~~~~
stock_polygons <- sf::st_read(
  con,
  query = "SELECT * FROM stock.geo_dist_pv",
  geometry_column = "geom"
)
geojsonio::geojson_write(
  stock_polygons,
  geometry = "polygon",
  file = paths$stock_polygons
)

# CREATE haulout ~~~~~~~~~~~~~~~~~~~
haulout <- sf::st_read(
  con,
  query = "SELECT * FROM surv_pv_cst.geo_haulout_20220414",
  geometry_column = "geom"
) %>%
  select(name)
geojsonio::geojson_write(
  haulout,
  geometry = "polygon",
  file = paths$haulout
)

# CREATE poly_metadata and last_surveyed ~~~~~~~~~~~~~~~~~~~
tbl_effort_4shiny <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT * FROM surv_pv_cst.tbl_effort_4Shiny"
) %>%
  rename(year = effort_year) %>%
  filter(year != "NULL" & year != 1111) %>%
  mutate(year = as.numeric(year)) %>%
  select(polyid, year, surveyed, last_surveyed)

poly_metadata <- tbl_effort_4shiny %>%
  select(polyid, year, surveyed)
save(poly_metadata, file = paths$poly_metadata)

last_surveyed <- tbl_effort_4shiny %>%
  select(polyid, last_surveyed) %>%
  unique()
save(last_surveyed, file = paths$last_surveyed)

# START survey_poly (before joined to abundance data) ~~~~~~~~~~~~~~~~~~~
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
    -distance_km,
    -behm_canal
  ) %>%
  rename(polygon_id = id) %>%
  left_join(last_surveyed, by = "polyid")

stock_names <- survey_polygons %>%
  select(polyid, stockname) %>%
  st_drop_geometry()

stock_groups <- split(stock_names$polyid, stock_names$stockname)

# CREATE data_cube ~~~~~~~~~~~~~~~~~~~
data_cube_4trend <- load_rdata(paths$data_cube_input)

data_cube <- data_cube_4trend %>%
  data.frame() %>%
  rownames_to_column() %>%
  rename(polyid = rowname) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "abund"
  ) %>%
  mutate(cube = sub(".*\\.", "", year)) %>%
  mutate(
    cube = ifelse(substring(cube, 1, 1) == "X", 0, cube),
    year = as.numeric(substring(year, 2, 5))
  ) %>%
  left_join(stock_names, by = "polyid")

save(data_cube, file = paths$data_cube)

# CREATE trend tables ~~~~~~~~~~~~~~~~~~~
year_first <- min(data_cube$year)
year_last <- max(data_cube$year)

trend_products <- calculate_trend_products(
  data_cube_4trend = data_cube_4trend,
  stock_groups = stock_groups,
  year_first = year_first,
  year_last = year_last
)

trend_linear_all <- trend_products$trend_linear_all
trend_linear_stock <- trend_products$trend_linear_stock
trend_linear_polyid <- trend_products$trend_linear_polyid
trend_prop_all <- trend_products$trend_prop_all
trend_prop_stock <- trend_products$trend_prop_stock
trend_prop_polyid <- trend_products$trend_prop_polyid
trend_p_positive <- trend_products$trend_p_positive

save(trend_linear_all, file = paths$trend_linear_all)
save(trend_linear_stock, file = paths$trend_linear_stock)
save(trend_linear_polyid, file = paths$trend_linear_polyid)
save(trend_prop_all, file = paths$trend_prop_all)
save(trend_prop_stock, file = paths$trend_prop_stock)
save(trend_prop_polyid, file = paths$trend_prop_polyid)
save(trend_p_positive, file = paths$trend_p_positive)

# CREATE survey_polygons (with most recent abundance + trend) ~~~~~~~~~~~~~~~~~~~
abundance_most_recent <- calculate_abundance(
  data_cube = data_cube,
  group_by_var = c("polyid", "cube", "year"),
  subset_type = "most_recent",
  most_recent_year = year_last
) %>%
  left_join(poly_metadata %>% filter(year == year_last), by = "polyid")

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
  )

geojsonio::geojson_write(
  survey_polygons,
  geometry = "polygon",
  file = paths$survey_polygons
)
