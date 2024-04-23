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

# Function to calculate confidence intervals
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
calculate_abundance <- function(data_cube, subset_type, group_by_var, most_recent_year = NULL, poly_metadata = NULL, filter = NULL) {
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
    
  } else {
    subset_abund_ci <- calculate_ci(subset, ci_type = "abund", group_by_ci = c("year"), select_ci = c("cube")) 
    
    subset_abund <- subset %>%
      group_by(year) %>%
      summarise(abund_est = mean(abund)) %>%
      left_join(subset_abund_ci, by = "year") 
  }
  
  # Get survey effort data
  if(subset_type == "most_recent"){
    subset_summ <- subset_abund # %>%
      # left_join(subset_trend, by = c("polyid")) 
  } else {
    subset_summ <- subset_abund %>%
      # left_join(subset_trend, by = "year") %>%
      left_join(subset_abund_effort, by = c("year")) %>%
      mutate(abund_surveyed = ifelse(is.na(abund_surveyed), 0, abund_surveyed)) %>%
      mutate(effort = round(abund_surveyed * 100 / abund_est, 2))
  }
  
  return(subset_summ)
}

# Function to generate trend matrix (which feeds into calculating trend)
generate_trend_matrix <- function(trend_type, maxi, trend_length, pop) {
  trend_matrix <- NULL
  
  if (trend_type == "linear") {
    for(i in 1:(maxi - trend_length + 1))
      trend_matrix <- cbind(trend_matrix,
                            apply(pop, 1, function(v){coef(lm(y~x, data.frame(x=1:8, y = v[i:(i + trend_length - 1)])))[2]}))
  }
  if (trend_type == "proportional"){
    for(i in 1:(maxi - trend_length + 1)) 
      trend_matrix = cbind(trend_matrix,
                           100*(exp(apply(pop, 1, function(v){coef(lm(I(log(y))~x, data.frame(x=1:8, y = v[i:(i + trend_length - 1)])))[2]}))-1))
  }
  
  return(trend_matrix)
}

# Function to calculate trend
create_trend_table <- function(linear_trend_matrix, year_first, year_last, identifier) {
  bot <- apply(linear_trend_matrix, 2, quantile, prob = .025) %>%
    data.frame() %>%
    rename(trend_b95 = 1) 
  
  top <- apply(linear_trend_matrix, 2, quantile, prob = .975) %>%
    data.frame() %>%
    rename(trend_t95 = 1)
  
  trend <- apply(linear_trend_matrix, 2, mean) %>%
    data.frame() %>%
    rename(trend_est = 1) %>%
    mutate(year = c((year_first + 7): year_last),
           identifier = identifier) %>%
    cbind(bot) %>%
    cbind(top)
  
  return(trend)
}

# Function to do all the things that are required to calculate trend
calculate_trend <- function(data_cube_4trend, trend_type, group_by, group_list, year_first, year_last) {
  # Define variables
  n_years <- year_last - year_first + 1
  pop <- matrix(NA, nrow = 1000, ncol = n_years)
  maxi <- n_years
  trend_length <- 8
  
  trend <- data.frame(trend_est = numeric(), 
                      year = integer(),
                      identifier = character(),
                      trend_b95 = numeric(),
                      trend_t95 = numeric())
  
  # Calculate trend for all polys
  if (group_by == "all") {
    for (i in 1:1000) pop[i,] <- apply(data_cube_4trend[[i]][,], 2, sum) 
    trend_matrix <- generate_trend_matrix(trend_type, maxi, trend_length, pop)
    trend <- create_trend_table(trend_matrix, year_first, year_last, identifier = 'all')
  } 
  else { # Calculate trend by stock and polyid
    for (g in 1:length(group_list)) {
      if (group_by == "stock") {
        for(i in 1:1000) pop[i,] <- apply(data_cube_4trend[[i]][attr(data_cube_4trend[[i]], 'stockid') == g, ], 2, sum)
      }
      if (group_by == "polyid") {
        pop <- matrix(unlist(lapply(data_cube_4trend, function(x){x[group_list[g],]})), nrow = 1000, ncol = n_years)
      }
      
      trend_matrix <- generate_trend_matrix(trend_type, maxi, trend_length, pop)
      trend_temp <- create_trend_table(trend_matrix, year_first, year_last, identifier = group_list[g])
      
      trend <- trend %>%
        rbind(trend_temp)
    }
  }
  
  # Clean up trend table for use in app
  if (group_by == "all") {
    trend <- trend %>%
      select(year, trend_est, trend_b95, trend_t95)
  }
  if (group_by == "stock") {
    trend <- trend %>%
      rename(stockname = identifier) %>%
      select(stockname, year, trend_est, trend_b95, trend_t95)
  }
  if (group_by == "polyid") {
    trend <- trend %>%
      rename(polyid = identifier) %>%
      select(polyid, year, trend_est, trend_b95, trend_t95)
  }

  
  return(trend)
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