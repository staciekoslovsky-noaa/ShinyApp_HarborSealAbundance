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