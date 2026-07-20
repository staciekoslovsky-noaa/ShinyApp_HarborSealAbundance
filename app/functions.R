## Create functions -----------------------------------------

# Function to return RData file with user-specified name
load_rdata <- function(fileName) {
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Function to calculate confidence intervals
calculate_ci <- function(subset, ci_type, group_by_ci, select_ci, num_samples) {
  subset_abund_ci <- subset %>%
    group_by_at(group_by_ci) %>%
    arrange(abund) %>%
    mutate(rank = sequence(n())) %>%
    ungroup() %>%
    filter(rank == num_samples * 0.025 | rank == num_samples * 0.975) %>%
    select(!select_ci) %>%
    unique() %>%
    pivot_wider(names_from = rank, values_from = abund) %>%
    rename(
      !!(paste0(as.name(ci_type), "_b95")) := as.character(num_samples * 0.025),
      !!(paste0(as.name(ci_type), "_t95")) := as.character(num_samples * 0.975)
    )
}


calculate_abundance <- function(
  data_cube,
  subset_type,
  group_by_var,
  most_recent_year = NULL,
  poly_metadata = NULL,
  filter = NULL
) {
  if (subset_type == "most_recent") {
    subset <- data_cube %>%
      filter(year == most_recent_year)

    num_samples <- max(as.numeric(data_cube$cube)) + 1

    polys <- unique(subset$polyid)

    subset <- subset %>%
      group_by_at(group_by_var) %>%
      summarise(abund = sum(abund)) %>%
      ungroup()

    subset_abund_ci <- calculate_ci(
      subset,
      ci_type = "abund",
      group_by_ci = c("polyid", "year"),
      select_ci = c("cube", "year"),
      num_samples
    )

    subset_abund <- subset %>%
      group_by(polyid) %>%
      summarise(abund_est = mean(abund)) %>%
      left_join(subset_abund_ci, by = "polyid")

    subset_summ <- subset_abund # %>%
    # left_join(subset_trend, by = c("polyid"))
  } else {
    if (subset_type == "all") {
      subset <- data_cube
    } else if (subset_type == "stock") {
      subset <- data_cube %>%
        filter(stockname == filter)
    } else if (subset_type == "poly_in_list") {
      subset <- data_cube %>%
        filter(polyid %in% filter)
    }

    num_samples <- max(as.numeric(data_cube$cube)) + 1

    polys <- unique(subset$polyid)

    subset_effort <- poly_metadata %>%
      filter(polyid %in% polys)

    subset_abund_effort <- subset %>%
      inner_join(subset_effort, by = c("polyid", "year")) %>%
      group_by_at(group_by_var) %>%
      summarise(abund = sum(abund)) %>%
      ungroup() %>%
      group_by(year) %>%
      summarise(abund_surveyed = mean(abund))

    subset <- subset %>%
      group_by_at(group_by_var) %>%
      summarise(abund = sum(abund)) %>%
      ungroup()

    subset_abund_ci <- calculate_ci(
      subset,
      ci_type = "abund",
      group_by_ci = c("year"),
      select_ci = c("cube"),
      num_samples
    )

    subset_abund <- subset %>%
      group_by(year) %>%
      summarise(abund_est = mean(abund)) %>%
      left_join(subset_abund_ci, by = "year")

    subset_summ <- subset_abund %>%
      # left_join(subset_trend, by = "year") %>%
      left_join(subset_abund_effort, by = c("year")) %>%
      mutate(
        abund_surveyed = ifelse(is.na(abund_surveyed), 0, abund_surveyed)
      ) %>%
      mutate(effort = round(abund_surveyed * 100 / abund_est, 2))
  }
  return(subset_summ)
}

generate_trend_matrix <- function(trend_type, maxi, trend_length, pop) {
  num_windows <- maxi - trend_length + 1
  num_rows <- nrow(pop)

  trend_matrix <- matrix(NA_real_, nrow = num_rows, ncol = num_windows)

  x <- 1:trend_length

  if (trend_type == "linear") {
    x_mean <- mean(x)
    x_dev <- x - x_mean
    denom <- sum(x_dev^2)

    for (i in 1:num_windows) {
      window_mat <- pop[, i:(i + trend_length - 1), drop = FALSE]

      trend_matrix[, i] <- (window_mat %*% x_dev) / denom
    }
  } else if (trend_type == "proportional") {
    has_fastglm <- requireNamespace("fastglm", quietly = TRUE)

    X_design <- cbind(1, x)

    for (i in 1:num_windows) {
      window_mat <- pop[, i:(i + trend_length - 1), drop = FALSE]

      trend_matrix[, i] <- apply(window_mat, 1, function(v) {
        if (has_fastglm) {
          fit <- fastglm::fastglm(X_design, v, family = poisson())
          b1 <- fit$coefficients[2]
        } else {
          fit <- glm.fit(X_design, v, family = poisson())
          b1 <- fit$coefficients[2]
        }
        return(100 * (exp(b1) - 1))
      })
    }
  }

  return(trend_matrix)
}

# Function to calculate trend
create_trend_table <- function(
  trend_matrix,
  year_first,
  year_last,
  identifier
) {
  bot <- apply(trend_matrix, 2, quantile, prob = .025) %>%
    data.frame() %>%
    rename(trend_b95 = 1)

  top <- apply(trend_matrix, 2, quantile, prob = .975) %>%
    data.frame() %>%
    rename(trend_t95 = 1)

  trend <- apply(trend_matrix, 2, mean) %>%
    data.frame() %>%
    rename(trend_est = 1) %>%
    mutate(year = c((year_first + 7):year_last), identifier = identifier) %>%
    cbind(bot) %>%
    cbind(top)

  return(trend)
}

calculate_trend <- function(
  data_cube_4trend,
  trend_type,
  group_by,
  group_list,
  year_first,
  year_last
) {
  n_years <- year_last - year_first + 1
  num_samples <- length(data_cube_4trend)
  pop <- matrix(NA, nrow = num_samples, ncol = n_years)
  maxi <- n_years
  trend_length <- 8

  trend <- data.frame(
    trend_est = numeric(),
    year = integer(),
    identifier = character(),
    trend_b95 = numeric(),
    trend_t95 = numeric()
  )

  if (group_by == "all") {
    for (i in 1:num_samples) {
      pop[i, ] <- apply(data_cube_4trend[[i]][,], 2, sum)
    }
    trend_matrix <- generate_trend_matrix(trend_type, maxi, trend_length, pop)
    trend <- create_trend_table(
      trend_matrix,
      year_first,
      year_last,
      identifier = 'all'
    )

    trend <- trend %>%
      select(year, trend_est, trend_b95, trend_t95)
  } else {
    for (g in 1:length(group_list)) {
      if (group_by == "stock") {
        for (i in 1:num_samples) {
          pop[i, ] <- apply(
            data_cube_4trend[[i]][
              attr(data_cube_4trend[[i]], 'stockid') == g,
            ],
            2,
            sum
          )
        }
      }
      if (group_by == "polyid") {
        print(g)
        pop <- matrix(
          unlist(lapply(data_cube_4trend, function(x) {
            x[group_list[g], ]
          })),
          nrow = num_samples,
          ncol = n_years
        )
      }
    }
    trend_matrix <- generate_trend_matrix(trend_type, maxi, trend_length, pop)
    trend_temp <- create_trend_table(
      trend_matrix,
      year_first,
      year_last,
      identifier = group_list[g]
    )

    trend <- trend %>%
      rbind(trend_temp)

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
  }
  return(trend)
}

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
