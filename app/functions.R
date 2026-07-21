## Create functions -----------------------------------------

# Function to calculate abundance for a given group (all, stock, polyid, or poly_list)
calculate_abundance <- function(
  data_cube,
  group_by,
  group_list = NULL,
  poly_metadata = NULL
) {
  summarize_group_abundance <- function(
    data_subset,
    group_id = NULL,
    id_col = NULL
  ) {
    abundance_input <- data_subset %>%
      group_by(cube, year) %>%
      summarise(abund = sum(abund), .groups = "drop")

    abundance_summary <- abundance_input %>%
      group_by(year) %>%
      summarise(
        abund_est = mean(abund, na.rm = TRUE),
        abund_b95 = quantile(abund, 0.025, na.rm = TRUE),
        abund_t95 = quantile(abund, 0.975, na.rm = TRUE),
        .groups = "drop"
      )

    effort_summary <- data_subset %>%
      inner_join(poly_metadata, by = c("polyid", "year")) %>%
      group_by(cube, year) %>%
      summarise(abund = sum(abund), .groups = "drop") %>%
      group_by(year) %>%
      summarise(
        abund_surveyed = mean(abund, na.rm = TRUE),
        .groups = "drop"
      )

    out <- abundance_summary %>%
      left_join(effort_summary, by = "year") %>%
      mutate(
        abund_surveyed = ifelse(is.na(abund_surveyed), 0, abund_surveyed),
        effort = ifelse(
          abund_est == 0,
          0,
          round(abund_surveyed * 100 / abund_est, 2)
        )
      )

    if (!is.null(id_col)) {
      out[[id_col]] <- group_id
      out <- out %>% select(all_of(id_col), everything())
    }

    row.names(out) <- NULL
    out
  }

  if (group_by == "all") {
    return(summarize_group_abundance(data_cube))
  }

  if (group_by == "poly_list") {
    data_subset <- data_cube %>%
      filter(polyid %in% group_list)

    return(summarize_group_abundance(data_subset))
  }

  result_list <- vector("list", length(group_list))

  if (group_by == "stock") {
    for (g in seq_along(group_list)) {
      stock_name <- group_list[g]

      data_subset <- data_cube %>%
        filter(stockname == stock_name)

      result_list[[g]] <- summarize_group_abundance(
        data_subset = data_subset,
        group_id = stock_name,
        id_col = "stockname"
      )
    }

    return(bind_rows(result_list))
  }

  if (group_by == "polyid") {
    for (g in seq_along(group_list)) {
      poly_id <- group_list[g]

      data_subset <- data_cube %>%
        filter(polyid == poly_id)

      result_list[[g]] <- summarize_group_abundance(
        data_subset = data_subset,
        group_id = poly_id,
        id_col = "polyid"
      )
    }

    return(bind_rows(result_list))
  }

  stop("group_by must be one of 'all', 'stock', 'polyid', or 'poly_list'")
}

# Function to generate trend matrix for a given trend type and population data
generate_trend_matrix <- function(
  trend_type,
  maxi,
  trend_length,
  pop,
  log_offset = 1
) {
  num_windows <- maxi - trend_length + 1
  num_rows <- nrow(pop)

  trend_matrix <- matrix(NA_real_, nrow = num_rows, ncol = num_windows)

  x <- seq_len(trend_length)
  x_dev <- x - mean(x)
  denom <- sum(x_dev^2)

  if (trend_type == "linear") {
    working_pop <- pop
  } else if (trend_type == "proportional") {
    working_pop <- log(pop + log_offset)
  } else {
    stop("trend_type must be 'linear' or 'proportional'")
  }

  for (i in seq_len(num_windows)) {
    window_mat <- working_pop[, i:(i + trend_length - 1), drop = FALSE]
    slope <- (window_mat %*% x_dev) / denom

    if (trend_type == "proportional") {
      trend_matrix[, i] <- 100 * (exp(slope) - 1)
    } else {
      trend_matrix[, i] <- slope
    }
  }

  trend_matrix
}

# Function to calculate trend
create_trend_table <- function(
  trend_matrix,
  year_first,
  year_last,
  identifier
) {
  years <- (year_first + 7):year_last

  data.frame(
    year = years,
    identifier = identifier,
    trend_est = colMeans(trend_matrix, na.rm = TRUE),
    trend_b95 = apply(trend_matrix, 2, quantile, probs = 0.025, na.rm = TRUE),
    trend_t95 = apply(trend_matrix, 2, quantile, probs = 0.975, na.rm = TRUE),
    row.names = NULL
  )
}

# Function to calculate trend for a given group (all, stock, polyid, or poly_list)
calculate_trend <- function(
  data_cube,
  trend_type,
  group_by,
  group_list = NULL,
  year_first,
  year_last,
  log_offset = 1
) {
  trend_length <- 8
  n_years <- year_last - year_first + 1
  maxi <- n_years

  summarize_group_trend <- function(
    data_subset,
    group_id = NULL,
    id_col = NULL
  ) {
    trend_input <- data_subset %>%
      filter(year >= year_first, year <= year_last) %>%
      group_by(cube, year) %>%
      summarise(abund = sum(abund), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = abund,
        values_fill = 0
      ) %>%
      arrange(cube)

    year_cols <- as.character(seq(year_first, year_last))
    pop <- as.matrix(trend_input[, year_cols, drop = FALSE])

    trend_matrix <- generate_trend_matrix(
      trend_type = trend_type,
      maxi = maxi,
      trend_length = trend_length,
      pop = pop,
      log_offset = log_offset
    )

    out <- create_trend_table(
      trend_matrix = trend_matrix,
      year_first = year_first,
      year_last = year_last,
      identifier = if (is.null(group_id)) "all" else group_id
    )

    if (!is.null(id_col)) {
      names(out)[names(out) == "identifier"] <- id_col
    } else {
      out <- out[, c("year", "trend_est", "trend_b95", "trend_t95")]
    }

    row.names(out) <- NULL
    out
  }

  if (group_by == "all") {
    return(summarize_group_trend(data_cube))
  }

  if (group_by == "poly_list") {
    data_subset <- data_cube %>%
      filter(polyid %in% group_list)

    return(summarize_group_trend(data_subset))
  }

  result_list <- vector("list", length(group_list))

  if (group_by == "stock") {
    for (g in seq_along(group_list)) {
      stock_name <- group_list[g]

      data_subset <- data_cube %>%
        filter(stockname == stock_name)

      result_list[[g]] <- summarize_group_trend(
        data_subset = data_subset,
        group_id = stock_name,
        id_col = "stockname"
      )
    }

    return(bind_rows(result_list))
  }

  if (group_by == "polyid") {
    for (g in seq_along(group_list)) {
      poly_id <- group_list[g]

      data_subset <- data_cube %>%
        filter(polyid == poly_id)

      result_list[[g]] <- summarize_group_trend(
        data_subset = data_subset,
        group_id = poly_id,
        id_col = "polyid"
      )
    }

    return(bind_rows(result_list))
  }

  stop("group_by must be one of 'all', 'stock', 'polyid', or 'poly_list'")
}
