find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (dir.exists(file.path(current, "app")) &&
        dir.exists(file.path(current, "DataPrep"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Unable to locate the HarborSealAbundance repository root.", call. = FALSE)
    }
    current <- parent
  }
}

harbor_seal_paths <- function(repo_root = find_repo_root()) {
  env_or_default <- function(name, default) {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value) || !nzchar(value)) {
      default
    } else {
      value
    }
  }

  app_data_dir <- env_or_default(
    "HARBOR_SEAL_APP_DATA_DIR",
    file.path(repo_root, "app", "Data")
  )
  app_artifacts_dir <- env_or_default(
    "HARBOR_SEAL_APP_ARTIFACTS_DIR",
    file.path(repo_root, "local_data", "app_artifacts")
  )
  private_input_dir <- env_or_default(
    "HARBOR_SEAL_PRIVATE_INPUT_DIR",
    file.path(repo_root, "local_data", "private")
  )
  data_cube_input <- env_or_default(
    "HARBOR_SEAL_DATA_CUBE_PATH",
    file.path(private_input_dir, "akpv_datacube.rda")
  )

  list(
    repo_root = repo_root,
    app_dir = file.path(repo_root, "app"),
    app_data_dir = app_data_dir,
    app_artifacts_dir = app_artifacts_dir,
    private_input_dir = private_input_dir,
    data_cube_input = data_cube_input,
    stock_polygons = file.path(app_data_dir, "survey_stocks.geojson"),
    haulout = file.path(app_data_dir, "survey_haulout.geojson"),
    poly_metadata = file.path(app_data_dir, "poly_metadata.rda"),
    last_surveyed = file.path(app_data_dir, "last_surveyed.rda"),
    data_cube = file.path(app_artifacts_dir, "data_cube.rda"),
    survey_polygons = file.path(app_artifacts_dir, "survey_polygons.geojson"),
    trend_linear_all = file.path(app_artifacts_dir, "trend_linear_all.rda"),
    trend_linear_stock = file.path(app_artifacts_dir, "trend_linear_stock.rda"),
    trend_linear_polyid = file.path(app_artifacts_dir, "trend_linear_polyid.rda"),
    trend_prop_all = file.path(app_artifacts_dir, "trend_prop_all.rda"),
    trend_prop_stock = file.path(app_artifacts_dir, "trend_prop_stock.rda"),
    trend_prop_polyid = file.path(app_artifacts_dir, "trend_prop_polyid.rda"),
    trend_p_positive = file.path(app_artifacts_dir, "trend_p_positive.rda")
  )
}

ensure_directory <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

assert_file_exists <- function(path, label = basename(path)) {
  if (!file.exists(path)) {
    stop(
      paste0(label, " was not found at ", path, "."),
      call. = FALSE
    )
  }

  invisible(path)
}

assert_app_artifacts_exist <- function(paths) {
  required_files <- unlist(paths[c(
    "stock_polygons",
    "haulout",
    "poly_metadata",
    "last_surveyed",
    "data_cube",
    "survey_polygons",
    "trend_linear_all",
    "trend_linear_stock",
    "trend_linear_polyid",
    "trend_prop_all",
    "trend_prop_stock",
    "trend_prop_polyid",
    "trend_p_positive"
  )])

  missing <- required_files[!file.exists(required_files)]
  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing app artifacts. Run DataPrep/HarborSealAbundance_PrepData4App.R ",
        "or set HARBOR_SEAL_APP_ARTIFACTS_DIR / HARBOR_SEAL_APP_DATA_DIR ",
        "to existing artifact directories.\n",
        paste(missing, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  invisible(required_files)
}

load_rdata <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}

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
      ungroup() %>%
      left_join(subset_abund_ci, by = "polyid")

    subset_summ <- subset_abund
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
      ungroup() %>%
      left_join(subset_abund_ci, by = "year")

    subset_summ <- subset_abund %>%
      left_join(subset_abund_effort, by = c("year")) %>%
      mutate(
        abund_surveyed = ifelse(is.na(abund_surveyed), 0, abund_surveyed),
        effort = round(abund_surveyed * 100 / abund_est, 2)
      )
  }

  return(subset_summ)
}

extract_population_series <- function(sample_cube, row_ids = NULL) {
  if (is.null(row_ids)) {
    return(colSums(sample_cube))
  }

  sample_subset <- sample_cube[row_ids, , drop = FALSE]
  if (nrow(sample_subset) == 1) {
    as.numeric(sample_subset[1, ])
  } else {
    colSums(sample_subset)
  }
}

build_population_matrix <- function(data_cube_4trend, row_ids = NULL) {
  population_list <- lapply(
    data_cube_4trend,
    extract_population_series,
    row_ids = row_ids
  )

  do.call(rbind, lapply(population_list, as.numeric))
}

generate_trend_matrix_fast <- function(trend_type, maxi, trend_length, pop) {
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
      trend_matrix[, i] <- as.numeric((window_mat %*% x_dev) / denom)
    }
  } else if (trend_type == "proportional") {
    X_design <- cbind(1, x)

    for (i in 1:num_windows) {
      window_mat <- pop[, i:(i + trend_length - 1), drop = FALSE]
      trend_matrix[, i] <- apply(window_mat, 1, function(v) {
        fit <- glm.fit(X_design, v, family = poisson())
        100 * (exp(fit$coefficients[2]) - 1)
      })
    }
  } else {
    stop("trend_type must be 'linear' or 'proportional'.", call. = FALSE)
  }

  trend_matrix
}

generate_trend_matrix <- function(trend_type, maxi, trend_length, pop) {
  generate_trend_matrix_fast(trend_type, maxi, trend_length, pop)
}

create_trend_table <- function(
  trend_matrix,
  year_first,
  year_last,
  identifier,
  trend_length = 8
) {
  bot <- apply(trend_matrix, 2, quantile, prob = 0.025) %>%
    data.frame() %>%
    rename(trend_b95 = 1)

  top <- apply(trend_matrix, 2, quantile, prob = 0.975) %>%
    data.frame() %>%
    rename(trend_t95 = 1)

  trend <- apply(trend_matrix, 2, mean) %>%
    data.frame() %>%
    rename(trend_est = 1) %>%
    mutate(
      year = seq.int(year_first + trend_length - 1, year_last),
      identifier = identifier
    ) %>%
    cbind(bot) %>%
    cbind(top)

  trend
}

create_p_positive_table <- function(trend_matrix, identifier) {
  data.frame(
    identifier = identifier,
    p_positive = mean(trend_matrix[, ncol(trend_matrix)] >= 0)
  )
}

calculate_trend <- function(
  data_cube_4trend,
  trend_type,
  group_by,
  group_list,
  year_first,
  year_last,
  group_rows = NULL,
  trend_length = 8
) {
  n_years <- year_last - year_first + 1

  if (group_by == "all") {
    pop <- build_population_matrix(data_cube_4trend)
    trend_matrix <- generate_trend_matrix(trend_type, n_years, trend_length, pop)
    trend <- create_trend_table(
      trend_matrix,
      year_first,
      year_last,
      identifier = "all",
      trend_length = trend_length
    ) %>%
      select(year, trend_est, trend_b95, trend_t95)

    return(trend)
  }

  trend_list <- lapply(seq_along(group_list), function(g) {
    if (group_by == "stock") {
      if (!is.null(group_rows)) {
        row_ids <- group_rows[[group_list[g]]]
        pop <- build_population_matrix(data_cube_4trend, row_ids = row_ids)
      } else {
        pop <- do.call(rbind, lapply(data_cube_4trend, function(sample_cube) {
          extract_population_series(
            sample_cube,
            row_ids = which(attr(sample_cube, "stockid") == g)
          )
        }))
      }
    } else if (group_by == "polyid") {
      pop <- build_population_matrix(data_cube_4trend, row_ids = group_list[g])
    } else {
      stop("group_by must be 'all', 'stock', or 'polyid'.", call. = FALSE)
    }

    trend_matrix <- generate_trend_matrix(trend_type, n_years, trend_length, pop)
    create_trend_table(
      trend_matrix,
      year_first,
      year_last,
      identifier = group_list[g],
      trend_length = trend_length
    )
  })

  trend <- dplyr::bind_rows(trend_list)

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

  trend
}

calculate_trend_products <- function(
  data_cube_4trend,
  stock_groups,
  year_first,
  year_last,
  trend_length = 8
) {
  polyids <- rownames(data_cube_4trend[[1]])

  all_pop <- build_population_matrix(data_cube_4trend)
  linear_all_matrix <- generate_trend_matrix(
    "linear",
    ncol(all_pop),
    trend_length,
    all_pop
  )
  prop_all_matrix <- generate_trend_matrix(
    "proportional",
    ncol(all_pop),
    trend_length,
    all_pop
  )

  stock_results <- lapply(names(stock_groups), function(stockname) {
    pop <- build_population_matrix(data_cube_4trend, row_ids = stock_groups[[stockname]])
    linear_matrix <- generate_trend_matrix("linear", ncol(pop), trend_length, pop)
    prop_matrix <- generate_trend_matrix("proportional", ncol(pop), trend_length, pop)

    list(
      linear = create_trend_table(
        linear_matrix,
        year_first,
        year_last,
        identifier = stockname,
        trend_length = trend_length
      ),
      proportional = create_trend_table(
        prop_matrix,
        year_first,
        year_last,
        identifier = stockname,
        trend_length = trend_length
      )
    )
  })

  poly_results <- lapply(polyids, function(polyid) {
    pop <- build_population_matrix(data_cube_4trend, row_ids = polyid)
    linear_matrix <- generate_trend_matrix("linear", ncol(pop), trend_length, pop)
    prop_matrix <- generate_trend_matrix("proportional", ncol(pop), trend_length, pop)

    list(
      linear = create_trend_table(
        linear_matrix,
        year_first,
        year_last,
        identifier = polyid,
        trend_length = trend_length
      ),
      proportional = create_trend_table(
        prop_matrix,
        year_first,
        year_last,
        identifier = polyid,
        trend_length = trend_length
      ),
      p_positive = create_p_positive_table(linear_matrix, polyid)
    )
  })

  list(
    trend_linear_all = create_trend_table(
      linear_all_matrix,
      year_first,
      year_last,
      identifier = "all",
      trend_length = trend_length
    ) %>% select(year, trend_est, trend_b95, trend_t95),
    trend_linear_stock = dplyr::bind_rows(lapply(stock_results, `[[`, "linear")) %>%
      rename(stockname = identifier) %>%
      select(stockname, year, trend_est, trend_b95, trend_t95),
    trend_linear_polyid = dplyr::bind_rows(lapply(poly_results, `[[`, "linear")) %>%
      rename(polyid = identifier) %>%
      select(polyid, year, trend_est, trend_b95, trend_t95),
    trend_prop_all = create_trend_table(
      prop_all_matrix,
      year_first,
      year_last,
      identifier = "all",
      trend_length = trend_length
    ) %>% select(year, trend_est, trend_b95, trend_t95),
    trend_prop_stock = dplyr::bind_rows(lapply(stock_results, `[[`, "proportional")) %>%
      rename(stockname = identifier) %>%
      select(stockname, year, trend_est, trend_b95, trend_t95),
    trend_prop_polyid = dplyr::bind_rows(lapply(poly_results, `[[`, "proportional")) %>%
      rename(polyid = identifier) %>%
      select(polyid, year, trend_est, trend_b95, trend_t95),
    trend_p_positive = dplyr::bind_rows(lapply(poly_results, `[[`, "p_positive")) %>%
      rename(polyid = identifier) %>%
      select(polyid, p_positive)
  )
}
