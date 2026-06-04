server <- function(input, output, session) {
  # Map starts off at center, map.view represents current view
  map.view <- shiny::reactiveValues(values = c(mean_x, mean_y))

  ## Generate Leaflet map and polygons ##
  output$map1 <- leaflet::renderLeaflet({
    abund_bins <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 12000)

    #Create palette to color polygons
    pal <- colorBin(
      palette = "inferno",
      reverse = TRUE,
      domain = na.omit(survey_polygons$abund_est),
      bins = abund_bins,
      pretty = FALSE,
      na.color = "#00000000"
    )

    factpal <- colorFactor(
      palette = viridis_pal(option = "H")(nrow(stock_polygons)),
      domain = stock_polygons$stockname
    )

    map %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(
            fillOpacity = 0,
            color = '#f6d746',
            weight = 3
          ),
          showArea = TRUE,
          metric = FALSE
        ),
        rectangleOptions = FALSE,
        #circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(
          edit = FALSE,
          selectedPathOptions = FALSE,
          remove = TRUE
        ),
        singleFeature = TRUE
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(maxWidth = 250)
      ) %>%
      # addPolygons(data = stock_polygons) %>%
      addPolygons(
        data = stock_polygons,
        fillOpacity = 0.2,
        opacity = 0.4,
        fillColor = ~ factpal(stockname),
        color = "gray",
        weight = 2,
        label = stock_polygons$stockname,
        labelOptions = labelOptions(
          sticky = FALSE,
          noHide = FALSE,
          textOnly = TRUE,
          direction = "center"
        )
      ) %>%
      addPolygons(
        data = survey_polygons,
        layerId = ~polyid,
        group = "stockname",
        popup = ~ htmltools::htmlEscape(popup_text),
        weight = 1,
        fillColor = ~ pal(survey_polygons$abund_est),
        color = ~ pal(survey_polygons$abund_est),
        opacity = 0.7,
        fillOpacity = get_opacity(
          as.vector(survey_polygons$abund_est),
          abund_bins
        ),
        label = survey_polygons$polyid,
        labelOptions = labelOptions(
          sticky = TRUE,
          noHide = FALSE,
          textOnly = TRUE,
          direction = "center"
        )
      ) %>%
      addCircles(
        data = haulout,
        fillColor = 'darkslategray',
        color = 'darkslategray',
        weight = 2,
        opacity = 0.8,
        group = "haulout"
      ) %>%
      addLegend(
        "bottomleft",
        pal = pal,
        values = abund_bins,
        title = "Abundance:"
      ) %>%
      addControl(
        HTML(paste(most_recent_year, "Estimated Abundance")),
        position = "topright"
      ) %>%

      groupOptions("haulout", zoomLevels = 8:20) %>%

      #Set view to the middle of all the polygons' coordinates
      setView(lng = map.view$values[1], lat = map.view$values[2], zoom = 4)
  })

  # Reactive dataset and title for abundance plot
  plotted.abundance <- reactiveValues(values = abundance)
  title.abundance <- reactiveVal(value = "in All Stocks")

  # Reactive dataset and title for trend plot
  plotted.trend <- reactiveValues(
    values = trend %>% filter(trend_type == "linear_all")
  )
  title.trend <- reactiveVal(value = "in All Stocks")
  title.trend.type <- reactiveVal(value = "Linear")
  yaxis.trend <- reactiveVal(value = "Trend (# of Seals)")

  # Reactive value that zooms map to centroid of selected features
  zoom.to.stock <- reactiveVal(value = FALSE)

  ## Update reactive dataset when button is pressed ##
  observeEvent(input$update, {
    title.trend.type(input$trend.type)
    yaxis.trend(ifelse(
      input$trend.type == "Linear",
      "Trend (# of Seals)",
      "Trend (% of Population)"
    ))

    # Process data if "stock" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (input$filter == "Stock") {
      # Update the dataset to be filtered by the selected stock
      if (input$stock.select != "All") {
        stock_filter <- input$stock.select

        plotted.abundance$values <- calculate_abundance(
          data_cube = data_cube,
          group_by_var = c('cube', 'year'),
          subset_type = 'stock',
          poly_metadata = poly_metadata,
          filter = stock_filter
        )
        title.abundance(paste("in the", input$stock.select, "Stock"))

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_stock"
            } else {
              trend_type == "prop_stock"
            }
          ) %>%
          filter(identifier == stock_filter)
        title.trend(paste("in the", input$stock.select, "Stock"))

        currently_plotted_ids <- survey_polygons %>%
          filter(stockname == stock_filter)
      } else {
        plotted.abundance$values <- abundance
        title.abundance("in All Stocks")

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_all"
            } else {
              trend_type == "prop_all"
            }
          )
        title.trend("in All Stocks")
      }
      zoom.to.stock(TRUE)
    } else if (input$filter == "Custom Polygon") {
      # Process data if "polygon" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If there is no drawn shape, revert to default data (otherwise there is a fatal error and the R session is aborted)
      if (
        is.null(input$map1_draw_new_feature) ||
          (input$map1_draw_new_feature$properties$feature_type == "circle")
      ) {
        plotted.abundance$values <- abundance
        title.abundance("in All Stocks")

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_all"
            } else {
              trend_type == "prop_all"
            }
          )
        title.trend("in All Stocks")
      } else {
        # Select based on drawn polygon
        drawn <- input$map1_draw_new_feature
        polygon_coordinates <- do.call(
          rbind,
          lapply(drawn$geometry$coordinates[[1]], function(x) {
            c(x[[1]][1], x[[2]][1])
          })
        )
        drawn_polygon <- data.frame(
          lat = polygon_coordinates[, 2],
          long = polygon_coordinates[, 1]
        ) %>%
          st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        found_in_bounds <- st_join(
          sf::st_set_crs(drawn_polygon, 4326),
          sf::st_set_crs(survey_polygons, 4326)
        )

        poly_filter <- found_in_bounds$polyid

        # Subset data by polyids within polygon
        plotted.abundance$values <- calculate_abundance(
          data_cube = data_cube,
          group_by_var = c('cube', 'year'),
          subset_type = 'poly_in_list',
          poly_metadata = poly_metadata,
          filter = poly_filter
        ) %>%
          left_join(trend_linear_stock, by = "year")
        title.abundance("in the Selected Survey Units")

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_all"
            } else {
              trend_type == "prop_all"
            }
          )
        title.trend(
          "in All Stocks (trend cannot be subset to the selected survey units)"
        )

        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% poly_filter)
      }
      zoom.to.stock(TRUE)
    } else if (input$filter == "Custom Circle") {
      # Process data if "circle" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If there is no drawn circle, revert to default data (otherwise there is a fatal error and the r session is aborted)
      if (
        is.null(input$map1_draw_new_feature) ||
          (input$map1_draw_new_feature$properties$feature_type != "circle")
      ) {
        plotted.abundance$values <- abundance
        title.abundance("in All Stocks")

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_all"
            } else {
              trend_type == "prop_all"
            }
          )
        title.trend("in All Stocks")
      } else {
        # Select based on drawn circle
        drawn <- input$map1_draw_new_feature

        drawn_circle <- data.frame(
          lat = drawn$geometry$coordinates[[2]],
          long = drawn$geometry$coordinates[[1]]
        ) %>%
          sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
          sf::st_transform(crs = 32606) %>%
          sf::st_buffer(dist = drawn$properties$radius) %>%
          sf::st_transform(4326)
        drawn_circle$geometry <- (sf::st_geometry(drawn_circle) + c(360, 90)) %%
          c(360) -
          c(0, 90)

        found_in_bounds <- st_join(
          sf::st_set_crs(drawn_circle, 4326),
          sf::st_set_crs(survey_polygons, 4326)
        )

        circle_filter <- found_in_bounds$polyid

        # Calculate custom abundance estimates for app
        plotted.abundance$values <- calculate_abundance(
          data_cube = data_cube,
          group_by_var = c('cube', 'year'),
          subset_type = 'poly_in_list',
          poly_metadata = poly_metadata,
          filter = circle_filter
        )
        title.abundance("Within the Given Radius")

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_all"
            } else {
              trend_type == "prop_all"
            }
          )
        title.trend(
          "in All Stocks  (trend cannot be subset to the selected survey units)"
        )

        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% circle_filter)
      }
      zoom.to.stock(TRUE)
    } else if (input$filter == "Glacial Sites") {
      # Process data if "glacial" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Select glacial sites
      poly_filter <- survey_polygons %>%
        filter(!is.na(glacier_name)) %>%
        select(polyid) %>%
        st_drop_geometry()
      poly_filter <- poly_filter$polyid

      # Subset data by polyids within polygon
      plotted.abundance$values <- calculate_abundance(
        data_cube = data_cube,
        group_by_var = c('cube', 'year'),
        subset_type = 'poly_in_list',
        poly_metadata = poly_metadata,
        filter = poly_filter
      ) %>%
        left_join(trend_linear_stock, by = "year")
      title.abundance("in Survey Units in Glacial Habitat")

      plotted.trend$values <- trend %>%
        filter(
          if (input$trend.type == "Linear") {
            trend_type == "linear_all"
          } else {
            trend_type == "prop_all"
          }
        )
      title.trend(
        "in All Stocks (trend cannot be subset to the selected survey units)"
      )

      currently_plotted_ids <- survey_polygons %>%
        filter(polyid %in% poly_filter)

      zoom.to.stock(TRUE)
    } else if (input$filter == "Survey Unit") {
      # Process data if "survey unit" selected ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (is.null(input$map1_shape_click$id)) {
        plotted.abundance$values <- abundance
        title.abundance("in All Stocks")

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_all"
            } else {
              trend_type == "prop_all"
            }
          )
        title.trend("in All Stocks")
      } else {
        singlePoly_filter <- input$map1_shape_click$id

        # Calculate custom abundance estimates for app
        plotted.abundance$values <- calculate_abundance(
          data_cube = data_cube,
          group_by_var = c('cube', 'year'),
          subset_type = 'poly_in_list',
          poly_metadata = poly_metadata,
          filter = singlePoly_filter
        )
        title.abundance(paste("in Survey Unit:", singlePoly_filter))

        plotted.trend$values <- trend %>%
          filter(
            if (input$trend.type == "Linear") {
              trend_type == "linear_polyid"
            } else {
              trend_type == "prop_polyid"
            }
          ) %>%
          filter(identifier == singlePoly_filter)
        title.trend(paste("in Survey Unit:", singlePoly_filter))

        currently_plotted_ids <- survey_polygons %>%
          filter(polyid %in% singlePoly_filter)
      }
      zoom.to.stock(TRUE)
    }

    # Map survey_polygons where the ids match the ones in plotted.abundance$values
    leafletProxy("map1") %>%
      clearGroup(group = "lines")

    if (exists("currently_plotted_ids") == TRUE) {
      leafletProxy("map1") %>%
        addPolylines(
          data = currently_plotted_ids,
          fillOpacity = 0,
          opacity = 0.4,
          color = "#59FAFC",
          weight = 3,
          #layerId = ~polyid
          group = "lines"
        )
    }

    if (zoom.to.stock()) {
      if (exists("currently_plotted_ids") == TRUE) {
        View(currently_plotted_ids)

        subset_x_centroid <- mean(currently_plotted_ids$centroid.x)
        subset_y_centroid <- mean(currently_plotted_ids$centroid.y)

        leafletProxy("map1") %>%
          setView(subset_x_centroid, subset_y_centroid, zoom = 6)
      } else {
        leafletProxy("map1") %>%
          setView(lng = map.view$values[1], lat = map.view$values[2], zoom = 4)
      }
    }
  })

  observeEvent(input$default, {
    plotted.abundance$values <- abundance
    title.abundance("in All Stocks")

    plotted.trend$values <- trend %>%
      filter(
        if (input$trend.type == "Linear") {
          trend_type == "linear_all"
        } else {
          trend_type == "prop_all"
        }
      )
    title.trend("in All Stocks")
    title.trend.type <- input$trend.type
    yaxis.trend <- ifelse(
      input$trend.type == "Linear",
      "Trend (# of Seals)",
      "Trend (% of Population)"
    )

    leafletProxy("map1") %>%
      setView(mean_x, mean_y, zoom = 4) %>%
      clearGroup(group = "lines")
  })

  ## Plots -----------------------------------------
  # A caption for the chart is available if we'd be interested in including that
  # Data labels in plotOptions -> line -> dataLabels

  # Plot for abundance
  output$plot1 <- leaflet::renderHighchart({
    abund_subset <- plotted.abundance$values

    highchart() %>%
      hc_xAxis(
        title = list(
          text = "Year",
          style = list(color = "#FFFFFF", fontSize = "16px")
        ),
        labels = list(style = list(color = "#FFFFFF"))
      ) %>%
      hc_yAxis_multiples(
        list(
          title = list(
            text = "# of Harbor Seals",
            style = list(color = "#FFFFFF", fontSize = "16px")
          ),
          labels = list(format = '{value}', style = list(color = "#FFFFFF")),
          min = min(abund_subset$abund_b95),
          max = max(abund_subset$abund_t95),
          showFirstLabel = TRUE,
          showLastLabel = TRUE,
          opposite = FALSE
        ),
        list(
          title = list(
            text = "% of Harbor Seals Surveyed",
            style = list(color = "#FFFFFF", fontSize = "16px")
          ),
          min = 0,
          max = 100,
          labels = list(format = "{value}%", style = list(color = "#FFFFFF")),
          showLastLabel = TRUE,
          opposite = TRUE
        )
      ) %>%
      # hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(
        data = abund_subset,
        type = "column",
        hcaes(x = year, y = effort),
        color = "#855278",
        name = "% of Harbor Seals Surveyed",
        opacity = 0.5,
        yAxis = 1
      ) %>%
      hc_add_series(
        data = abund_subset,
        type = "arearange",
        hcaes(x = year, low = abund_b95, high = abund_t95),
        color = "#E6C6B5",
        opacity = 0.4,
        name = "95th Percentile Confidence Interval"
      ) %>%
      hc_add_series(
        data = abund_subset,
        type = "line",
        lineWidth = 5,
        hcaes(x = year, y = abund_est),
        color = "#E55C30", #"#ED6925",
        name = "Estimated Abundance"
      ) %>%
      hc_title(
        text = paste(
          "Estimated Harbor Seal Abundance by Year",
          title.abundance()
        ),
        style = list(color = "#FFFFFF")
      ) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF"))
  })

  #Plot for trend
  output$plot2 <- leaflet::renderHighchart({
    trend_subset <- plotted.trend$values
    print(yaxis.trend())

    highchart() %>%
      hc_xAxis(
        title = list(
          text = "Year",
          style = list(color = "#FFFFFF", fontSize = "16px")
        ),
        labels = list(style = list(color = "#FFFFFF"))
      ) %>%
      hc_yAxis(
        title = list(
          text = yaxis.trend(),
          style = list(color = "#FFFFFF", fontSize = "16px")
        ),
        labels = list(format = '{value}', style = list(color = "#FFFFFF")),
        min = min(trend_subset$trend_b95),
        max = max(trend_subset$trend_t95),
        showFirstLabel = TRUE,
        showLastLabel = TRUE,
        opposite = FALSE
      ) %>%
      # hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_add_series(
        data = trend_subset,
        type = "arearange",
        hcaes(x = year, low = trend_b95, high = trend_t95),
        color = "#E6C6B5",
        opacity = 0.5,
        name = "95th Percentile Confidence Interval"
      ) %>%
      hc_add_series(
        data = trend_subset,
        type = "line",
        lineWidth = 5,
        hcaes(x = year, y = trend_est),
        color = "#E55C30",
        name = "Trend"
      ) %>%
      hc_title(
        text = paste(
          "8-Year ",
          title.trend.type(),
          " Trend in Estimated Harbor Seal Abundance By Year ",
          title.trend(),
          sep = ""
        ),
        style = list(color = "#FFFFFF")
      ) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF"))
  })
}
