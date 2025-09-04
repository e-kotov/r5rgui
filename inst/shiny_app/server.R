# --- SHINY SERVER LOGIC ---
server <- function(input, output, session) {
  # Retrieve arguments passed from the r5r_gui function
  app_args <- get(".r5rgui_args", envir = .GlobalEnv)
  r5r_network <- app_args$r5r_network
  r5r_network_name <- app_args$r5r_network_name
  map_center <- app_args$center
  map_zoom <- app_args$zoom

  # --- Read demo mode status from global R options ---
  # This will be TRUE if launched from r5r_gui_demo(), FALSE otherwise
  is_demo_mode <- getOption("r5rgui.is_demo_mode", default = FALSE)

  # Update the departure date input with the value from the function arguments
  shiny::updateDateInput(
    session,
    "departure_date",
    value = app_args$departure_date
  )

  locations <- shiny::reactiveValues(start = NULL, end = NULL)

  output$map <- mapgl::renderMaplibre({
    mapgl::maplibre(
      center = map_center,
      zoom = map_zoom,
      style = mapgl::carto_style("voyager")
    )
  })

  # --- COORDINATE HANDLING LOGIC (unchanged) ---
  shiny::observeEvent(input$map_click, {
    coords <- list(lon = input$map_click$lng, lat = input$map_click$lat)
    coords <- list(lat = round(coords$lat, 5), lon = round(coords$lon, 5))
    locations$start <- coords
    session$sendCustomMessage(
      type = 'updateMarker',
      message = list(id = 'start', lng = coords$lon, lat = coords$lat)
    )
    shiny::updateTextInput(
      session,
      "start_coords_input",
      value = paste(round(coords$lat, 5), round(coords$lon, 5), sep = ", ")
    )
  })

  shiny::observeEvent(input$js_right_click, {
    coords <- list(
      lon = input$js_right_click$lng,
      lat = input$js_right_click$lat
    )
    coords <- list(lat = round(coords$lat, 5), lon = round(coords$lon, 5))
    locations$end <- coords
    session$sendCustomMessage(
      type = 'updateMarker',
      message = list(id = 'end', lng = coords$lon, lat = coords$lat)
    )
    shiny::updateTextInput(
      session,
      "end_coords_input",
      value = paste(round(coords$lat, 5), round(coords$lon, 5), sep = ", ")
    )
  })

  shiny::observeEvent(input$marker_dragged, {
    drag_info <- input$marker_dragged
    new_coords <- list(lon = drag_info$lng, lat = drag_info$lat)
    new_coords <- list(
      lat = round(new_coords$lat, 5),
      lon = round(new_coords$lon, 5)
    )
    if (drag_info$id == "start") {
      locations$start <- new_coords
      shiny::updateTextInput(
        session,
        "start_coords_input",
        value = paste(
          round(new_coords$lat, 5),
          round(new_coords$lon, 5),
          sep = ", "
        )
      )
    } else if (drag_info$id == "end") {
      locations$end <- new_coords
      shiny::updateTextInput(
        session,
        "end_coords_input",
        value = paste(
          round(new_coords$lat, 5),
          round(new_coords$lon, 5),
          sep = ", "
        )
      )
    }
  })

  shiny::observeEvent(
    input$start_coords_input,
    {
      shiny::req(input$start_coords_input)
      tryCatch(
        {
          parts <- as.numeric(trimws(strsplit(input$start_coords_input, ",")[[
            1
          ]]))
          if (length(parts) == 2 && !any(is.na(parts))) {
            coords <- list(lat = parts[1], lon = parts[2])
            if (!isTRUE(all.equal(locations$start, coords))) {
              locations$start <- coords
              session$sendCustomMessage(
                type = 'updateMarker',
                message = list(id = 'start', lng = coords$lon, lat = coords$lat)
              )
            }
          }
        },
        error = function(e) {}
      )
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$end_coords_input,
    {
      shiny::req(input$end_coords_input)
      tryCatch(
        {
          parts <- as.numeric(trimws(strsplit(input$end_coords_input, ",")[[
            1
          ]]))
          if (length(parts) == 2 && !any(is.na(parts))) {
            coords <- list(lat = parts[1], lon = parts[2])
            if (!isTRUE(all.equal(locations$end, coords))) {
              locations$end <- coords
              session$sendCustomMessage(
                type = 'updateMarker',
                message = list(id = 'end', lng = coords$lon, lat = coords$lat)
              )
            }
          }
        },
        error = function(e) {}
      )
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(input$reset, {
    locations$start <- NULL
    locations$end <- NULL
    session$sendCustomMessage(type = 'clearAllMarkers', message = 'clear')
    shiny::updateTextInput(session, "start_coords_input", value = "")
    shiny::updateTextInput(session, "end_coords_input", value = "")
    proxy <- mapgl::maplibre_proxy("map")
    mapgl::clear_layer(proxy, "route_layer")
    mapgl::clear_legend(proxy)
  })

  # --- ROUTE CALCULATION (unchanged) ---
  route_data <- shiny::eventReactive(
    list(
      locations$start,
      locations$end,
      input$departure_date,
      input$departure_time,
      input$time_window,
      input$max_walk_time,
      input$max_trip_duration
    ),
    {
      shiny::req(
        locations$start,
        locations$end,
        input$max_walk_time,
        input$max_trip_duration
      )
      shiny::showNotification(
        "Calculating route...",
        duration = 3,
        type = "message"
      )

      origin <- data.frame(
        id = "start_point",
        lat = locations$start$lat,
        lon = locations$start$lon
      )
      destination <- data.frame(
        id = "end_point",
        lat = locations$end$lat,
        lon = locations$end$lon
      )
      departure_datetime <- as.POSIXct(
        paste(input$departure_date, input$departure_time),
        format = "%Y-%m-%d %H:%M"
      )

      if (is.na(departure_datetime)) {
        shiny::showNotification("Invalid date or time format.", type = "error")
        return(NULL)
      }

      tryCatch(
        {
          r5r::detailed_itineraries(
            r5r_network = r5r_network,
            origins = origin,
            destinations = destination,
            mode = c("WALK", "TRANSIT"),
            departure_datetime = departure_datetime,
            time_window = as.integer(input$time_window),
            max_walk_time = as.integer(input$max_walk_time),
            max_trip_duration = as.integer(input$max_trip_duration),
            shortest_path = TRUE,
            drop_geometry = FALSE
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error calculating route:", e$message),
            type = "error"
          )
          return(NULL)
        }
      )
    }
  )

  # --- MAP DRAWING OBSERVER (unchanged) ---
  shiny::observe({
    proxy <- mapgl::maplibre_proxy("map")
    mapgl::clear_layer(proxy, "route_layer")
    mapgl::clear_legend(proxy)

    detailed_route <- route_data()

    if (!is.null(detailed_route) && nrow(detailed_route) > 0) {
      first_option <- detailed_route[detailed_route$option == 1, ]

      unique_modes <- unique(first_option$mode)
      base_colors <- c(
        "WALK" = "#2f4b7c",
        "BUS" = "#ffa600",
        "RAIL" = "#665191",
        "SUBWAY" = "#d45087"
      )
      mode_colors <- base_colors[names(base_colors) %in% unique_modes]

      new_modes <- setdiff(unique_modes, names(mode_colors))
      if (length(new_modes) > 0) {
        new_colors <- scales::hue_pal()(length(new_modes))
        names(new_colors) <- new_modes
        mode_colors <- c(mode_colors, new_colors)
      }

      mapgl::add_legend(
        proxy,
        legend_title = "Travel Mode",
        values = names(mode_colors),
        colors = as.character(mode_colors),
        type = "categorical"
      )

      mapgl::add_line_layer(
        proxy,
        id = "route_layer",
        source = first_option,
        line_color = mapgl::match_expr(
          column = "mode",
          values = names(mode_colors),
          stops = as.character(mode_colors),
          default = "gray"
        ),
        line_width = 5,
        line_opacity = 0.8,
        tooltip = "mode"
      )
    } else if (!is.null(route_data()) && nrow(route_data()) == 0) {
      shiny::showNotification("No route found.", type = "warning")
    }
  })

  # --- ITINERARY TABLE RENDERER (unchanged) ---
  output$itinerary_table <- DT::renderDataTable({
    detailed_route <- route_data()
    shiny::req(!is.null(detailed_route), nrow(detailed_route) > 0)

    first_option_df <- detailed_route[detailed_route$option == 1, ]
    display_df <- sf::st_drop_geometry(first_option_df)

    cols_to_show <- c(
      "mode",
      "distance",
      "duration",
      "route_short_name",
      "trip_headsign",
      "departure_time",
      "arrival_time"
    )
    cols_exist <- cols_to_show[cols_to_show %in% names(display_df)]
    display_df <- display_df[, cols_exist, with = FALSE]

    if ("duration" %in% names(display_df)) {
      display_df$duration <- round(display_df$duration, 1)
    }
    if ("distance" %in% names(display_df)) {
      display_df$distance <- round(display_df$distance, 2)
    }

    DT::datatable(
      display_df,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  # --- OBSERVER FOR COPY CODE BUTTON ---
  shiny::observeEvent(input$copy_code, {
    if (is.null(locations$start) || is.null(locations$end)) {
      shiny::showNotification(
        "Please set start and end points on the map first.",
        type = "warning",
        duration = 5
      )
      return()
    }

    departure_datetime_str <- paste(input$departure_date, input$departure_time)

    # Conditionally define the setup code and the network object name.
    if (is_demo_mode) {
      # If it's a demo, provide the full setup script.
      setup_code <- paste0(
        "# --- Setup code for r5r Porto Alegre sample data ---\n",
        "data_path <- system.file(\"extdata/poa\", package = \"r5r\")\n",
        "r5r_network <- r5r::build_network(data_path = data_path, verbose = FALSE)\n\n",
        "# --- Itinerary calculation ---\n"
      )
      # The itinerary call will refer to the `r5r_network` object created above.
      network_object_name <- "r5r_network"
    } else {
      # If not a demo, the user has their own network object, so no setup code is needed.
      setup_code <- ""
      # Use the name of the object the user passed to the r5r_gui() function.
      network_object_name <- r5r_network_name
    }

    itinerary_call <- glue::glue(
      "itinerary <- r5r::detailed_itineraries(\n",
      "  r5r_network = {network_object_name},\n",
      "  origins = data.frame(\n",
      "    id = \"start_point\",\n",
      "    lat = {locations$start$lat},\n",
      "    lon = {locations$start$lon}\n",
      "  ),\n",
      "  destinations = data.frame(\n",
      "    id = \"end_point\",\n",
      "    lat = {locations$end$lat},\n",
      "    lon = {locations$end$lon}\n",
      "  ),\n",
      "  mode = c(\"WALK\", \"TRANSIT\"),\n",
      "  departure_datetime = as.POSIXct(\"{departure_datetime_str}\", format = \"%Y-%m-%d %H:%M\"),\n",
      "  time_window = {as.integer(input$time_window)}L,\n",
      "  max_walk_time = {as.integer(input$max_walk_time)}L,\n",
      "  max_trip_duration = {as.integer(input$max_trip_duration)}L,\n",
      "  shortest_path = TRUE,\n",
      "  drop_geometry = FALSE\n",
      ")\n\n",
      "# View the first travel option on a map\n",
      "mapgl::maplibre_view(itinerary[itinerary$option == 1, ], column = \"mode\")"
    )

    # Combine the setup code (if any) with the main itinerary call.
    code_string <- paste0(setup_code, itinerary_call)

    shiny::showModal(shiny::modalDialog(
      title = "R Code for detailed_itineraries()",
      shiny::textAreaInput(
        "code_output",
        label = "Copy the code below:",
        value = code_string,
        width = "100%",
        height = "300px",
        resize = "vertical"
      ),
      easyClose = TRUE,
      footer = shiny::modalButton("Dismiss")
    ))
  })
}
