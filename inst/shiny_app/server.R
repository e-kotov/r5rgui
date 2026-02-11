# --- SHINY SERVER FACTORY ---
function(app_args) {
  # The returned function is the server logic. It has access to 'app_args'.
  function(input, output, session) {
    # Retrieve arguments passed from the r5r_gui function
    # The line below is the only one removed from the original server logic.
    # app_args <- get(".r5rgui_args", envir = .GlobalEnv)
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

    # Update the transport mode input with the value from the function arguments
    shiny::updateSelectInput(
      session,
      "mode",
      selected = app_args$mode
    )

    locations <- shiny::reactiveValues(start = NULL, end = NULL)
    copy_code_message <- shiny::reactiveVal(NULL)
    r5r_exec_time <- shiny::reactiveVal(NULL)
    compare_mode <- shiny::reactiveVal(FALSE)

    output$compare_mode_ui <- shiny::renderUI({
      active <- compare_mode()
      label <- if (active) "Mode: Compare" else "Mode: Normal"
      color <- if (active) "#984ea3" else "#3b82f6"
      
      shiny::actionButton(
        "toggle_compare",
        label,
        style = sprintf("background-color: %s; color: white; border-width: 0px;", color)
      )
    })

    shiny::observeEvent(input$toggle_compare, {
      compare_mode(!compare_mode())
    })

    # Keep a hidden input for conditional panel in UI
    shiny::observe({
      session$sendCustomMessage("updateCompareMode", compare_mode())
    })

    # Sync mode inputs when switching
    shiny::observeEvent(compare_mode(), {
      if (compare_mode()) {
        shiny::updateSelectInput(session, "mode_1", selected = input$mode)
      } else {
        shiny::updateSelectInput(session, "mode", selected = input$mode_1)
      }
    })

    output$copy_code_message_ui <- shiny::renderUI({
      copy_code_message()
    })

    output$exec_time_overlay_ui <- shiny::renderUI({
      et <- r5r_exec_time()
      if (is.null(et)) {
        return(NULL)
      }
      shiny::div(
        style = "background: rgba(255, 255, 255, 0.8); padding: 2px 6px; border-radius: 4px; font-size: 11px; color: #777; pointer-events: none; border: 1px solid rgba(0,0,0,0.1); margin-bottom: 5px;",
        shiny::div("Last request:"),
        shiny::div(paste0(round(et * 1000, 0), "ms"), style = "font-weight: bold; color: #333;")
      )
    })

    output$map <- mapgl::renderMaplibre({
      mapgl::maplibre(
        center = map_center,
        zoom = map_zoom,
        style = mapgl::carto_style("voyager")
      )
    })

    # --- COORDINATE HANDLING LOGIC ---
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
                  message = list(
                    id = 'start',
                    lng = coords$lon,
                    lat = coords$lat
                  )
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
      mapgl::clear_layer(proxy, "route_layer_1")
      mapgl::clear_layer(proxy, "route_layer_2")
      mapgl::clear_legend(proxy)
      copy_code_message(NULL)
      r5r_exec_time(NULL)
      compare_mode(FALSE)
    })

    # --- OBSERVERS TO CLEAR COPY CODE MESSAGE ---
    shiny::observeEvent(
      input$map_click,
      {
        copy_code_message(NULL)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$js_right_click,
      {
        copy_code_message(NULL)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$start_coords_input,
      {
        copy_code_message(NULL)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$end_coords_input,
      {
        copy_code_message(NULL)
      },
      ignoreInit = TRUE
    )

    # --- ROUTE CALCULATION ---
    route_data <- shiny::eventReactive(
      list(
        locations$start,
        locations$end,
        input$departure_date,
        input$departure_time,
        input$time_window,
        input$max_walk_time,
        input$max_trip_duration,
        input$mode,
        input$mode_1,
        input$mode_2,
        compare_mode()
      ),
      {
        shiny::req(
          locations$start,
          locations$end,
          input$max_walk_time,
          input$max_trip_duration
        )
        
        is_comparing <- compare_mode()
        modes_to_use <- if (is_comparing) list(m1 = input$mode_1, m2 = input$mode_2) else list(m = input$mode)
        
        # Validate that modes are not empty
        if (is_comparing) {
          shiny::req(length(modes_to_use$m1) > 0, length(modes_to_use$m2) > 0)
        } else {
          shiny::req(length(modes_to_use$m) > 0)
        }

        shiny::showNotification(
          if (is_comparing) "Calculating routes (Compare Mode)..." else "Calculating route...",
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

        run_routing <- function(modes) {
          # Ensure r5r_network is accessible (it should be via lexical scoping)
          tryCatch({
            if (utils::packageVersion("r5r") >= "2.3.0") {
              r5r::detailed_itineraries(
                r5r_network = r5r_network,
                origins = origin,
                destinations = destination,
                mode = modes,
                departure_datetime = departure_datetime,
                time_window = as.integer(input$time_window),
                max_walk_time = as.integer(input$max_walk_time),
                max_trip_duration = as.integer(input$max_trip_duration),
                shortest_path = TRUE,
                drop_geometry = FALSE
              )
            } else {
              r5r::detailed_itineraries(
                r5r_core = r5r_network,
                origins = origin,
                destinations = destination,
                mode = modes,
                departure_datetime = departure_datetime,
                time_window = as.integer(input$time_window),
                max_walk_time = as.integer(input$max_walk_time),
                max_trip_duration = as.integer(input$max_trip_duration),
                shortest_path = TRUE,
                drop_geometry = FALSE
              )
            }
          }, error = function(e) {
            warning("r5r error: ", e$message)
            return(NULL)
          })
        }

        t0 <- Sys.time()
        if (is_comparing) {
          res1 <- run_routing(modes_to_use$m1)
          res2 <- run_routing(modes_to_use$m2)
          res <- list(res1 = res1, res2 = res2)
        } else {
          res <- run_routing(modes_to_use$m)
        }
        r5r_exec_time(as.numeric(difftime(Sys.time(), t0, units = "secs")))
        
        return(res)
      }
    )

    # --- MAP DRAWING OBSERVER ---
    shiny::observe({
      proxy <- mapgl::maplibre_proxy("map")
      mapgl::clear_layer(proxy, "route_layer")
      mapgl::clear_layer(proxy, "route_layer_1")
      mapgl::clear_layer(proxy, "route_layer_2")
      mapgl::clear_legend(proxy)

      res <- route_data()
      if (is.null(res)) return()

      is_comparing <- compare_mode()
      
      # Base color palette for common modes
      base_colors <- c(
        "WALK" = "#2f4b7c",
        "BUS" = "#ffa600",
        "RAIL" = "#665191",
        "SUBWAY" = "#d45087",
        "CAR" = "#a05195",
        "BICYCLE" = "#70ad47"
      )

      # Helper to extract first option
      get_first_option <- function(df) {
        if (is.null(df) || nrow(df) == 0) return(NULL)
        if ("option" %in% names(df)) df[df$option == 1, ] else df
      }

      all_legend_values <- character()
      all_legend_colors <- character()

      draw_route_layer <- function(detailed_route, layer_id, legend_prefix = "") {
        first_option <- get_first_option(detailed_route)
        if (is.null(first_option) || nrow(first_option) == 0) return()

        unique_modes <- unique(as.character(first_option$mode))
        
        # Determine colors for this specific route's modes
        mode_colors <- base_colors[names(base_colors) %in% unique_modes]
        new_modes <- setdiff(unique_modes, names(mode_colors))
        if (length(new_modes) > 0) {
          new_colors <- scales::hue_pal()(length(new_modes))
          names(new_colors) <- new_modes
          mode_colors <- c(mode_colors, new_colors)
        }

        # Accumulate for global legend
        for (m in unique_modes) {
          all_legend_values <<- c(all_legend_values, paste0(legend_prefix, m))
          all_legend_colors <<- c(all_legend_colors, as.character(mode_colors[m]))
        }

        mapgl::add_line_layer(
          proxy,
          id = layer_id,
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
      }

      if (is_comparing) {
        draw_route_layer(res$res1, "route_layer_1", "1: ")
        draw_route_layer(res$res2, "route_layer_2", "2: ")
      } else {
        draw_route_layer(res, "route_layer")
      }
      
      # Add combined legend if we have values
      if (length(all_legend_values) > 0) {
        mapgl::add_legend(
          proxy,
          legend_title = "Travel Mode",
          values = all_legend_values,
          colors = all_legend_colors,
          type = "categorical"
        )
      }
      
      # Handle empty results notifications
      if (!is_comparing && !is.null(res) && nrow(res) == 0) {
        shiny::showNotification("No route found. Try increasing walk time or checking points.", type = "warning")
      } else if (is_comparing) {
        r1_empty <- is.null(res$res1) || nrow(res$res1) == 0
        r2_empty <- is.null(res$res2) || nrow(res$res2) == 0
        if (r1_empty && r2_empty) {
          shiny::showNotification("No routes found for either selection.", type = "warning")
        } else if (r1_empty) {
          shiny::showNotification("Route 1 not found.", type = "warning")
        } else if (r2_empty) {
          shiny::showNotification("Route 2 not found.", type = "warning")
        }
      }
    })

    # --- ITINERARY TABLE RENDERER ---
    output$itinerary_table <- DT::renderDataTable({
      res <- route_data()
      shiny::req(!is.null(res))
      
      detailed_route <- if (compare_mode()) res$res1 else res
      shiny::req(!is.null(detailed_route), nrow(detailed_route) > 0)

      # Show first option
      first_option_df <- if ("option" %in% names(detailed_route)) {
        detailed_route[detailed_route$option == 1, ]
      } else {
        detailed_route
      }
      
      display_df <- sf::st_drop_geometry(first_option_df)
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
    # This observer handles the "Copy Code" button functionality.
    # When triggered, it generates R code that reproduces the current routing query, allowing users to copy and run it outside the GUI.
    #
    # The observer includes logic to handle compatibility with different versions of the r5r package.
    # Specifically, starting from r5r version 2.3.0, the routing functions and argument names changed:
    #   - The network object argument changed from 'r5r_core' to 'r5r_network'.
    #   - The function signatures may differ between versions.
    # The code checks the installed r5r version and adjusts the generated code accordingly, ensuring that the copied code will work regardless of which r5r version the user has installed.
    shiny::observeEvent(input$copy_code, {
      if (is.null(locations$start) || is.null(locations$end)) {
        copy_code_message(
          shiny::div(
            style = "background-color: #ea8436ff; color: white; padding: 5px 10px; border-radius: 5px; margin-bottom: 5px;",
            "Please set start and end points on the map first."
          )
        )
        return()
      }

      # If we have start/end points, clear any existing message
      copy_code_message(NULL)

      departure_datetime_str <- paste(
        input$departure_date,
        input$departure_time
      )

      # Check r5r version to determine correct function and argument names
      r5r_version_ge_230 <- utils::packageVersion("r5r") >= "2.3.0"
      network_arg_name <- if (r5r_version_ge_230) "r5r_network" else "r5r_core"

      setup_code <- ""
      network_object_name_for_code <- r5r_network_name

      if (is_demo_mode) {
        network_object_name_for_code <- "r5r_network" # This is the object name created in the setup code

        # Determine the full line of code for setting up the network
        setup_call_string <- if (r5r_version_ge_230) {
          "r5r_network <- r5r::build_network(data_path = data_path, verbose = FALSE)"
        } else {
          "r5r_network <- r5r::setup_r5(data_path = data_path, verbose = FALSE)"
        }

        setup_code <- glue::glue(
          "# --- Setup code for r5r Porto Alegre sample data ---\n",
          "data_path <- system.file(\"extdata/poa\", package = \"r5r\")\n",
          "{setup_call_string}\n\n",
          "# --- Itinerary calculation ---\n"
        )
      }

      is_comparing <- compare_mode()

      # Use glue to construct the detailed_itineraries call
      if (is_comparing) {
        itinerary_call <- glue::glue(
          "itinerary1 <- r5r::detailed_itineraries(\n",
          "  {network_arg_name} = {network_object_name_for_code},\n",
          "  origins = data.frame(id = \"start_point\", lat = {locations$start$lat}, lon = {locations$start$lon}),\n",
          "  destinations = data.frame(id = \"end_point\", lat = {locations$end$lat}, lon = {locations$end$lon}),\n",
          "  mode = {paste0(\"c(\", paste0(shQuote(input$mode_1), collapse = \", \"), \")\")},\n",
          "  departure_datetime = as.POSIXct(\"{departure_datetime_str}\", format = \"%Y-%m-%d %H:%M\"),\n",
          "  time_window = {as.integer(input$time_window)}L,\n",
          "  max_walk_time = {as.integer(input$max_walk_time)}L,\n",
          "  max_trip_duration = {as.integer(input$max_trip_duration)}L,\n",
          "  shortest_path = TRUE,\n",
          "  drop_geometry = FALSE\n",
          ")\n\n",
          "itinerary2 <- r5r::detailed_itineraries(\n",
          "  {network_arg_name} = {network_object_name_for_code},\n",
          "  origins = data.frame(id = \"start_point\", lat = {locations$start$lat}, lon = {locations$start$lon}),\n",
          "  destinations = data.frame(id = \"end_point\", lat = {locations$end$lat}, lon = {locations$end$lon}),\n",
          "  mode = {paste0(\"c(\", paste0(shQuote(input$mode_2), collapse = \", \"), \")\")},\n",
          "  departure_datetime = as.POSIXct(\"{departure_datetime_str}\", format = \"%Y-%m-%d %H:%M\"),\n",
          "  time_window = {as.integer(input$time_window)}L,\n",
          "  max_walk_time = {as.integer(input$max_walk_time)}L,\n",
          "  max_trip_duration = {as.integer(input$max_trip_duration)}L,\n",
          "  shortest_path = TRUE,\n",
          "  drop_geometry = FALSE\n",
          ")\n\n",
          "# View both travel options on a map\n",
          "mapgl::maplibre(style = mapgl::carto_style(\"voyager\")) |>\n",
          "  mapgl::add_line_layer(id = \"route1\", source = itinerary1[itinerary1$option == 1, ], line_color = \"#2f4b7c\", line_width = 5) |>\n",
          "  mapgl::add_line_layer(id = \"route2\", source = itinerary2[itinerary2$option == 1, ], line_color = \"#ffa600\", line_width = 5)"
        )
      } else {
        itinerary_call <- glue::glue(
          "itinerary <- r5r::detailed_itineraries(\n",
          "  {network_arg_name} = {network_object_name_for_code},\n",
          "  origins = data.frame(id = \"start_point\", lat = {locations$start$lat}, lon = {locations$start$lon}),\n",
          "  destinations = data.frame(id = \"end_point\", lat = {locations$end$lat}, lon = {locations$end$lon}),\n",
          "  mode = {paste0(\"c(\", paste0(shQuote(input$mode), collapse = \", \"), \")\")},\n",
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
      }

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

    # --- OBSERVER FOR QUIT BUTTON ---
    shiny::observeEvent(input$quit_app, {
      shiny::stopApp()
    })
  }
}
