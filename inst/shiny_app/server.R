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

    # Initial Sync from app_args
    shiny::updateDateInput(session, "departure_date_1_internal", value = app_args$departure_date)
    shiny::updateDateInput(session, "departure_date_2_internal", value = app_args$departure_date)
    shiny::updateSelectInput(session, "mode_1_internal", selected = app_args$mode)

    # Handle Network List Logic
    networks <- app_args$r5r_network
    network_names <- names(networks)
    has_multiple_networks <- length(networks) > 1

    # Render Network Selectors
    output$network_selector_1 <- shiny::renderUI({
      if (has_multiple_networks) {
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::selectInput("network_1_internal", "Network", choices = network_names, selected = network_names[1])
        )
      } else {
        NULL
      }
    })

    output$network_selector_2 <- shiny::renderUI({
      if (has_multiple_networks) {
        # Default to 2nd network if available, else 1st
        default_net <- if (length(network_names) >= 2) network_names[2] else network_names[1]
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::selectInput("network_2_internal", "Network", choices = network_names, selected = default_net)
        )
      } else {
        NULL
      }
    })

    locations <- shiny::reactiveValues(start = NULL, end = NULL)
    copy_code_message <- shiny::reactiveVal(NULL)
    r5r_exec_time_1 <- shiny::reactiveVal(NULL)
    r5r_exec_time_2 <- shiny::reactiveVal(NULL)
    compare_mode <- shiny::reactiveVal(FALSE)

    output$left_sidebar_title <- shiny::renderUI({
      if (compare_mode()) "Route 1 Settings" else "Trip Parameters"
    })

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

    output$copy_code_message_ui <- shiny::renderUI({
      copy_code_message()
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
      mapgl::clear_layer(proxy, "route_layer_1")
      mapgl::clear_layer(proxy, "route_layer_2")
      mapgl::clear_legend(proxy)
      copy_code_message(NULL)
      r5r_exec_time_1(NULL)
      r5r_exec_time_2(NULL)
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
        input$mode_1_internal,
        input$departure_date_1_internal,
        input$departure_time_1_internal,
        input$time_window_1_internal,
        input$max_walk_time_1_internal,
        input$max_trip_duration_1_internal,
        input$network_1_internal,
        input$mode_2_internal,
        input$departure_date_2_internal,
        input$departure_time_2_internal,
        input$time_window_2_internal,
        input$max_walk_time_2_internal,
        input$max_trip_duration_2_internal,
        input$network_2_internal,
        compare_mode()
      ),
      {
        shiny::req(locations$start, locations$end)
        
        is_comparing <- compare_mode()
        
        shiny::req(
          input$mode_1_internal,
          input$max_walk_time_1_internal,
          input$max_trip_duration_1_internal
        )
        if (is_comparing) {
          shiny::req(
            input$mode_2_internal,
            input$max_walk_time_2_internal,
            input$max_trip_duration_2_internal
          )
        }

        shiny::showNotification(
          if (is_comparing) "Calculating routes..." else "Calculating route...",
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

        run_routing <- function(net_id, modes, date, time, window, walk, total) {
          # Select network object
          current_net <- if (!is.null(net_id) && net_id %in% names(networks)) {
            networks[[net_id]]
          } else {
            networks[[1]]
          }

          departure_datetime <- as.POSIXct(
            paste(date, time),
            format = "%Y-%m-%d %H:%M"
          )

          if (is.na(departure_datetime)) return(NULL)

          tryCatch({
            if (utils::packageVersion("r5r") >= "2.3.0") {
              r5r::detailed_itineraries(
                r5r_network = current_net,
                origins = origin,
                destinations = destination,
                mode = modes,
                departure_datetime = departure_datetime,
                time_window = as.integer(window),
                max_walk_time = as.integer(walk),
                max_trip_duration = as.integer(total),
                shortest_path = TRUE,
                drop_geometry = FALSE
              )
            } else {
              r5r::detailed_itineraries(
                r5r_core = current_net,
                origins = origin,
                destinations = destination,
                mode = modes,
                departure_datetime = departure_datetime,
                time_window = as.integer(window),
                max_walk_time = as.integer(walk),
                max_trip_duration = as.integer(total),
                shortest_path = TRUE,
                drop_geometry = FALSE
              )
            }
          }, error = function(e) {
            warning("r5r error: ", e$message)
            return(NULL)
          })
        }

        t1_start <- Sys.time()
        res1 <- run_routing(
          input$network_1_internal,
          input$mode_1_internal, input$departure_date_1_internal, input$departure_time_1_internal,
          input$time_window_1_internal, input$max_walk_time_1_internal, input$max_trip_duration_1_internal
        )
        r5r_exec_time_1(as.numeric(difftime(Sys.time(), t1_start, units = "secs")))

        if (is_comparing) {
          t2_start <- Sys.time()
          res2 <- run_routing(
            input$network_2_internal,
            input$mode_2_internal, input$departure_date_2_internal, input$departure_time_2_internal,
            input$time_window_2_internal, input$max_walk_time_2_internal, input$max_trip_duration_2_internal
          )
          r5r_exec_time_2(as.numeric(difftime(Sys.time(), t2_start, units = "secs")))
          res <- list(res1 = res1, res2 = res2)
        } else {
          res <- res1
          r5r_exec_time_2(NULL)
        }
        
        return(res)
      }
    )

    # --- MAP DRAWING OBSERVER ---
    shiny::observe({
      proxy <- mapgl::maplibre_proxy("map")
      mapgl::clear_layer(proxy, "route_layer_1")
      mapgl::clear_layer(proxy, "route_layer_2")
      mapgl::clear_legend(proxy)

      res <- route_data()
      if (is.null(res)) return()

      is_comparing <- compare_mode()
      
      # Distinct palettes
      palette1 <- c("WALK"="#2f4b7c", "BUS"="#ffa600", "RAIL"="#665191", "SUBWAY"="#d45087", "CAR"="#a05195", "BICYCLE"="#70ad47")
      palette2 <- c("WALK"="#1b4332", "BUS"="#40916c", "RAIL"="#52b788", "SUBWAY"="#74c69d", "CAR"="#95d5b2", "BICYCLE"="#b7e4c7")

      draw_route_layer <- function(detailed_route, layer_id, palette, pos, time_val, legend_id, title_prefix = "Modes") {
        if (is.null(detailed_route) || nrow(detailed_route) == 0) return()
        
        first_option <- if ("option" %in% names(detailed_route)) {
          detailed_route[detailed_route$option == 1, ]
        } else {
          detailed_route
        }
        
        if (nrow(first_option) == 0) return()

        unique_modes <- unique(as.character(first_option$mode))
        mode_colors <- palette[names(palette) %in% unique_modes]
        
        # Handle unknown modes
        new_modes <- setdiff(unique_modes, names(mode_colors))
        if (length(new_modes) > 0) {
          new_colors <- scales::hue_pal()(length(new_modes))
          names(new_colors) <- new_modes
          mode_colors <- c(mode_colors, new_colors)
        }

        # Build legend title with execution time
        time_str <- if (!is.null(time_val)) sprintf("<div style='font-size:10px; color:#777; margin-bottom:2px;'>Time: %dms</div>", round(time_val * 1000)) else ""
        leg_title <- shiny::HTML(paste0(time_str, "<b>", title_prefix, "</b>"))

        # Calculate totals for the summary item
        total_dur <- round(sum(first_option$duration, na.rm = TRUE), 1)
        total_dist <- round(sum(first_option$distance, na.rm = TRUE) / 1000, 2)
        summary_label <- sprintf("Total: %g min, %g km", total_dur, total_dist)

        mapgl::add_legend(
          proxy,
          legend_title = leg_title,
          values = c(names(mode_colors), summary_label),
          colors = c(as.character(mode_colors), "rgba(0,0,0,0)"),
          type = "categorical",
          position = pos,
          unique_id = legend_id,
          add = TRUE
        )

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
        # Determine network labels
        net1_label <- if (has_multiple_networks && !is.null(input$network_1_internal)) paste0(": ", input$network_1_internal) else ""
        net2_label <- if (has_multiple_networks && !is.null(input$network_2_internal)) paste0(": ", input$network_2_internal) else ""
        
        draw_route_layer(res$res1, "route_layer_1", palette1, "top-left", r5r_exec_time_1(), "legend_1", paste0("Route 1", net1_label))
        draw_route_layer(res$res2, "route_layer_2", palette2, "top-right", r5r_exec_time_2(), "legend_2", paste0("Route 2", net2_label))
      } else {
        net1_label <- if (has_multiple_networks && !is.null(input$network_1_internal)) paste0(" (", input$network_1_internal, ")") else ""
        draw_route_layer(res, "route_layer_1", palette1, "top-left", r5r_exec_time_1(), "legend_1", paste0("Modes", net1_label))
      }
      
      # Handle empty results notifications
      if (!is_comparing && !is.null(res) && nrow(res) == 0) {
        shiny::showNotification("No route found.", type = "warning")
      } else if (is_comparing) {
        r1_empty <- is.null(res$res1) || nrow(res$res1) == 0
        r2_empty <- is.null(res$res2) || nrow(res$res2) == 0
        if (r1_empty && r2_empty) {
          shiny::showNotification("No routes found.", type = "warning")
        }
      }
    })

    # --- ITINERARY TABLE RENDERERS ---
    
    # Helper function to render a single itinerary table
    render_itinerary_table <- function(detailed_route) {
      shiny::req(!is.null(detailed_route), nrow(detailed_route) > 0)

      # Show first option
      first_option_df <- if ("option" %in% names(detailed_route)) {
        detailed_route[detailed_route$option == 1, ]
      } else {
        detailed_route
      }
      
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
    }

    # Normal mode table
    output$itinerary_table <- DT::renderDataTable({
      res <- route_data()
      shiny::req(!is.null(res), !compare_mode())
      render_itinerary_table(res)
    })

    # Compare mode table 1
    output$itinerary_table_1 <- DT::renderDataTable({
      res <- route_data()
      shiny::req(!is.null(res), compare_mode())
      render_itinerary_table(res$res1)
    })

    # Compare mode table 2
    output$itinerary_table_2 <- DT::renderDataTable({
      res <- route_data()
      shiny::req(!is.null(res), compare_mode())
      render_itinerary_table(res$res2)
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

      # Handle network referencing in generated code
      get_network_code_ref <- function(net_id) {
        if (!has_multiple_networks) return(network_object_name_for_code)
        # If we have multiple networks, we assume the user has a list object named 'r5r_network' (or whatever passed name)
        # and we access it by name.
        # However, for demo mode, we might need special handling if we want to show 'list(...)' setup?
        # For simplicity, we assume the user has the list object in their environment.
        if (is.null(net_id)) return(paste0(network_object_name_for_code, "[[1]]"))
        paste0(network_object_name_for_code, "[[\"", net_id, "\"]]")
      }

      # Use glue to construct the detailed_itineraries call
      if (is_comparing) {
        net1_ref <- get_network_code_ref(input$network_1_internal)
        net2_ref <- get_network_code_ref(input$network_2_internal)
        
        itinerary_call <- glue::glue(
          "itinerary1 <- r5r::detailed_itineraries(\n",
          "  {network_arg_name} = {net1_ref},\n",
          "  origins = data.frame(id = \"start_point\", lat = {locations$start$lat}, lon = {locations$start$lon}),\n",
          "  destinations = data.frame(id = \"end_point\", lat = {locations$end$lat}, lon = {locations$end$lon}),\n",
          "  mode = {paste0(\"c(\", paste0(shQuote(input$mode_1_internal), collapse = \", \"), \")\")},\n",
          "  departure_datetime = as.POSIXct(\"{input$departure_date_1_internal} {input$departure_time_1_internal}\", format = \"%Y-%m-%d %H:%M\"),\n",
          "  time_window = {as.integer(input$time_window_1_internal)}L,\n",
          "  max_walk_time = {as.integer(input$max_walk_time_1_internal)}L,\n",
          "  max_trip_duration = {as.integer(input$max_trip_duration_1_internal)}L,\n",
          "  shortest_path = TRUE,\n",
          "  drop_geometry = FALSE\n",
          ")\n\n",
          "itinerary2 <- r5r::detailed_itineraries(\n",
          "  {network_arg_name} = {net2_ref},\n",
          "  origins = data.frame(id = \"start_point\", lat = {locations$start$lat}, lon = {locations$start$lon}),\n",
          "  destinations = data.frame(id = \"end_point\", lat = {locations$end$lat}, lon = {locations$end$lon}),\n",
          "  mode = {paste0(\"c(\", paste0(shQuote(input$mode_2_internal), collapse = \", \"), \")\")},\n",
          "  departure_datetime = as.POSIXct(\"{input$departure_date_2_internal} {input$departure_time_2_internal}\", format = \"%Y-%m-%d %H:%M\"),\n",
          "  time_window = {as.integer(input$time_window_2_internal)}L,\n",
          "  max_walk_time = {as.integer(input$max_walk_time_2_internal)}L,\n",
          "  max_trip_duration = {as.integer(input$max_trip_duration_2_internal)}L,\n",
          "  shortest_path = TRUE,\n",
          "  drop_geometry = FALSE\n",
          ")\n\n",
          "# View both travel options on a map\n",
          "mapgl::maplibre(style = mapgl::carto_style(\"voyager\")) |>\n",
          "  mapgl::add_line_layer(id = \"route1\", source = itinerary1[itinerary1$option == 1, ], line_color = \"#2f4b7c\", line_width = 5) |>\n",
          "  mapgl::add_line_layer(id = \"route2\", source = itinerary2[itinerary2$option == 1, ], line_color = \"#1b4332\", line_width = 5)"
        )
      } else {
        net1_ref <- get_network_code_ref(input$network_1_internal)
        
        itinerary_call <- glue::glue(
          "itinerary <- r5r::detailed_itineraries(\n",
          "  {network_arg_name} = {net1_ref},\n",
          "  origins = data.frame(id = \"start_point\", lat = {locations$start$lat}, lon = {locations$start$lon}),\n",
          "  destinations = data.frame(id = \"end_point\", lat = {locations$end$lat}, lon = {locations$end$lon}),\n",
          "  mode = {paste0(\"c(\", paste0(shQuote(input$mode_1_internal), collapse = \", \"), \")\")},\n",
          "  departure_datetime = as.POSIXct(\"{input$departure_date_1_internal} {input$departure_time_1_internal}\", format = \"%Y-%m-%d %H:%M\"),\n",
          "  time_window = {as.integer(input$time_window_1_internal)}L,\n",
          "  max_walk_time = {as.integer(input$max_walk_time_1_internal)}L,\n",
          "  max_trip_duration = {as.integer(input$max_trip_duration_1_internal)}L,\n",
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
