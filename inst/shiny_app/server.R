# --- SHINY SERVER FACTORY ---
function(app_args) {
  # The returned function is the server logic. It has access to 'app_args'.
  function(input, output, session) {
    # Retrieve arguments passed from the r5r_gui function
    r5r_network <- app_args$r5r_network
    r5r_network_name <- app_args$r5r_network_name
    map_center <- app_args$center
    map_zoom <- app_args$zoom
    basemaps <- app_args$basemaps

    # Fallback if basemaps is missing or NULL
    if (is.null(basemaps)) {
      basemaps <- list("Voyager" = mapgl::carto_style("voyager"))
    }

    # --- Read demo mode status from global R options ---
    is_demo_mode <- getOption("r5rgui.is_demo_mode", default = FALSE)

    # Initial Sync from app_args
    shiny::updateDateInput(
      session,
      "departure_date_1_internal",
      value = app_args$departure_date
    )
    shiny::updateDateInput(
      session,
      "departure_date_2_internal",
      value = app_args$departure_date
    )
    shiny::updateSelectInput(
      session,
      "mode_1_internal",
      selected = app_args$mode
    )

    # Handle Network List Logic
    networks <- app_args$r5r_network
    network_names <- names(networks)
    has_multiple_networks <- length(networks) > 1

    # Render Network Selectors
    output$network_selector_1 <- shiny::renderUI({
      if (has_multiple_networks) {
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::selectInput(
            "network_1_internal",
            "Network",
            choices = network_names,
            selected = network_names[1]
          )
        )
      } else {
        NULL
      }
    })

    output$network_selector_2 <- shiny::renderUI({
      if (has_multiple_networks) {
        default_net <- if (length(network_names) >= 2) {
          network_names[2]
        } else {
          network_names[1]
        }
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::selectInput(
            "network_2_internal",
            "Network",
            choices = network_names,
            selected = default_net
          )
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
    cube_mode <- shiny::reactiveVal(FALSE)
    mapgl_3d_mode_val <- shiny::reactiveVal(FALSE)
    current_basemap <- shiny::reactiveVal(unname(basemaps[[1]]))

    # Output rendering for conditionally disabling tabs based on whether network is provided
    output$has_network <- shiny::reactive({
      !is.null(networks)
    })
    shiny::outputOptions(output, "has_network", suspendWhenHidden = FALSE)

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
        style = sprintf(
          "background-color: %s; color: white; border-width: 0px;",
          color
        )
      )
    })

    output$mapgl_3d_ui <- shiny::renderUI({
      active <- mapgl_3d_mode_val()
      label <- if (active) "MapGL 3D: On" else "MapGL 3D: Off"
      color <- if (active) "#21918c" else "#6c757d"

      shiny::actionButton(
        "toggle_mapgl_3d",
        label,
        style = sprintf(
          "background-color: %s; color: white; border-width: 0px;",
          color
        )
      )
    })

    output$cube_mode_ui <- shiny::renderUI({
      active <- cube_mode()
      label <- if (active) "3D View: On" else "3D View: Off"
      color <- if (active) "#984ea3" else "#6c757d"

      shiny::actionButton(
        "toggle_cube",
        label,
        style = sprintf(
          "background-color: %s; color: white; border-width: 0px;",
          color
        )
      )
    })

    shiny::observeEvent(input$toggle_compare, {
      compare_mode(!compare_mode())
    })

    shiny::observeEvent(input$toggle_mapgl_3d, {
      mapgl_3d_mode_val(!mapgl_3d_mode_val())
      shiny::updateCheckboxInput(session, "mapgl_3d_mode", value = mapgl_3d_mode_val())
    })

    shiny::observeEvent(input$toggle_cube, {
      cube_mode(!cube_mode())
    })

    shiny::observe({
      session$sendCustomMessage("updateCompareMode", compare_mode())
    })

    shiny::observe({
      session$sendCustomMessage("updateCubeMode", cube_mode())
    })

    shiny::observe({
      session$sendCustomMessage("updateMapGL3DMode", mapgl_3d_mode_val())
    })

    # Travel Time Surface Grid reactive calculation removed

    output$copy_code_message_ui <- shiny::renderUI({
      copy_code_message()
    })

    # Basemap Selector UI
    output$basemap_ui <- shiny::renderUI({
      shiny::selectInput(
        "basemap",
        label = NULL,
        choices = basemaps,
        selected = unname(basemaps[[1]]),
        width = "200px"
      )
    })

    # Basemap Observer with Persistent Style Handler
    shiny::observeEvent(input$basemap, {
      shiny::req(input$basemap)
      if (input$basemap != current_basemap()) {
        session$sendCustomMessage(
          "setPersistentStyle",
          list(styleUrl = input$basemap)
        )
        current_basemap(input$basemap)
      }
    })

    output$map <- mapgl::renderMaplibre({
      mapgl::maplibre(
        center = map_center,
        zoom = map_zoom,
        style = unname(basemaps[[1]])
      ) |>
        mapgl::add_navigation_control(position = "bottom-right") |>
        mapgl::add_fullscreen_control(position = "bottom-right") |>
        mapgl::add_scale_control(position = "bottom-left", unit = "metric") |>
        mapgl::add_draw_control(
          rectangle = FALSE,
          freehand = FALSE,
          point = FALSE,
          line_string = FALSE,
          polygon = TRUE,
          trash = TRUE
        )
    })

    # --- COORDINATE HANDLING LOGIC ---
    shiny::observeEvent(input$map_click, {
      if (isTRUE(mapgl_3d_mode_val())) return()
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
      if (isTRUE(mapgl_3d_mode_val())) return()
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

        run_routing <- function(
          net_id,
          modes,
          date,
          time,
          window,
          walk,
          total
        ) {
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

          if (is.na(departure_datetime)) {
            return(NULL)
          }

          tryCatch(
            {
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
            },
            error = function(e) {
              warning("r5r error: ", e$message)
              return(NULL)
            }
          )
        }

        t1_start <- Sys.time()
        res1 <- run_routing(
          input$network_1_internal,
          input$mode_1_internal,
          input$departure_date_1_internal,
          input$departure_time_1_internal,
          input$time_window_1_internal,
          input$max_walk_time_1_internal,
          input$max_trip_duration_1_internal
        )
        r5r_exec_time_1(as.numeric(difftime(
          Sys.time(),
          t1_start,
          units = "secs"
        )))

        show_iso <- isTRUE(input$show_3d_isochrones)
        cutoff <- if (show_iso) as.numeric(input$isochrone_cutoff_3d) else 15

        # Calculate isochrones for Route 1 if enabled
        iso1 <- NULL
        if (show_iso && !is.null(res1) && nrow(res1) > 0) {
          net1 <- if (!is.null(input$network_1_internal) && input$network_1_internal %in% names(networks)) {
            networks[[input$network_1_internal]]
          } else {
            networks[[1]]
          }
          iso1 <- calculate_route_isochrones(
            itinerary = res1,
            r5r_network = net1,
            cutoffs = cutoff,
            mode = "WALK",
            date = input$departure_date_1_internal
          )
        }

        if (is_comparing) {
          t2_start <- Sys.time()
          res2 <- run_routing(
            input$network_2_internal,
            input$mode_2_internal,
            input$departure_date_2_internal,
            input$departure_time_2_internal,
            input$time_window_2_internal,
            input$max_walk_time_2_internal,
            input$max_trip_duration_2_internal
          )
          r5r_exec_time_2(as.numeric(difftime(
            Sys.time(),
            t2_start,
            units = "secs"
          )))

          # Calculate isochrones for Route 2 if enabled
          iso2 <- NULL
          if (show_iso && !is.null(res2) && nrow(res2) > 0) {
            net2 <- if (!is.null(input$network_2_internal) && input$network_2_internal %in% names(networks)) {
              networks[[input$network_2_internal]]
            } else {
              if (length(networks) >= 2) networks[[2]] else networks[[1]]
            }
            iso2 <- calculate_route_isochrones(
              itinerary = res2,
              r5r_network = net2,
              cutoffs = cutoff,
              mode = "WALK",
              date = input$departure_date_2_internal
            )
          }

          res <- list(res1 = res1, iso1 = iso1, res2 = res2, iso2 = iso2)
        } else {
          res <- list(res = res1, iso = iso1)
          r5r_exec_time_2(NULL)
        }

        return(res)
      }
    )

    # --- MAP DRAWING OBSERVER ---
    shiny::observe({
      res <- route_data()

      shiny::isolate({
        proxy <- mapgl::maplibre_proxy("map")
        mapgl::clear_layer(proxy, "route_layer_1")
        mapgl::clear_layer(proxy, "route_layer_2")
        mapgl::clear_layer(proxy, "isochrone_layer_1")
        mapgl::clear_layer(proxy, "isochrone_layer_2")
        mapgl::clear_legend(proxy)

        if (is.null(res)) {
          return()
        }

        is_comparing <- compare_mode()

        if (is_comparing) {
          route1 <- res$res1
          iso1 <- res$iso1
          route2 <- res$res2
          iso2 <- res$iso2
        } else {
          route1 <- res$res
          iso1 <- res$iso
          route2 <- NULL
          iso2 <- NULL
        }

        # Distinct palettes
        palette1 <- c(
          "WALK" = "#2f4b7c",
          "BUS" = "#ffa600",
          "RAIL" = "#665191",
          "SUBWAY" = "#d45087",
          "CAR" = "#a05195",
          "BICYCLE" = "#70ad47"
        )
        palette2 <- c(
          "WALK" = "#1b4332",
          "BUS" = "#40916c",
          "RAIL" = "#52b788",
          "SUBWAY" = "#74c69d",
          "CAR" = "#95d5b2",
          "BICYCLE" = "#b7e4c7"
        )

        draw_route_layer <- function(
          detailed_route,
          layer_id,
          palette,
          pos,
          time_val,
          legend_id,
          title_prefix = "Modes"
        ) {
          if (is.null(detailed_route) || nrow(detailed_route) == 0) {
            return()
          }

          first_option <- if ("option" %in% names(detailed_route)) {
            detailed_route[detailed_route$option == 1, ]
          } else {
            detailed_route
          }

          if (nrow(first_option) == 0) {
            return()
          }

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
          time_str <- if (!is.null(time_val)) {
            sprintf(
              "<div style='font-size:10px; color:#777; margin-bottom:2px;'>Query time: %dms</div>",
              round(time_val * 1000)
            )
          } else {
            ""
          }
          leg_title <- shiny::HTML(paste0(
            time_str,
            "<b>",
            title_prefix,
            "</b>"
          ))

          # Calculate totals for the summary item
          if ("total_duration" %in% names(first_option)) {
            total_dur_min <- first_option$total_duration[1]
          } else if ("segment_duration" %in% names(first_option)) {
            total_dur_min <- sum(first_option$segment_duration, na.rm = TRUE) +
              sum(first_option$wait, na.rm = TRUE)
          } else {
            total_dur_min <- 0
          }

          total_dist_km <- round(
            sum(first_option$distance, na.rm = TRUE) / 1000,
            2
          )

          # Format duration
          if (total_dur_min > 60) {
            hours <- floor(total_dur_min / 60)
            mins <- round(total_dur_min %% 60)
            dur_str <- sprintf("%dh %dm", hours, mins)
          } else {
            dur_str <- sprintf("%g min", round(total_dur_min, 1))
          }

          summary_labels <- c(
            sprintf("Time: %s", dur_str),
            sprintf("Dist: %g km", total_dist_km)
          )

          mapgl::add_legend(
            proxy,
            legend_title = leg_title,
            values = c(names(mode_colors), summary_labels),
            colors = c(
              as.character(mode_colors),
              "rgba(0,0,0,0)",
              "rgba(0,0,0,0)"
            ),
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
          net1_label <- if (
            has_multiple_networks && !is.null(input$network_1_internal)
          ) {
            paste0(": ", input$network_1_internal)
          } else {
            ""
          }
          net2_label <- if (
            has_multiple_networks && !is.null(input$network_2_internal)
          ) {
            paste0(": ", input$network_2_internal)
          } else {
            ""
          }

          draw_route_layer(
            route1,
            "route_layer_1",
            palette1,
            "top-left",
            r5r_exec_time_1(),
            "legend_1",
            paste0("Route 1", net1_label)
          )
          draw_route_layer(
            route2,
            "route_layer_2",
            palette2,
            "top-right",
            r5r_exec_time_2(),
            "legend_2",
            paste0("Route 2", net2_label)
          )
        } else {
          net1_label <- if (
            has_multiple_networks && !is.null(input$network_1_internal)
          ) {
            paste0(" (" , input$network_1_internal, ")")
          } else {
            ""
          }
          draw_route_layer(
            route1,
            "route_layer_1",
            palette1,
            "top-left",
            r5r_exec_time_1(),
            "legend_1",
            paste0("Modes", net1_label)
          )
        }

        # Draw Isochrone 1
        if (!is.null(iso1) && nrow(iso1) > 0) {
          iso_colors <- c("15" = "#238b45", "30" = "#fec44f", "45" = "#ec7014", "60" = "#cc4c02")
          unique_cuts <- unique(as.character(iso1$isochrone))
          matching_colors <- iso_colors[names(iso_colors) %in% unique_cuts]
          new_cuts <- setdiff(unique_cuts, names(matching_colors))
          if (length(new_cuts) > 0) {
            new_colors <- scales::hue_pal()(length(new_cuts))
            names(new_colors) <- new_cuts
            matching_colors <- c(matching_colors, new_colors)
          }
          
          mapgl::add_fill_layer(
            proxy,
            id = "isochrone_layer_1",
            source = iso1,
            fill_color = mapgl::match_expr(
              column = "isochrone",
              values = names(matching_colors),
              stops = as.character(matching_colors),
              default = "#8856a7"
            ),
            fill_opacity = 0.35,
            fill_outline_color = "#1b4332",
            before_id = "route_layer_1"
          )
        }

        # Draw Isochrone 2
        if (is_comparing && !is.null(iso2) && nrow(iso2) > 0) {
          iso_colors <- c("15" = "#238b45", "30" = "#fec44f", "45" = "#ec7014", "60" = "#cc4c02")
          unique_cuts <- unique(as.character(iso2$isochrone))
          matching_colors <- iso_colors[names(iso_colors) %in% unique_cuts]
          new_cuts <- setdiff(unique_cuts, names(matching_colors))
          if (length(new_cuts) > 0) {
            new_colors <- scales::hue_pal()(length(new_cuts))
            names(new_colors) <- new_cuts
            matching_colors <- c(matching_colors, new_colors)
          }
          
          mapgl::add_fill_layer(
            proxy,
            id = "isochrone_layer_2",
            source = iso2,
            fill_color = mapgl::match_expr(
              column = "isochrone",
              values = names(matching_colors),
              stops = as.character(matching_colors),
              default = "#8856a7"
            ),
            fill_opacity = 0.35,
            fill_outline_color = "#1b4332",
            before_id = "route_layer_2"
          )
        }

        # Handle empty results notifications
        if (!is_comparing && !is.null(route1) && nrow(route1) == 0) {
          shiny::showNotification("No route found.", type = "warning")
        } else if (is_comparing) {
          r1_empty <- is.null(route1) || nrow(route1) == 0
          r2_empty <- is.null(route2) || nrow(route2) == 0
          if (r1_empty && r2_empty) {
            shiny::showNotification("No routes found.", type = "warning")
          }
        }
      })
    })

    # --- MAPGL 3D SPACE-TIME CUBE OBSERVER ---
    shiny::observe({
      proxy <- mapgl::maplibre_proxy("map")
      
      # Always clear it first when this block runs
      mapgl::clear_layer(proxy, "spacetime_cube_layer")

      if (!isTRUE(mapgl_3d_mode_val())) return()
      shiny::req(locations$start)

      net <- if (!is.null(input$network_1_internal) && input$network_1_internal %in% names(networks)) {
        networks[[input$network_1_internal]]
      } else {
        networks[[1]]
      }

      shiny::showNotification("Calculating 3D MapGL Space-Time Cube...", id = "mapgl_cube_calc", duration = NULL)
      on.exit(shiny::removeNotification("mapgl_cube_calc"), add = TRUE)

      origin_coords <- c(locations$start$lon, locations$start$lat)

      cube_data <- get_space_time_cube_data(
        r5r_network = net,
        origin_coords = origin_coords,
        access_variable = if (!is.null(input$access_variable_3d)) input$access_variable_3d else "jobs",
        access_cutoff = if (!is.null(input$access_cutoff_3d)) as.numeric(input$access_cutoff_3d) else 30,
        access_metric = if (!is.null(input$access_metric_3d)) input$access_metric_3d else "cumulative",
        access_n_closest = if (!is.null(input$access_n_closest_3d)) as.integer(input$access_n_closest_3d) else 1L,
        date = input$departure_date_1_internal
      )

      if (!is.null(cube_data) && nrow(cube_data) > 0) {
        min_acc <- min(cube_data$accessibility, na.rm = TRUE)
        max_acc <- max(cube_data$accessibility, na.rm = TRUE)
        if (min_acc == max_acc) max_acc <- min_acc + 1
        
        # Save bounds for slicing
        bbox <- sf::st_bbox(cube_data)
        mapgl_cube_bounds(list(
          x = c(bbox["xmin"], bbox["xmax"]),
          y = c(bbox["ymin"], bbox["ymax"])
        ))

        mapgl::add_fill_extrusion_layer(
          proxy,
          id = "spacetime_cube_layer",
          source = cube_data,
          fill_extrusion_base = mapgl::get_column("base_height"),
          fill_extrusion_height = mapgl::get_column("top_height"),
          fill_extrusion_color = mapgl::interpolate(
            column = "accessibility",
            values = seq(min_acc, max_acc, length.out = 5),
            stops = c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"),
            na_color = "#cccccc"
          ),
          fill_extrusion_opacity = 0.85
        )
        
        # Adjust map pitch/bearing to show 3D effectively
        mapgl::ease_to(proxy, center = origin_coords, pitch = 60, bearing = -20, duration = 1000)
      } else {
        shiny::showNotification("No accessibility data found for the origin.", type = "warning")
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
      render_itinerary_table(res$res)
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

    # --- 3D SPACE-TIME CUBE PLOTLY RENDERERS ---

    cube_bounds <- shiny::reactiveVal(NULL)
    mapgl_cube_bounds <- shiny::reactiveVal(NULL)

    shiny::observeEvent(list(input$map_drawn_features, input$slice_z, mapgl_3d_mode_val()), {
      # Handle MapGL slicing via drawn brush
      if (isTRUE(mapgl_3d_mode_val())) {
        map_bounds <- mapgl_cube_bounds()
        if (!is.null(map_bounds)) {
          # Default to no spatial bounds filter
          has_spatial_filter <- FALSE
          
          # Overwrite with drawn features if present
          if (!is.null(input$map_drawn_features) && length(input$map_drawn_features$features) > 0) {
            tryCatch({
              coords <- unlist(lapply(input$map_drawn_features$features, function(f) f$geometry$coordinates))
              lons <- coords[c(TRUE, FALSE)]
              lats <- coords[c(FALSE, TRUE)]
              if (length(lons) > 0 && length(lats) > 0) {
                map_x_range <- c(min(lons), max(lons))
                map_y_range <- c(min(lats), max(lats))
                has_spatial_filter <- TRUE
              }
            }, error = function(e) NULL)
          }

          z_range <- input$slice_z
          
          if (has_spatial_filter) {
            filter_expr <- list("all",
                 list(">=", "lon", map_x_range[1]),
                 list("<=", "lon", map_x_range[2]),
                 list(">=", "lat", map_y_range[1]),
                 list("<=", "lat", map_y_range[2]),
                 list(">=", "hour", z_range[1]),
                 list("<=", "hour", z_range[2])
            )
          } else {
            filter_expr <- list("all",
                 list(">=", "hour", z_range[1]),
                 list("<=", "hour", z_range[2])
            )
          }
          
          proxy <- mapgl::maplibre_proxy("map")
          mapgl::set_filter(proxy, "spacetime_cube_layer", filter_expr)
        }
      }
    })

    shiny::observeEvent(list(input$slice_x, input$slice_y, input$slice_z), {
      bounds <- cube_bounds()
      if (!is.null(bounds) && isTRUE(cube_mode())) {
        x_range <- bounds$x[1] + (bounds$x[2] - bounds$x[1]) * (input$slice_x / 100)
        y_range <- bounds$y[1] + (bounds$y[2] - bounds$y[1]) * (input$slice_y / 100)
        z_range <- input$slice_z
  
        restyle_update <- list(
          "transforms[0].value" = x_range[1],
          "transforms[1].value" = x_range[2],
          "transforms[2].value" = y_range[1],
          "transforms[3].value" = y_range[2],
          "transforms[4].value" = z_range[1],
          "transforms[5].value" = z_range[2]
        )
  
        if (compare_mode()) {
          plotly::plotlyProxy("space_time_cube_1", session) |>
            plotly::plotlyProxyInvoke("restyle", restyle_update)
          plotly::plotlyProxy("space_time_cube_2", session) |>
            plotly::plotlyProxyInvoke("restyle", restyle_update)
        } else {
          plotly::plotlyProxy("space_time_cube", session) |>
            plotly::plotlyProxyInvoke("restyle", restyle_update)
        }
      }
    }, ignoreInit = TRUE)

    output$space_time_cube <- plotly::renderPlotly({
      shiny::req(input$show_cube)

      res <- tryCatch({
        route_data()
      }, error = function(e) {
        NULL
      })

      net <- if (!is.null(input$network_1_internal) && input$network_1_internal %in% names(networks)) {
        networks[[input$network_1_internal]]
      } else {
        networks[[1]]
      }

      if (!is.null(res)) {
        p <- plot_space_time_cube(
          itinerary = res$res,
          r5r_network = net,
          isochrones_sf = res$iso,
          isochrones = !is.null(res$iso),
          date = input$departure_date_1_internal,
          departure_time = input$departure_time_1_internal,
          accessibility_surface = isTRUE(input$show_3d_access_surface),
          access_variable = if (!is.null(input$access_variable_3d)) input$access_variable_3d else "jobs",
          access_cutoff = if (!is.null(input$access_cutoff_3d)) as.numeric(input$access_cutoff_3d) else 30,
          access_metric = if (!is.null(input$access_metric_3d)) input$access_metric_3d else "cumulative",
          access_n_closest = if (!is.null(input$access_n_closest_3d)) as.integer(input$access_n_closest_3d) else 1L
        )
      } else {
        shiny::req(locations$start)
        p <- plot_space_time_cube(
          itinerary = NULL,
          origin_coords = c(locations$start$lon, locations$start$lat),
          r5r_network = net,
          date = input$departure_date_1_internal,
          departure_time = input$departure_time_1_internal,
          accessibility_surface = isTRUE(input$show_3d_access_surface),
          access_variable = if (!is.null(input$access_variable_3d)) input$access_variable_3d else "jobs",
          access_cutoff = if (!is.null(input$access_cutoff_3d)) as.numeric(input$access_cutoff_3d) else 30,
          access_metric = if (!is.null(input$access_metric_3d)) input$access_metric_3d else "cumulative",
          access_n_closest = if (!is.null(input$access_n_closest_3d)) as.integer(input$access_n_closest_3d) else 1L
        )
      }
      
      if (!is.null(p$x$custom_bounds)) {
        cube_bounds(p$x$custom_bounds)
      }
      p
    })

    output$space_time_cube_1 <- plotly::renderPlotly({
      res <- route_data()
      shiny::req(!is.null(res))
      shiny::req(compare_mode())
      shiny::req(input$show_cube)

      net <- if (!is.null(input$network_1_internal) && input$network_1_internal %in% names(networks)) {
        networks[[input$network_1_internal]]
      } else {
        networks[[1]]
      }

      p <- plot_space_time_cube(
        itinerary = res$res1,
        r5r_network = net,
        isochrones_sf = res$iso1,
        isochrones = !is.null(res$iso1),
        date = input$departure_date_1_internal,
        accessibility_surface = isTRUE(input$show_3d_access_surface),
        access_variable = if (!is.null(input$access_variable_3d)) input$access_variable_3d else "jobs",
        access_cutoff = if (!is.null(input$access_cutoff_3d)) as.numeric(input$access_cutoff_3d) else 30,
        access_metric = if (!is.null(input$access_metric_3d)) input$access_metric_3d else "cumulative",
        access_n_closest = if (!is.null(input$access_n_closest_3d)) as.integer(input$access_n_closest_3d) else 1L
      )
      
      if (!is.null(p$x$custom_bounds)) {
        cube_bounds(p$x$custom_bounds)
      }
      p
    })

    output$space_time_cube_2 <- plotly::renderPlotly({
      res <- route_data()
      shiny::req(!is.null(res))
      shiny::req(compare_mode())
      shiny::req(input$show_cube)

      net <- if (!is.null(input$network_2_internal) && input$network_2_internal %in% names(networks)) {
        networks[[input$network_2_internal]]
      } else {
        networks[[1]]
      }

      p <- plot_space_time_cube(
        itinerary = res$res2,
        r5r_network = net,
        isochrones_sf = res$iso2,
        isochrones = !is.null(res$iso2),
        date = input$departure_date_2_internal,
        accessibility_surface = isTRUE(input$show_3d_access_surface),
        access_variable = if (!is.null(input$access_variable_3d)) input$access_variable_3d else "jobs",
        access_cutoff = if (!is.null(input$access_cutoff_3d)) as.numeric(input$access_cutoff_3d) else 30,
        access_metric = if (!is.null(input$access_metric_3d)) input$access_metric_3d else "cumulative",
        access_n_closest = if (!is.null(input$access_n_closest_3d)) as.integer(input$access_n_closest_3d) else 1L
      )
      
      if (!is.null(p$x$custom_bounds)) {
        cube_bounds(p$x$custom_bounds)
      }
      p
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
        if (!has_multiple_networks) {
          return(network_object_name_for_code)
        }
        # If we have multiple networks, we assume the user has a list object named 'r5r_network' (or whatever passed name)
        # and we access it by name.
        # However, for demo mode, we might need special handling if we want to show 'list(...)' setup?
        # For simplicity, we assume the user has the list object in their environment.
        if (is.null(net_id)) {
          return(paste0(network_object_name_for_code, "[[1]]"))
        }
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
