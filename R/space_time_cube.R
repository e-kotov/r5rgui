#' Get UTM EPSG code for a given longitude and latitude
#'
#' @param lon Longitude
#' @param lat Latitude
#' @return Integer EPSG code
#' @noRd
get_utm_epsg <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  if (lat >= 0) {
    32600 + zone
  } else {
    32700 + zone
  }
}

#' Convert time object (POSIXct, POSIXlt or HH:MM:SS string) to decimal hours of the day
#'
#' @param time Object representing time
#' @return Numeric decimal hours
#' @noRd
to_decimal_hours <- function(time) {
  if (inherits(time, "POSIXt")) {
    time <- format(time, "%H:%M:%S")
  }
  time <- as.character(time)
  parts <- strsplit(time, ":")
  sapply(parts, function(x) {
    if (length(x) >= 2) {
      h <- as.numeric(x[1])
      m <- as.numeric(x[2])
      s <- if (length(x) >= 3) as.numeric(x[3]) else 0
      h + m/60 + s/3600
    } else {
      NA_real_
    }
  })
}

#' Check if a mode is a transit mode
#'
#' @param mode Mode string
#' @return Logical
#' @noRd
is_transit_mode <- function(mode) {
  if (is.null(mode) || is.na(mode) || length(mode) == 0) return(FALSE)
  mode <- toupper(mode)
  mode %in% c("BUS", "RAIL", "SUBWAY", "TRAM", "TRAMWAY", "FERRY", "CABLE_CAR", "GONDOLA", "FUNICULAR", "TRANSIT")
}

#' Calculate Isochrones for Route Transfer/Dwell Locations
#'
#' This function identifies transfer locations (where the wait time before a leg is > 0),
#' transit alighting locations (where transitioning from a transit mode to WALK),
#' as well as the initial origin and final destination of the trip,
#' and calculates travel-time isochrones from those locations.
#'
#' @param itinerary An `sf` object returned by `r5r::detailed_itineraries()`.
#' @param r5r_network A `TransportNetwork` object (from `r5r`).
#' @param cutoffs Numeric vector of travel time cutoffs (in minutes) for isochrones (defaults to `15`).
#' @param mode Character vector of modes to calculate isochrones (defaults to `"WALK"`).
#' @param date Date object or character string (YYYY-MM-DD) specifying the date of the trip (defaults to `Sys.Date()`).
#'
#' @return An `sf` object containing combined isochrone polygons, or `NULL` if no isochrones were calculated.
#' @export
calculate_route_isochrones <- function(itinerary,
                                       r5r_network,
                                       cutoffs = 15,
                                       mode = "WALK",
                                       date = Sys.Date()) {
  if (is.null(itinerary) || nrow(itinerary) == 0 || is.null(r5r_network)) {
    return(NULL)
  }

  # Filter for the first option (option 1)
  if ("option" %in% names(itinerary)) {
    itinerary_opt <- itinerary[itinerary$option == 1, ]
  } else {
    itinerary_opt <- itinerary
  }

  if (nrow(itinerary_opt) == 0) {
    return(NULL)
  }

  # Sort by segment sequence
  if ("segment" %in% names(itinerary_opt)) {
    itinerary_opt <- itinerary_opt[order(itinerary_opt$segment), ]
  } else {
    itinerary_opt <- itinerary_opt[order(itinerary_opt$departure_time), ]
  }

  # Helper to parse HH:MM:SS or HH:MM string to POSIXct
  date_str <- as.character(date)
  to_posixct <- function(time_str) {
    if (inherits(time_str, "POSIXt")) return(time_str)
    time_str <- as.character(time_str)
    res <- as.POSIXct(paste(date_str, time_str), format = "%Y-%m-%d %H:%M:%S")
    if (all(is.na(res))) {
      res <- as.POSIXct(paste(date_str, time_str), format = "%Y-%m-%d %H:%M")
    }
    res
  }

  # Calculate leg arrival times
  trip_dep <- to_posixct(itinerary_opt$departure_time[1])
  arr_times <- list()
  
  current_time <- trip_dep
  for (i in seq_len(nrow(itinerary_opt))) {
    leg <- itinerary_opt[i, ]
    wait_time <- if ("wait" %in% names(leg)) as.numeric(leg$wait) else 0
    seg_dur <- if ("segment_duration" %in% names(leg)) as.numeric(leg$segment_duration) else if ("duration" %in% names(leg)) as.numeric(leg$duration) else 0
    
    current_time <- current_time + (wait_time * 60)
    current_time <- current_time + (seg_dur * 60)
    arr_times[[i]] <- current_time
  }

  targets <- list()

  # 1. Add Origin of the entire trip (at trip departure time)
  coords_1 <- sf::st_coordinates(itinerary_opt[1, ])
  targets[[length(targets) + 1]] <- list(
    id = "dwell_1",
    lon = coords_1[1, "X"],
    lat = coords_1[1, "Y"],
    time = trip_dep
  )

  # 2. Add Intermediate Transfers and Alighting points
  if (nrow(itinerary_opt) > 1) {
    for (i in 2:nrow(itinerary_opt)) {
      leg_prev <- itinerary_opt[i - 1, ]
      leg_curr <- itinerary_opt[i, ]
      
      wait_time <- if ("wait" %in% names(leg_curr)) as.numeric(leg_curr$wait) else 0
      mode_prev <- if ("mode" %in% names(leg_prev)) as.character(leg_prev$mode) else ""
      mode_curr <- if ("mode" %in% names(leg_curr)) as.character(leg_curr$mode) else ""
      
      is_boarding_or_transfer <- wait_time > 0
      is_alighting <- is_transit_mode(mode_prev) && (toupper(mode_curr) == "WALK")
      
      if (is_boarding_or_transfer || is_alighting) {
        coords_curr <- sf::st_coordinates(leg_curr)
        start_coord <- coords_curr[1, ]
        
        targets[[length(targets) + 1]] <- list(
          id = sprintf("dwell_%d", i),
          lon = start_coord["X"],
          lat = start_coord["Y"],
          time = arr_times[[i - 1]]
        )
      }
    }
  }

  # 3. Add Destination of the entire trip (at final arrival time)
  N <- nrow(itinerary_opt)
  coords_N <- sf::st_coordinates(itinerary_opt[N, ])
  targets[[length(targets) + 1]] <- list(
    id = sprintf("dwell_%d", N + 1),
    lon = coords_N[nrow(coords_N), "X"],
    lat = coords_N[nrow(coords_N), "Y"],
    time = arr_times[[N]]
  )

  isochrones_list <- list()

  # Run isochrones for all collected targets
  for (t in targets) {
    origin_df <- data.frame(id = t$id, lon = t$lon, lat = t$lat)
    
    iso_res <- tryCatch({
      if (utils::packageVersion("r5r") >= "2.3.0") {
        r5r::isochrone(
          r5r_network = r5r_network,
          origins = origin_df,
          mode = mode,
          cutoffs = cutoffs,
          departure_datetime = t$time,
          verbose = FALSE,
          progress = FALSE
        )
      } else {
        r5r::isochrone(
          r5r_core = r5r_network,
          origins = origin_df,
          mode = mode,
          cutoffs = cutoffs,
          departure_datetime = t$time,
          verbose = FALSE,
          progress = FALSE
        )
      }
    }, error = function(e) {
      warning("calculate_route_isochrones warning: ", e$message)
      NULL
    })
    
    if (!is.null(iso_res) && nrow(iso_res) > 0) {
      isochrones_list[[length(isochrones_list) + 1]] <- iso_res
    }
  }

  if (length(isochrones_list) == 0) {
    return(NULL)
  }

  # Combine all isochrones into one sf object
  combined_iso <- do.call(rbind, isochrones_list)
  
  # Ensure standard geometry column name
  sf::st_geometry(combined_iso) <- "geometry"
  combined_iso
}

#' Plot 3D Space-Time Cube for an Itinerary or a Single Origin
#'
#' This function visualizes travel trajectories as 3D paths in a Space-Time Cube.
#' If the `itinerary` argument is `NULL` but `origin_coords` is provided, the function
#' switches to displaying a single-origin accessibility cube containing a vertical
#' baseline at the origin and stacked accessibility surfaces at 15, 30, 45, and 60 minutes.
#'
#' @param itinerary Optional `sf` object returned by `r5r::detailed_itineraries()`.
#' @param r5r_network Optional `TransportNetwork` object (from `r5r`).
#' @param option Integer, which routing option from the itinerary to plot (defaults to 1).
#' @param isochrones Logical, whether to calculate and show isochrones (defaults to `TRUE` if `r5r_network` or `isochrones_sf` is provided).
#' @param isochrone_cutoffs Numeric vector of travel time cutoffs (in minutes) for isochrones (defaults to `15`).
#' @param isochrone_mode Character vector of modes to calculate isochrones (defaults to `"WALK"`).
#' @param date Date object or character string (YYYY-MM-DD) specifying the date of the trip (defaults to `Sys.Date()`).
#' @param departure_time Character string, departure time of the trip (HH:MM) (defaults to `"14:00"`).
#' @param isochrones_sf Optional pre-calculated `sf` object of isochrones.
#' @param accessibility_surface Logical, whether to compute and display accessibility surfaces (defaults to `TRUE` if `r5r_network` is provided).
#' @param access_variable Character string, which opportunity variable to visualize (defaults to `"jobs"`).
#' @param access_cutoff Numeric value, travel-time cutoff in minutes for accessibility calculations (defaults to `30`).
#' @param access_metric Character string, accessibility metric to compute (e.g. `"cumulative"`, `"gravity"`, `"cost_to_closest"`, `"balancing_cost"`, etc.).
#' @param access_n_closest Integer, number of closest opportunities to consider for `"cost_to_closest"` (defaults to `1`).
#' @param origin_coords Numeric vector of length 2 `c(lon, lat)` representing the origin coordinates when `itinerary = NULL`.
#'
#' @return A plotly object.
#' @export
plot_space_time_cube <- function(itinerary = NULL,
                                 r5r_network = NULL,
                                 option = 1,
                                 isochrones = !is.null(r5r_network) || !is.null(isochrones_sf),
                                 isochrone_cutoffs = 15,
                                 isochrone_mode = "WALK",
                                 date = Sys.Date(),
                                 departure_time = "14:00",
                                 isochrones_sf = NULL,
                                 accessibility_surface = !is.null(r5r_network),
                                 access_variable = "jobs",
                                 access_cutoff = 30,
                                 access_metric = "cumulative",
                                 access_n_closest = 1,
                                 origin_coords = NULL) {
  
  if (is.null(itinerary) && is.null(origin_coords)) {
    stop("plot_space_time_cube: both itinerary and origin_coords are NULL", call. = FALSE)
  }

  mode_colors <- c(
    "WALK" = "#1f77b4",     # Muted blue
    "BICYCLE" = "#2ca02c",  # Cooked green
    "CAR" = "#d62728",      # Crimson red
    "BUS" = "#ff7f0e",      # Safety orange
    "SUBWAY" = "#9467bd",   # Amethyst purple
    "RAIL" = "#8c564b",     # Sienna brown
    "TRAM" = "#e377c2",     # Orchid pink
    "TRANSIT" = "#7f7f7f"   # Slate gray
  )

  # Helper to parse HH:MM:SS or HH:MM string to POSIXct
  date_str <- as.character(date)
  to_posixct <- function(time_str) {
    if (inherits(time_str, "POSIXt")) return(time_str)
    time_str <- as.character(time_str)
    res <- as.POSIXct(paste(date_str, time_str), format = "%Y-%m-%d %H:%M:%S")
    if (all(is.na(res))) {
      res <- as.POSIXct(paste(date_str, time_str), format = "%Y-%m-%d %H:%M")
    }
    res
  }

  p <- plotly::plot_ly()

  if (!is.null(itinerary)) {
    # Filter for the selected option
    if ("option" %in% names(itinerary)) {
      itinerary_opt <- itinerary[itinerary$option == option, ]
    } else {
      itinerary_opt <- itinerary
    }

    if (nrow(itinerary_opt) == 0) {
      stop(sprintf("plot_space_time_cube: no rows found for option %d", option), call. = FALSE)
    }

    # Sort by segment sequence
    if ("segment" %in% names(itinerary_opt)) {
      itinerary_opt <- itinerary_opt[order(itinerary_opt$segment), ]
    } else {
      itinerary_opt <- itinerary_opt[order(itinerary_opt$departure_time), ]
    }

    # Calculate center for projection
    centroid <- sf::st_centroid(sf::st_union(itinerary_opt))
    center_coords <- sf::st_coordinates(centroid)
    center_lon <- center_coords[1, "X"]
    center_lat <- center_coords[1, "Y"]
    epsg <- get_utm_epsg(center_lon, center_lat)

    # Project center and itinerary
    center_proj <- sf::st_transform(sf::st_sfc(sf::st_point(c(center_lon, center_lat)), crs = 4326), epsg)
    center_xy <- sf::st_coordinates(center_proj)[1, c("X", "Y")]
    itinerary_proj <- sf::st_transform(itinerary_opt, epsg)

    # Calculate segment-specific departure and arrival times cumulatively
    trip_dep <- to_posixct(itinerary_proj$departure_time[1])
    
    dep_times <- vector("list", nrow(itinerary_proj))
    arr_times <- vector("list", nrow(itinerary_proj))
    
    current_time <- trip_dep
    for (i in seq_len(nrow(itinerary_proj))) {
      leg <- itinerary_proj[i, ]
      wait_time <- if ("wait" %in% names(leg)) as.numeric(leg$wait) else 0
      seg_dur <- if ("segment_duration" %in% names(leg)) as.numeric(leg$segment_duration) else if ("duration" %in% names(leg)) as.numeric(leg$duration) else 0
      
      # Wait before segment
      current_time <- current_time + (wait_time * 60)
      dep_times[[i]] <- current_time
      
      # Duration of segment
      current_time <- current_time + (seg_dur * 60)
      arr_times[[i]] <- current_time
    }
    
    itinerary_proj$leg_departure_time <- do.call(c, dep_times)
    itinerary_proj$leg_arrival_time <- do.call(c, arr_times)

    # 1. Build trip trajectory points
    trip_points <- list()
    dwell_segments <- list()
    dwell_locations <- list()

    for (i in seq_len(nrow(itinerary_proj))) {
      leg <- itinerary_proj[i, ]
      coords <- sf::st_coordinates(leg)
      
      # Project coordinates to local km relative to center
      x <- (coords[, "X"] - center_xy["X"]) / 1000
      y <- (coords[, "Y"] - center_xy["Y"]) / 1000
      
      # Calculate cumulative distance along leg
      dists <- sqrt(diff(x)^2 + diff(y)^2)
      cum_dists <- c(0, cumsum(dists))
      total_dist <- cum_dists[length(cum_dists)]
      
      # Interpolate time using segment-specific times
      t_start <- to_decimal_hours(leg$leg_departure_time)
      t_end <- to_decimal_hours(leg$leg_arrival_time)
      
      if (total_dist > 0) {
        z <- t_start + (cum_dists / total_dist) * (t_end - t_start)
      } else {
        z <- seq(t_start, t_end, length.out = nrow(coords))
      }
      
      mode_label <- if ("mode" %in% names(leg)) as.character(leg$mode) else "TRAVEL"
      
      trip_points[[i]] <- data.frame(
        x = x,
        y = y,
        z = z,
        mode = mode_label,
        type = "trip",
        leg_index = i,
        stringsAsFactors = FALSE
      )
    }

    # 2. Identify isochrone locations (dwell/wait periods + alighting points + origin + destination)
    N <- nrow(itinerary_proj)

    # 2a. Add Origin of the entire trip (at trip departure time)
    coords_1 <- sf::st_coordinates(itinerary_proj[1, ])
    x_orig <- (coords_1[1, "X"] - center_xy["X"]) / 1000
    y_orig <- (coords_1[1, "Y"] - center_xy["Y"]) / 1000
    z_orig <- to_decimal_hours(trip_dep)

    coords_wgs84_orig <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(coords_1[1, c("X", "Y")]), crs = epsg), 4326))
    dwell_locations[["1"]] <- list(
      lon = coords_wgs84_orig[1, "X"],
      lat = coords_wgs84_orig[1, "Y"],
      arrival_time = trip_dep,
      departure_time = trip_dep,
      x = x_orig,
      y = y_orig,
      z_start = z_orig,
      z_end = z_orig
    )

    # 2b. Add Intermediate Transfers/Alighting points
    if (N > 1) {
      for (i in 2:N) {
        leg_prev <- itinerary_proj[i - 1, ]
        leg_curr <- itinerary_proj[i, ]
        
        wait_time <- if ("wait" %in% names(leg_curr)) as.numeric(leg_curr$wait) else 0
        mode_prev <- if ("mode" %in% names(leg_prev)) as.character(leg_prev$mode) else ""
        mode_curr <- if ("mode" %in% names(leg_curr)) as.character(leg_curr$mode) else ""
        
        is_boarding_or_transfer <- wait_time > 0
        is_alighting <- is_transit_mode(mode_prev) && (toupper(mode_curr) == "WALK")
        
        if (is_boarding_or_transfer || is_alighting) {
          coords_curr <- sf::st_coordinates(leg_curr)
          start_coord <- coords_curr[1, ]
          
          x_dwell <- (start_coord["X"] - center_xy["X"]) / 1000
          y_dwell <- (start_coord["Y"] - center_xy["Y"]) / 1000
          z_start <- to_decimal_hours(leg_prev$leg_arrival_time)
          z_end <- to_decimal_hours(leg_curr$leg_departure_time)
          
          if (is_boarding_or_transfer) {
            dwell_segments[[length(dwell_segments) + 1]] <- data.frame(
              x = c(x_dwell, x_dwell),
              y = c(y_dwell, y_dwell),
              z = c(z_start, z_end),
              mode = "WAIT",
              type = "dwell",
              leg_index = i - 1,
              stringsAsFactors = FALSE
            )
          }
          
          coords_wgs84 <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(start_coord[c("X", "Y")]), crs = epsg), 4326))
          dwell_locations[[as.character(i)]] <- list(
            lon = coords_wgs84[1, "X"],
            lat = coords_wgs84[1, "Y"],
            arrival_time = leg_prev$leg_arrival_time,
            departure_time = leg_curr$leg_departure_time,
            x = x_dwell,
            y = y_dwell,
            z_start = z_start,
            z_end = z_end
          )
        }
      }
    }

    # 2c. Add Destination of the entire trip (at final arrival time)
    coords_N <- sf::st_coordinates(itinerary_proj[N, ])
    x_dest <- (coords_N[nrow(coords_N), "X"] - center_xy["X"]) / 1000
    y_dest <- (coords_N[nrow(coords_N), "Y"] - center_xy["Y"]) / 1000
    z_dest <- to_decimal_hours(itinerary_proj$leg_arrival_time[N])

    coords_wgs84_dest <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(coords_N[nrow(coords_N), c("X", "Y")]), crs = epsg), 4326))
    dwell_locations[[as.character(N + 1)]] <- list(
      lon = coords_wgs84_dest[1, "X"],
      lat = coords_wgs84_dest[1, "Y"],
      arrival_time = itinerary_proj$leg_arrival_time[N],
      departure_time = itinerary_proj$leg_arrival_time[N],
      x = x_dest,
      y = y_dest,
      z_start = z_dest,
      z_end = z_dest
    )

    trajectory_data <- do.call(rbind, trip_points)
    
    # Connect segments with NAs in plotly to prevent drawing diagonals
    trajectory_plotly <- data.frame()
    for (i in seq_along(trip_points)) {
      trajectory_plotly <- rbind(trajectory_plotly, trip_points[[i]], data.frame(x = NA, y = NA, z = NA, mode = NA, type = NA, leg_index = NA))
    }

    # Draw trips
    unique_modes <- unique(trajectory_data$mode)
    for (m in unique_modes) {
      m_data <- trajectory_plotly[trajectory_plotly$mode == m & !is.na(trajectory_plotly$mode), ]
      if (nrow(m_data) == 0) next
      
      m_indices <- which(trajectory_plotly$mode == m | is.na(trajectory_plotly$mode))
      if (length(m_indices) > 0) {
        m_plotly_data <- trajectory_plotly[m_indices, ]
        color <- if (m %in% names(mode_colors)) mode_colors[m] else "#777777"
        
        p <- p |> plotly::add_trace(
          data = m_plotly_data,
          x = ~x, y = ~y, z = ~z,
          type = "scatter3d", mode = "lines",
          name = tolower(m),
          line = list(color = color, width = 6),
          hovertemplate = paste0("Mode: ", m, "<br>x: %{x:.2f} km<br>y: %{y:.2f} km<br>Time: %{z:.2f}h<extra></extra>"),
          showlegend = TRUE
        )
      }
    }

    # Draw dwell vertical lines
    if (length(dwell_segments) > 0) {
      dwell_data <- do.call(rbind, lapply(dwell_segments, function(df) {
        rbind(df, data.frame(x = NA, y = NA, z = NA, mode = NA, type = NA, leg_index = NA))
      }))
      
      p <- p |> plotly::add_trace(
        data = dwell_data,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "lines",
        name = "wait/transfer",
        line = list(color = "#ff7c43", width = 5, dash = "dash"),
        hovertemplate = "Waiting<br>x: %{x:.2f} km<br>y: %{y:.2f} km<br>Time: %{z:.2f}h<extra></extra>",
        showlegend = FALSE
      )
    }

    # Draw start and end markers
    start_pt <- trip_points[[1]][1, ]
    end_pt_leg <- trip_points[[length(trip_points)]]
    end_pt <- end_pt_leg[nrow(end_pt_leg), ]
    
    p <- p |> plotly::add_trace(
      x = c(start_pt$x, end_pt$x),
      y = c(start_pt$y, end_pt$y),
      z = c(start_pt$z, end_pt$z),
      type = "scatter3d", mode = "markers",
      name = "endpoints",
      marker = list(
        size = 6,
        color = c("#009E73", "#D55E00"),
        symbol = "circle"
      ),
      hovertemplate = c("Start<br>x: %{x:.2f} km<br>y: %{y:.2f} km<br>Time: %{z:.2f}h<extra></extra>",
                        "End<br>x: %{x:.2f} km<br>y: %{y:.2f} km<br>Time: %{z:.2f}h<extra></extra>"),
      showlegend = FALSE
    )

    # Draw shadow (2D footprints on the bottom plane)
    min_z <- min(trajectory_data$z, na.rm = TRUE)
    max_z <- max(trajectory_data$z, na.rm = TRUE)
    bottom_z <- min_z - 0.1
    
    shadow_plotly <- trajectory_plotly
    shadow_plotly$z[!is.na(shadow_plotly$z)] <- bottom_z
    
    p <- p |> plotly::add_trace(
      data = shadow_plotly,
      x = ~x, y = ~y, z = ~z,
      type = "scatter3d", mode = "lines",
      name = "route footprint",
      line = list(color = "#cccccc", width = 3, dash = "solid"),
      hoverinfo = "skip",
      showlegend = FALSE
    )

    all_raster_coords_x <- x_orig
    all_raster_coords_y <- y_orig

  } else {
    # Single-point origin mode
    center_lon <- origin_coords[1]
    center_lat <- origin_coords[2]
    epsg <- get_utm_epsg(center_lon, center_lat)

    center_proj <- sf::st_transform(sf::st_sfc(sf::st_point(c(center_lon, center_lat)), crs = 4326), epsg)
    center_xy <- sf::st_coordinates(center_proj)[1, c("X", "Y")]

    # Use a sequence of departure hours over the day (0:00 to 23:00)
    dep_hours <- 0:23
    min_z <- min(dep_hours)
    max_z <- max(dep_hours)
    bottom_z <- min_z - 0.5

    # Draw vertical baseline at origin representing the start location
    p <- p |> plotly::add_trace(
      x = c(0, 0),
      y = c(0, 0),
      z = c(min_z, max_z),
      type = "scatter3d", mode = "lines+markers",
      name = "origin baseline",
      line = list(color = "#009E73", width = 6),
      marker = list(size = 4, color = "#009E73"),
      hovertemplate = "Origin Location<br>x: 0 km<br>y: 0 km<br>Time: %{z:.2f}h<extra></extra>",
      showlegend = TRUE
    )

    # Draw a small shadow marker for the origin on the floor
    p <- p |> plotly::add_trace(
      x = c(0),
      y = c(0),
      z = c(bottom_z),
      type = "scatter3d", mode = "markers",
      name = "origin footprint",
      marker = list(color = "#cccccc", size = 6),
      hoverinfo = "skip",
      showlegend = FALSE
    )

    all_raster_coords_x <- 0
    all_raster_coords_y <- 0

    # Build accessibility planes mapping for varying departure times
    dwell_locations <- list()
    for (h in dep_hours) {
      hour_str <- sprintf("%02d:00:00", h)
      dep_dt <- to_posixct(hour_str)
      dwell_locations[[as.character(h)]] <- list(
        lon = center_lon, 
        lat = center_lat, 
        arrival_time = dep_dt, 
        z_start = as.numeric(h), 
        cutoff_val = access_cutoff
      )
    }
  }

  # 3. Calculate and draw accessibility raster planes at each dwell location if requested
  if (accessibility_surface && !is.null(r5r_network) && length(dwell_locations) > 0) {
    poa_grid_file <- system.file("extdata/poa/poa_hexgrid.csv", package = "r5r")
    poa_grid <- if (file.exists(poa_grid_file)) read.csv(poa_grid_file) else NULL

    if (!is.null(poa_grid) && access_variable %in% names(poa_grid)) {
      # Replace NA values with 0
      poa_grid[[access_variable]][is.na(poa_grid[[access_variable]])] <- 0

      # Determine demand variable (different from opportunity)
      demand_var <- if (access_variable == "population") "jobs" else "population"
      if (demand_var %in% names(poa_grid)) {
        poa_grid[[demand_var]][is.na(poa_grid[[demand_var]])] <- 0
      }

      # Legend label
      var_label <- switch(access_variable,
        "jobs" = "Jobs",
        "schools" = "Schools",
        "healthcare" = "Healthcare",
        "population" = "Population",
        access_variable
      )
      
      colorbar_title <- switch(access_metric,
        "cumulative" = sprintf("Reachable %s", var_label),
        "cumulative_interval" = sprintf("Interval Reachable %s", var_label),
        "gravity" = sprintf("Gravity Index (%s)", var_label),
        "cost_to_closest" = sprintf("Min Time to %d %s (min)", access_n_closest, var_label),
        "floating_catchment" = sprintf("FCA Index (%s)", var_label),
        "spatial_availability" = sprintf("Spatial Availability (%s)", var_label),
        "balancing_cost" = sprintf("Balancing Cost (min)"),
        sprintf("Accessibility (%s)", var_label)
      )

      # Pass 1: Compute all matrices and scores to establish global min & max
      all_scores <- numeric()
      results_list <- list()

      for (dwell_key in names(dwell_locations)) {
        dwell <- dwell_locations[[dwell_key]]
        orig_df <- data.frame(id = "start", lon = dwell$lon, lat = dwell$lat)

        ttm_start <- tryCatch({
          if (utils::packageVersion("r5r") >= "2.3.0") {
            r5r::travel_time_matrix(
              r5r_network = r5r_network,
              origins = orig_df,
              destinations = poa_grid,
              mode = isochrone_mode,
              departure_datetime = dwell$arrival_time,
              max_trip_duration = 60,
              verbose = FALSE,
              progress = FALSE
            )
          } else {
            r5r::travel_time_matrix(
              r5r_core = r5r_network,
              origins = orig_df,
              destinations = poa_grid,
              mode = isochrone_mode,
              departure_datetime = dwell$arrival_time,
              max_trip_duration = 60,
              verbose = FALSE,
              progress = FALSE
            )
          }
        }, error = function(e) {
          NULL
        })

        if (!is.null(ttm_start) && nrow(ttm_start) > 0) {
          start_tt_col <- intersect(names(ttm_start), c("travel_time_p50", "travel_time"))[1]
          names(ttm_start)[names(ttm_start) == start_tt_col] <- "travel_time"
          
          # Get subset of grid cells reachable from this stop
          reachable_ids <- ttm_start$to_id
          reachable_grid <- poa_grid[poa_grid$id %in% reachable_ids, ]
          
          if (nrow(reachable_grid) > 0) {
            # Travel times from reachable cells within access limits
            current_cutoff <- if (!is.null(dwell$cutoff_val)) dwell$cutoff_val else access_cutoff
            max_duration <- if (access_metric %in% c("cost_to_closest", "balancing_cost")) 90 else current_cutoff
            
            ttm_all <- tryCatch({
              if (utils::packageVersion("r5r") >= "2.3.0") {
                r5r::travel_time_matrix(
                  r5r_network = r5r_network,
                  origins = reachable_grid,
                  destinations = poa_grid,
                  mode = isochrone_mode,
                  departure_datetime = dwell$arrival_time,
                  max_trip_duration = max_duration,
                  verbose = FALSE,
                  progress = FALSE
                )
              } else {
                r5r::travel_time_matrix(
                  r5r_core = r5r_network,
                  origins = reachable_grid,
                  destinations = poa_grid,
                  mode = isochrone_mode,
                  departure_datetime = dwell$arrival_time,
                  max_trip_duration = max_duration,
                  verbose = FALSE,
                  progress = FALSE
                )
              }
            }, error = function(e) {
              NULL
            })
            
            if (!is.null(ttm_all) && nrow(ttm_all) > 0) {
              all_tt_col <- intersect(names(ttm_all), c("travel_time_p50", "travel_time"))[1]
              names(ttm_all)[names(ttm_all) == all_tt_col] <- "travel_time"
              
              acc_res <- tryCatch({
                if (access_metric == "cumulative") {
                  accessibility::cumulative_cutoff(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    cutoff = current_cutoff,
                    fill_missing_ids = TRUE
                  )
                } else if (access_metric == "cumulative_interval") {
                  accessibility::cumulative_interval(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    interval = c(max(5, current_cutoff - 10), current_cutoff)
                  )
                } else if (access_metric == "gravity") {
                  accessibility::gravity(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    decay_function = accessibility::decay_exponential(decay_value = 0.1),
                    fill_missing_ids = TRUE
                  )
                } else if (access_metric == "cost_to_closest") {
                  accessibility::cost_to_closest(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    n = access_n_closest,
                    fill_missing_ids = TRUE
                  )
                } else if (access_metric == "floating_catchment") {
                  accessibility::floating_catchment_area(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    demand = demand_var,
                    method = "2sfca",
                    decay_function = accessibility::decay_binary(cutoff = current_cutoff),
                    fill_missing_ids = TRUE
                  )
                } else if (access_metric == "spatial_availability") {
                  accessibility::spatial_availability(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    demand = demand_var,
                    decay_function = accessibility::decay_exponential(decay_value = 0.1),
                    fill_missing_ids = TRUE
                  )
                } else if (access_metric == "balancing_cost") {
                  accessibility::balancing_cost(
                    travel_matrix = ttm_all,
                    land_use_data = poa_grid,
                    opportunity = access_variable,
                    travel_cost = "travel_time",
                    demand = demand_var,
                    fill_missing_ids = TRUE
                  )
                } else {
                  NULL
                }
              }, error = function(e) {
                NULL
              })
              
              if (!is.null(acc_res) && nrow(acc_res) > 0) {
                score_col <- if (access_metric %in% c("cost_to_closest", "balancing_cost")) {
                  "travel_time"
                } else {
                  access_variable
                }

                if (score_col %in% names(acc_res)) {
                  names(acc_res)[names(acc_res) == score_col] <- "accessibility"
                  acc_res$accessibility[is.na(acc_res$accessibility)] <- 0
                  
                  merged_grid <- merge(reachable_grid, acc_res, by = "id")
                  all_scores <- c(all_scores, merged_grid$accessibility)
                  results_list[[dwell_key]] <- merged_grid
                }
              }
            }
          }
        }
      }

      # Pass 2: Plot the meshes with a single, shared colorbar and color scale limits
      if (length(results_list) > 0 && length(all_scores) > 0) {
        global_min <- min(all_scores, na.rm = TRUE)
        global_max <- max(all_scores, na.rm = TRUE)
        
        if (global_min == global_max) {
          global_max <- global_min + 1
        }

        if (is.null(itinerary)) {
          # Render as a true 3D volume (solid cube)
          vol_x <- numeric()
          vol_y <- numeric()
          vol_z <- numeric()
          vol_val <- numeric()
          
          dwell_keys <- names(results_list)
          
          # Create a perfect rectangular 2D grid for the volume trace
          grid_res <- 30
          merged_grid_sample <- results_list[[dwell_keys[1]]]
          grid_sf <- sf::st_as_sf(merged_grid_sample, coords = c("lon", "lat"), crs = 4326)
          grid_proj <- sf::st_transform(grid_sf, epsg)
          grid_coords <- sf::st_coordinates(grid_proj)
          
          x_min <- min(grid_coords[, "X"])
          x_max <- max(grid_coords[, "X"])
          y_min <- min(grid_coords[, "Y"])
          y_max <- max(grid_coords[, "Y"])
          
          x_seq <- seq(x_min, x_max, length.out = grid_res)
          y_seq <- seq(y_min, y_max, length.out = grid_res)
          grid_2d <- expand.grid(X = x_seq, Y = y_seq)
          grid_2d_sf <- sf::st_as_sf(grid_2d, coords = c("X", "Y"), crs = epsg)
          
          # Pre-compute nearest feature and distance mask for the grid
          nearest_idx <- sf::st_nearest_feature(grid_2d_sf, grid_proj)
          dists <- sf::st_distance(grid_2d_sf, grid_proj[nearest_idx, ], by_element = TRUE)
          valid_mask <- as.numeric(dists) <= 1500 # 1.5km max distance from hex center
          
          grid_2d_x <- (grid_2d$X - center_xy["X"]) / 1000
          grid_2d_y <- (grid_2d$Y - center_xy["Y"]) / 1000
          
          # value to represent empty space (safely outside isomin)
          empty_val <- global_min - abs(global_max - global_min) - 10 
          
          for (i in seq_along(dwell_keys)) {
            dwell_key <- dwell_keys[i]
            merged_grid <- results_list[[dwell_key]]
            dwell <- dwell_locations[[dwell_key]]
            
            z_grid <- rep(dwell$z_start, length(grid_2d_x))
            
            # Map values using nearest neighbor, mask out distant points
            val <- merged_grid$accessibility[nearest_idx]
            val[!valid_mask] <- empty_val
            # Also mask NAs
            val[is.na(val)] <- empty_val
            
            vol_x <- c(vol_x, grid_2d_x)
            vol_y <- c(vol_y, grid_2d_y)
            vol_z <- c(vol_z, z_grid)
            vol_val <- c(vol_val, val)
            
            # Update overall raster coordinates to influence bounds correctly
            all_raster_coords_x <- c(all_raster_coords_x, grid_2d_x[valid_mask])
            all_raster_coords_y <- c(all_raster_coords_y, grid_2d_y[valid_mask])
          }
          
          p <- p |> plotly::add_trace(
            x = vol_x,
            y = vol_y,
            z = vol_z,
            value = vol_val,
            type = "volume",
            isomin = global_min,
            isomax = global_max,
            opacity = 0.1,
            surface = list(count = 15),
            transforms = list(
              list(type = "filter", target = "x", operation = ">=", value = -99999),
              list(type = "filter", target = "x", operation = "<=", value = 99999),
              list(type = "filter", target = "y", operation = ">=", value = -99999),
              list(type = "filter", target = "y", operation = "<=", value = 99999),
              list(type = "filter", target = "z", operation = ">=", value = -99999),
              list(type = "filter", target = "z", operation = "<=", value = 99999)
            ),
            slices = list(
              x = list(show = TRUE),
              y = list(show = TRUE),
              z = list(show = TRUE)
            ),
            colorscale = "Viridis",
            showscale = TRUE,
            colorbar = list(title = colorbar_title, len = 0.5, y = 0.3),
            name = "Accessibility Volume",
            hoverinfo = "text",
            text = sprintf("Accessibility: %s (%s)", prettyNum(round(vol_val, 2), big.mark = ","), tolower(var_label)),
            showlegend = FALSE
          )
        } else {
          dwell_keys <- names(results_list)
          for (i in seq_along(dwell_keys)) {
            dwell_key <- dwell_keys[i]
            merged_grid <- results_list[[dwell_key]]
            dwell <- dwell_locations[[dwell_key]]
            
            # Project grid coordinates
            grid_sf <- sf::st_as_sf(merged_grid, coords = c("lon", "lat"), crs = 4326)
            grid_proj <- sf::st_transform(grid_sf, epsg)
            grid_coords <- sf::st_coordinates(grid_proj)
            
            x_grid <- (grid_coords[, "X"] - center_xy["X"]) / 1000
            y_grid <- (grid_coords[, "Y"] - center_xy["Y"]) / 1000
            z_grid <- rep(dwell$z_start, length(x_grid))
            
            # Collect coordinate ranges for plot bounding box
            all_raster_coords_x <- c(all_raster_coords_x, x_grid)
            all_raster_coords_y <- c(all_raster_coords_y, y_grid)
            
            if (dwell_key == "1") {
              stop_label <- "Origin"
            } else if (dwell_key == as.character(N + 1)) {
              stop_label <- "Destination"
            } else {
              stop_label <- sprintf("Stop %s", dwell_key)
            }
            
            # Only show scale/colorbar on the first stop's trace to avoid double-vision layering
            show_colorbar <- (i == 1)
            
            p <- p |> plotly::add_trace(
              x = x_grid,
              y = y_grid,
              z = z_grid,
              type = "mesh3d",
              intensity = merged_grid$accessibility,
              cmin = global_min,
              cmax = global_max,
              colorscale = "Viridis",
              opacity = 0.40,
              name = sprintf("%s accessibility plane", stop_label),
              showscale = show_colorbar,
              colorbar = if (show_colorbar) {
                list(title = colorbar_title, len = 0.5, y = 0.3)
              } else {
                NULL
              },
              hoverinfo = "text",
              text = sprintf("%s: %s (%s)", stop_label, prettyNum(round(merged_grid$accessibility, 2), big.mark = ","), tolower(var_label)),
              showlegend = FALSE
            )
          }
        }
      }
    }
  }

  # Build time axis formatting
  format_hours <- function(h) {
    hours <- floor(h) %% 24
    mins <- round((h - floor(h)) * 60)
    sprintf("%02d:%02d", hours, mins)
  }
  
  tick_vals <- seq(floor(min_z), ceiling(max_z), length.out = 5)
  tick_text <- format_hours(tick_vals)

  # Setup axes limits to fit all raster data
  xaxis_settings <- list(title = "X (km)", zeroline = FALSE, autorange = FALSE)
  yaxis_settings <- list(title = "Y (km)", zeroline = FALSE, autorange = FALSE)
  
  if (length(all_raster_coords_x) > 0) {
    pad_x <- (max(all_raster_coords_x) - min(all_raster_coords_x)) * 0.05
    pad_y <- (max(all_raster_coords_y) - min(all_raster_coords_y)) * 0.05
    xaxis_settings$range <- c(min(all_raster_coords_x) - pad_x, max(all_raster_coords_x) + pad_x)
    yaxis_settings$range <- c(min(all_raster_coords_y) - pad_y, max(all_raster_coords_y) + pad_y)
  }

  # Layout setup
  p <- p |> plotly::layout(
    margin = list(l = 0, r = 0, b = 0, t = 0),
    scene = list(
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = 0.8),
      xaxis = xaxis_settings,
      yaxis = yaxis_settings,
      zaxis = list(
        title = "Time of Day",
        tickvals = tick_vals,
        ticktext = tick_text,
        zeroline = FALSE,
        autorange = FALSE
      ),
      camera = list(
        eye = list(x = 1.25, y = -1.25, z = 0.95)
      )
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.02,
      yanchor = "bottom"
    )
  ) |>
    plotly::config(displaylogo = FALSE)

  if (length(all_raster_coords_x) > 0) {
    p$x$custom_bounds <- list(
      x = c(min(all_raster_coords_x) - pad_x, max(all_raster_coords_x) + pad_x),
      y = c(min(all_raster_coords_y) - pad_y, max(all_raster_coords_y) + pad_y),
      z = c(min_z, max_z)
    )
  }

  p
}
