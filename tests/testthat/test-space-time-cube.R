test_that("plot_space_time_cube works with mock detailed itinerary", {
  geom1 <- sf::st_sfc(sf::st_linestring(matrix(c(
    -51.22, -30.03,
    -51.21, -30.04
  ), ncol = 2, byrow = TRUE)), crs = 4326)
  
  itinerary <- sf::st_sf(
    option = 1L,
    segment = 1L,
    mode = "WALK",
    departure_time = "14:00:00",
    segment_duration = 15,
    wait = 0,
    geometry = geom1
  )
  
  p <- plot_space_time_cube(itinerary)
  expect_s3_class(p, "plotly")
})

test_that("plot_space_time_cube works with multiple legs and dwell time", {
  geom1 <- sf::st_sfc(sf::st_linestring(matrix(c(
    -51.22, -30.03,
    -51.21, -30.04
  ), ncol = 2, byrow = TRUE)), crs = 4326)
  
  geom2 <- sf::st_sfc(sf::st_linestring(matrix(c(
    -51.21, -30.04,
    -51.20, -30.05
  ), ncol = 2, byrow = TRUE)), crs = 4326)
  
  itinerary <- sf::st_sf(
    option = c(1L, 1L),
    segment = c(1L, 2L),
    mode = c("WALK", "BUS"),
    departure_time = c("14:00:00", "14:00:00"),
    segment_duration = c(15, 15),
    wait = c(0, 5),
    geometry = c(geom1, geom2)
  )
  
  p <- plot_space_time_cube(itinerary)
  expect_s3_class(p, "plotly")
  
  # If we have r5r_net from setup.R, we can also test isochrones!
  if (exists("r5r_net")) {
    p_iso <- plot_space_time_cube(
      itinerary, 
      r5r_network = r5r_net, 
      isochrones = TRUE, 
      isochrone_cutoffs = 5
    )
    expect_s3_class(p_iso, "plotly")
  }
})

test_that("calculate_route_isochrones returns correct structure with origin and destination", {
  if (exists("r5r_net")) {
    geom1 <- sf::st_sfc(sf::st_linestring(matrix(c(
      -51.22013, -30.03000,
      -51.21690, -30.03050
    ), ncol = 2, byrow = TRUE)), crs = 4326)
    
    geom2 <- sf::st_sfc(sf::st_linestring(matrix(c(
      -51.21690, -30.03050,
      -51.17352, -30.02947
    ), ncol = 2, byrow = TRUE)), crs = 4326)
    
    itinerary <- sf::st_sf(
      option = c(1L, 1L),
      segment = c(1L, 2L),
      mode = c("WALK", "BUS"),
      departure_time = c("14:00:00", "14:00:00"),
      segment_duration = c(15, 10),
      wait = c(0, 5),
      geometry = c(geom1, geom2)
    )
    
    iso_res <- calculate_route_isochrones(itinerary, r5r_network = r5r_net, cutoffs = 5)
    expect_s3_class(iso_res, "sf")
    expect_true(nrow(iso_res) > 0)
    expect_true("dwell_1" %in% iso_res$id)
    expect_true("dwell_2" %in% iso_res$id)
    expect_true("dwell_3" %in% iso_res$id)
  }
})

test_that("plot_space_time_cube works with all 7 accessibility metrics", {
  if (exists("r5r_net")) {
    geom1 <- sf::st_sfc(sf::st_linestring(matrix(c(
      -51.22013, -30.03000,
      -51.21690, -30.03050
    ), ncol = 2, byrow = TRUE)), crs = 4326)
    
    itinerary <- sf::st_sf(
      option = 1L,
      segment = 1L,
      mode = "WALK",
      departure_time = "14:00:00",
      segment_duration = 15,
      wait = 0,
      geometry = geom1
    )
    
    metrics <- c("cumulative", "cumulative_interval", "gravity", "cost_to_closest", "floating_catchment", "spatial_availability", "balancing_cost")
    
    for (m in metrics) {
      p <- plot_space_time_cube(
        itinerary = itinerary,
        r5r_network = r5r_net,
        accessibility_surface = TRUE,
        access_metric = m,
        access_cutoff = 10,
        access_n_closest = 1
      )
      expect_s3_class(p, "plotly")
    }
  }
})

test_that("get_space_time_cube_data returns stacked slices, incl. cost-based metrics", {
  skip_if_not(exists("r5r_net"))

  origin <- c(-51.22013, -30.03000)
  poa_grid <- read.csv(system.file("extdata/poa/poa_hexgrid.csv", package = "r5r"))

  cube <- get_space_time_cube_data(
    r5r_network = r5r_net,
    origin_coords = origin,
    access_variable = "jobs",
    access_metric = "cumulative",
    access_cutoff = 15,
    date = "2019-05-13",
    mode = c("WALK", "TRANSIT"),
    land_use = poa_grid,
    hours = c(8, 9),
    max_reach_minutes = 20
  )
  expect_s3_class(cube, "sf")
  expect_setequal(unique(cube$hour), c(8, 9))
  expect_true(all(
    c("id", "lon", "lat", "accessibility", "base_height", "top_height", "cube_label") %in%
      names(cube)
  ))
  expect_true(all(cube$top_height > cube$base_height))

  # Regression: cost-based metrics report travel_time, not the opportunity
  # column, and used to silently return NULL in the MapGL cube path
  cube_cost <- get_space_time_cube_data(
    r5r_network = r5r_net,
    origin_coords = origin,
    access_variable = "schools",
    access_metric = "cost_to_closest",
    date = "2019-05-13",
    mode = "WALK",
    land_use = poa_grid,
    hours = 8,
    max_reach_minutes = 15
  )
  expect_s3_class(cube_cost, "sf")
  expect_true(nrow(cube_cost) > 0)
})

test_that("plot_space_time_cube works with single origin point (itinerary = NULL)", {
  p_base <- plot_space_time_cube(
    itinerary = NULL,
    origin_coords = c(-51.22013, -30.03000),
    date = "2026-07-15",
    departure_time = "14:00"
  )
  expect_s3_class(p_base, "plotly")

  if (exists("r5r_net")) {
    p_acc <- plot_space_time_cube(
      itinerary = NULL,
      origin_coords = c(-51.22013, -30.03000),
      r5r_network = r5r_net,
      accessibility_surface = TRUE,
      access_metric = "cumulative",
      date = "2026-07-15",
      departure_time = "14:00"
    )
    expect_s3_class(p_acc, "plotly")
  }
})
