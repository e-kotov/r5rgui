# Synthetic fixtures: 3 cells in a line, cell "a" is the origin-side cell.
make_test_ttm <- function() {
  expand.grid(
    from_id = c("a", "b", "c"),
    to_id = c("a", "b", "c"),
    stringsAsFactors = FALSE
  ) |>
    transform(travel_time = c(0, 10, 20, 10, 0, 10, 20, 10, 0))
}

make_test_land_use <- function() {
  data.frame(
    id = c("a", "b", "c"),
    lon = c(-51.22, -51.21, -51.20),
    lat = c(-30.03, -30.03, -30.03),
    jobs = c(100, 200, NA),
    population = c(50, NA, 150)
  )
}

test_that("compute_access_metric returns id + accessibility for count metrics", {
  ttm <- make_test_ttm()
  lu <- make_test_land_use()

  for (metric in c("cumulative", "cumulative_interval", "gravity")) {
    res <- compute_access_metric(
      ttm = ttm,
      land_use = lu,
      access_metric = metric,
      access_variable = "jobs",
      access_cutoff = 15
    )
    expect_s3_class(res, "data.frame")
    expect_named(res, c("id", "accessibility"))
    expect_true(all(!is.na(res$accessibility)))
  }

  # cumulative with cutoff 15 reaches self + direct neighbor(s)
  res_cum <- compute_access_metric(
    ttm = ttm,
    land_use = lu,
    access_metric = "cumulative",
    access_variable = "jobs",
    access_cutoff = 15
  )
  a_val <- res_cum$accessibility[res_cum$id == "a"]
  expect_equal(a_val, 300) # jobs in a (100) + b (200); NA in c treated as 0
})

test_that("compute_access_metric handles cost-based metrics (regression: score column)", {
  # cost_to_closest and balancing_cost report a travel_time column, not the
  # opportunity column. This used to silently return no data in the MapGL cube.
  ttm <- make_test_ttm()
  lu <- make_test_land_use()

  res_cost <- compute_access_metric(
    ttm = ttm,
    land_use = lu,
    access_metric = "cost_to_closest",
    access_variable = "jobs",
    access_n_closest = 1L
  )
  expect_s3_class(res_cost, "data.frame")
  expect_named(res_cost, c("id", "accessibility"))
  expect_true(nrow(res_cost) > 0)

  res_bal <- compute_access_metric(
    ttm = ttm,
    land_use = lu,
    access_metric = "balancing_cost",
    access_variable = "jobs",
    demand_variable = "population"
  )
  expect_s3_class(res_bal, "data.frame")
  expect_named(res_bal, c("id", "accessibility"))
  expect_true(nrow(res_bal) > 0)
})

test_that("compute_access_metric picks a default demand variable for competitive metrics", {
  ttm <- make_test_ttm()
  lu <- make_test_land_use()

  for (metric in c("floating_catchment", "spatial_availability")) {
    res <- compute_access_metric(
      ttm = ttm,
      land_use = lu,
      access_metric = metric,
      access_variable = "jobs",
      access_cutoff = 15
    )
    expect_s3_class(res, "data.frame")
    expect_named(res, c("id", "accessibility"))
  }
})

test_that("compute_access_metric warns and returns NULL when demand is unavailable", {
  ttm <- make_test_ttm()
  lu <- make_test_land_use()
  lu$population <- NULL

  expect_warning(
    res <- compute_access_metric(
      ttm = ttm,
      land_use = lu,
      access_metric = "spatial_availability",
      access_variable = "jobs"
    ),
    "demand"
  )
  expect_null(res)
})

test_that("compute_access_metric returns NULL for unknown metrics and variables", {
  ttm <- make_test_ttm()
  lu <- make_test_land_use()

  expect_null(
    compute_access_metric(
      ttm = ttm,
      land_use = lu,
      access_metric = "not_a_metric",
      access_variable = "jobs"
    )
  )
  expect_null(
    compute_access_metric(
      ttm = ttm,
      land_use = lu,
      access_metric = "cumulative",
      access_variable = "not_a_column"
    )
  )
})

test_that("normalize_land_use handles data.frames, sf points and sf polygons", {
  lu_df <- make_test_land_use()

  # Plain data.frame
  norm <- normalize_land_use(lu_df)
  expect_named(norm, c("data", "polygons"))
  expect_null(norm$polygons)
  expect_type(norm$data$id, "character")

  # sf points
  pts <- sf::st_as_sf(lu_df, coords = c("lon", "lat"), crs = 4326)
  norm_pts <- normalize_land_use(pts)
  expect_null(norm_pts$polygons)
  expect_true(all(c("lon", "lat") %in% names(norm_pts$data)))
  expect_equal(norm_pts$data$lon, lu_df$lon, tolerance = 1e-6)

  # sf polygons
  polys <- sf::st_buffer(sf::st_transform(pts, 32722), 200) |>
    sf::st_transform(4326)
  norm_polys <- normalize_land_use(polys)
  expect_s3_class(norm_polys$polygons, "sf")
  expect_equal(nrow(norm_polys$polygons), nrow(lu_df))
  expect_true(all(c("lon", "lat") %in% names(norm_polys$data)))

  # missing id column gets one; missing lon/lat errors
  no_id <- lu_df[, setdiff(names(lu_df), "id")]
  expect_true("id" %in% names(normalize_land_use(no_id)$data))
  expect_error(
    normalize_land_use(data.frame(jobs = 1:3)),
    "lon"
  )
})

test_that("land_use_variables and default_demand_variable pick numeric columns", {
  lu <- make_test_land_use()
  expect_setequal(land_use_variables(lu), c("jobs", "population"))
  expect_equal(default_demand_variable(lu, "jobs"), "population")
  expect_equal(default_demand_variable(lu, "population"), "jobs")

  lu_one <- lu[, c("id", "lon", "lat", "jobs")]
  expect_null(default_demand_variable(lu_one, "jobs"))
})

test_that("derive_footprint_radius scales with grid spacing", {
  # Regular 500 m grid of points
  grid <- expand.grid(x = seq(0, 5000, by = 500), y = seq(0, 5000, by = 500))
  pts <- sf::st_as_sf(grid, coords = c("x", "y"), crs = 32722)
  radius <- derive_footprint_radius(pts)
  expect_gt(radius, 200)
  expect_lt(radius, 400)

  # Degenerate case falls back to the default
  single <- pts[1, ]
  expect_equal(derive_footprint_radius(single), 250)
})

test_that("normalize_travel_time_col standardizes the travel time column", {
  ttm_p50 <- data.frame(from_id = "a", to_id = "b", travel_time_p50 = 5)
  expect_named(
    normalize_travel_time_col(ttm_p50),
    c("from_id", "to_id", "travel_time")
  )
  ttm_plain <- data.frame(from_id = "a", to_id = "b", travel_time = 5)
  expect_identical(normalize_travel_time_col(ttm_plain), ttm_plain)
  expect_null(
    normalize_travel_time_col(data.frame(from_id = "a", to_id = "b"))
  )
})

test_that("compute_leg_times accumulates wait and segment durations", {
  itinerary <- data.frame(
    wait = c(0, 5),
    segment_duration = c(15, 10)
  )
  trip_dep <- as.POSIXct("2019-05-13 14:00:00", tz = "UTC")
  times <- compute_leg_times(itinerary, trip_dep)
  expect_equal(times$departure[1], trip_dep)
  expect_equal(times$arrival[1], trip_dep + 15 * 60)
  expect_equal(times$departure[2], trip_dep + 20 * 60)
  expect_equal(times$arrival[2], trip_dep + 30 * 60)
})

test_that("is_transit_mode is vectorized and NA-safe", {
  expect_false(is_transit_mode(NULL))
  expect_false(is_transit_mode(character(0)))
  expect_equal(
    is_transit_mode(c("WALK", "BUS", NA, "transit")),
    c(FALSE, TRUE, FALSE, TRUE)
  )
})
