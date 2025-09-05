testthat::skip_if_not_installed("r5r")
testthat::skip_if_not_installed("shiny")
testthat::skip_if_not_installed("mockery")

# --- Test for r5r_gui_demo() ---
testthat::test_that("r5r_gui_demo() fails gracefully if r5r is not installed", {
  mockery::stub(r5r_gui_demo, 'requireNamespace', FALSE)
  testthat::expect_error(
    r5r_gui_demo(),
    "The 'r5r' package is required to run this demo."
  )
})

testthat::test_that("r5r_gui_demo() prepares correct arguments for r5r_gui", {
  r5r_gui_args_captured <- NULL
  mockery::stub(r5r_gui_demo, 'r5r_gui', function(...) {
    r5r_gui_args_captured <<- list(...)
  })

  # Only try to mock 'build_network' if the installed {r5r} is new enough.
  if (utils::packageVersion("r5r") >= "2.3.0") {
    mockery::stub(r5r_gui_demo, 'r5r::build_network', "dummy_network")
  }

  # This mock for the older function is always safe to have.
  # On new versions it just won't be used; on old versions it's essential.
  mockery::stub(r5r_gui_demo, 'r5r::setup_r5', "dummy_network")

  # Run the function, which will now use the correct mock for the environment
  r5r_gui_demo()

  # Assertions remain the same
  testthat::expect_false(is.null(r5r_gui_args_captured))
  testthat::expect_equal(r5r_gui_args_captured$center, c(-51.22, -30.05))
  testthat::expect_equal(r5r_gui_args_captured$zoom, 11)
})


# --- Test for r5r_gui() ---

testthat::test_that("r5r_gui() prepares arguments correctly", {
  dummy_net <- list(name = "dummy_network_object")
  dummy_center <- c(-51.22, -30.05)
  dummy_zoom <- 11
  dummy_date <- as.Date("2025-09-01")

  # These variables will capture the state from *inside* the mock
  run_app_called_with <- NULL
  captured_global_args <- NULL

  # This mock now does two things:
  # 1. It captures the app directory path for a later test.
  # 2. It captures the global arguments variable *before* `on.exit` cleans it up.
  mockery::stub(r5r_gui, "shiny::runApp", function(appDir, ...) {
    run_app_called_with <<- appDir
    if (exists(".r5rgui_args", envir = .GlobalEnv)) {
      captured_global_args <<- get(".r5rgui_args", envir = .GlobalEnv)
    }
  })

  # Execute the function
  r5r_gui(
    r5r_network = dummy_net,
    center = dummy_center,
    zoom = dummy_zoom,
    departure_date = dummy_date
  )

  # --- ASSERTIONS ---
  # Now, we assert against our captured snapshot, not the live global environment.

  # 1. Check if the capture was successful
  testthat::expect_false(
    is.null(captured_global_args),
    label = "The mock should have captured the .r5rgui_args list."
  )

  # 2. Check the contents of the captured list
  testthat::expect_identical(captured_global_args$r5r_network, dummy_net)
  testthat::expect_identical(captured_global_args$center, dummy_center)
  testthat::expect_identical(captured_global_args$zoom, dummy_zoom)
  testthat::expect_identical(captured_global_args$departure_date, dummy_date)
  testthat::expect_identical(captured_global_args$r5r_network_name, "dummy_net")

  # 3. Check that runApp was still called correctly
  expected_app_dir <- system.file("shiny_app", package = "r5rgui")
  testthat::expect_equal(run_app_called_with, expected_app_dir)
})

testthat::test_that("r5r_gui() cleans up the global environment variable on exit", {
  # This test remains crucial to prove the cleanup works.
  mockery::stub(r5r_gui, "shiny::runApp", function(...) {
    # We can even mock it to do nothing, just letting the function exit.
  })

  # Execute the function
  r5r_gui(r5r_network = list(), center = c(0, 0), zoom = 1)

  # The most important check: did the `on.exit` call work after a successful run?
  testthat::expect_false(
    exists(".r5rgui_args", envir = .GlobalEnv),
    label = "The .r5rgui_args object should be removed after the function exits."
  )
})
