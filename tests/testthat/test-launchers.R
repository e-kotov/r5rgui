skip_on_cran()
skip_if_not_installed("r5r")
skip_if_not_installed("shiny")
skip_if_not_installed("rlang")

# --- Test for r5r_gui_demo() ---
test_that("r5r_gui_demo() and r5r_gui() fail gracefully if r5r is not installed", {
  local_mocked_bindings(
    check_r5r_available = function() FALSE,
    .package = "r5rgui"
  )

  expect_error(
    r5r_gui_demo(),
    "The 'r5r' package is required to run this demo. Please install it first."
  )

  expect_error(
    r5r_gui(r5r_network = "dummy_network"),
    "The 'r5r' package is required to run this demo. Please install it first."
  )
})

test_that("r5r_gui_demo() prepares correct arguments for r5r_gui", {
  r5r_gui_args_captured <- NULL

  local_mocked_bindings(
    r5r_gui = function(...) {
      r5r_gui_args_captured <<- list(...)
    },
    .package = "r5rgui"
  )

  if (utils::packageVersion("r5r") >= "2.3.0") {
    local_mocked_bindings(
      build_network = function(...) "dummy_network",
      .package = "r5r"
    )
  }
  local_mocked_bindings(
    setup_r5 = function(...) "dummy_network",
    .package = "r5r"
  )

  r5r_gui_demo(mode = c("WALK", "BUS"))

  expect_false(is.null(r5r_gui_args_captured))
  expect_equal(r5r_gui_args_captured$center, c(-51.22, -30.05))
  expect_equal(r5r_gui_args_captured$zoom, 11)
  expect_equal(r5r_gui_args_captured$mode, c("WALK", "BUS"))
})


# --- Test for r5r_gui() ---

test_that("r5r_gui() prepares arguments correctly for the shiny app", {
  dummy_net <- list(name = "dummy_network_object")
  dummy_center <- c(-51.22, -30.05)
  dummy_zoom <- 11
  dummy_date <- as.Date("2025-09-01")
  dummy_mode <- c("WALK", "BUS")

  captured_server_function <- NULL

  local_mocked_bindings(
    shinyApp = function(ui, server) {
      captured_server_function <<- server
    },
    .package = "shiny"
  )

  r5r_gui(
    r5r_network = dummy_net,
    center = dummy_center,
    zoom = dummy_zoom,
    departure_date = dummy_date,
    mode = dummy_mode
  )

  expect_false(
    is.null(captured_server_function),
    label = "The mock should have captured the server function."
  )
  expect_true(is.function(captured_server_function))

  server_env <- rlang::fn_env(captured_server_function)
  captured_args <- server_env$app_args

  expect_identical(captured_args$r5r_network, dummy_net)
  expect_identical(captured_args$center, dummy_center)
  expect_identical(captured_args$zoom, dummy_zoom)
  expect_identical(captured_args$departure_date, dummy_date)
  expect_identical(captured_args$mode, dummy_mode)
  expect_identical(captured_args$r5r_network_name, "dummy_net")
})

# --- Tests for automatic centering and zooming ---

test_that("r5r_gui() sets automatic center and zoom when not provided", {
  skip_if_not(exists("r5r_net"))

  captured_server_function <- NULL
  local_mocked_bindings(
    shinyApp = function(ui, server) {
      captured_server_function <<- server
    },
    .package = "shiny"
  )

  r5r_gui(r5r_network = r5r_net)

  server_env <- rlang::fn_env(captured_server_function)
  captured_args <- server_env$app_args

  expect_false(
    is.null(captured_args$center),
    label = "Center should be calculated"
  )
  expect_false(is.null(captured_args$zoom), label = "Zoom should be calculated")
  expect_true(is.numeric(captured_args$center))
  expect_true(is.numeric(captured_args$zoom))
  expect_equal(length(captured_args$center), 2)
})

test_that("r5r_gui() uses fallback and shows message for older r5r versions", {
  skip_if_not(exists("r5r_net"))

  captured_server_function <- NULL

  local_mocked_bindings(
    shinyApp = function(ui, server) {
      captured_server_function <<- server
    },
    .package = "shiny"
  )

  local_mocked_bindings(
    packageVersion = function(pkg) {
      if (pkg == "r5r") {
        return("2.3.0")
      }
      return(base::packageVersion(pkg))
    },
    .package = "utils"
  )

  expect_message(
    r5r_gui(r5r_network = r5r_net),
    "Calculating network bounding box with a legacy method. This is slow."
  )

  server_env <- rlang::fn_env(captured_server_function)
  captured_args <- server_env$app_args

  expect_false(
    is.null(captured_args$center),
    label = "Center should be calculated with fallback"
  )
  expect_false(
    is.null(captured_args$zoom),
    label = "Zoom should be calculated with fallback"
  )
})
