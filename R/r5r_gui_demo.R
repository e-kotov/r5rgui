#' @title Run a Demonstration of the r5rgui Application
#'
#' @description
#' A simple wrapper function to launch the `r5rgui` Shiny application using
#' the sample Porto Alegre dataset included with the `r5r` package.
#'
#' This function is designed for ease of use and demonstration purposes. It
#' automatically handles the setup of the `r5r_network` object and provides
#' default map settings (center and zoom) appropriate for the sample data.
#'
#' @details
#' The function first locates the sample data within the `r5r` package, then
#' calls `r5r::setup_r5()` to build the routing core. Finally, it launches
#' the Shiny app by calling `r5r_gui()` with these pre-configured objects.
#' Please note that `r5r` may need to download the sample data on first use,
#' which requires an internet connection.
#'
#' @return This function does not return a value; it launches a Shiny application.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # To run the demo application, simply call the function:
#'   r5r_gui_demo()
#' }
#'
r5r_gui_demo <- function() {
  if (!check_r5r_available()) {
    stop(
      "The 'r5r' package is required to run this demo. Please install it first.",
      call. = FALSE
    )
  }

  # Set the option to TRUE for the duration of this function call
  options(r5rgui.is_demo_mode = TRUE)
  # IMPORTANT: Ensure the option is reset when the function exits,
  # even if the app crashes.
  on.exit(options(r5rgui.is_demo_mode = NULL))

  message(
    "Setting up r5r with sample Porto Alegre data. This may take a moment..."
  )

  # Define the path to the sample data included with the r5r package
  data_path <- system.file("extdata/poa", package = "r5r")

  # Stop if the sample data directory cannot be found
  if (data_path == "") {
    stop(
      "Could not find the sample data directory in the 'r5r' package.",
      call. = FALSE
    )
  }

  # Set Java memory options and build the r5r_network object
  # Add backward compatibility for r5r versions < 2.3.0
  r5r_network <- if (utils::packageVersion("r5r") >= "2.3.0") {
    utils::getFromNamespace("build_network", "r5r")(
      data_path = data_path,
      verbose = FALSE
    )
  } else {
    r5r::setup_r5(data_path = data_path, verbose = FALSE)
  }

  # Define the default map center and zoom level for Porto Alegre
  map_center <- c(-51.22, -30.05)
  map_zoom <- 11
  departure_date = as.Date("2019-05-13")

  message(
    "Launching the r5rgui Shiny application in demo mode with Porto Alegre data..."
  )

  # Launch the main application with the demo data and settings
  r5r_gui(
    r5r_network = r5r_network,
    center = map_center,
    zoom = map_zoom,
    departure_date = departure_date
  )
}

# helper
# Check if r5r is installed
#' @noRd
check_r5r_available <- function() {
  requireNamespace("r5r", quietly = TRUE)
}
