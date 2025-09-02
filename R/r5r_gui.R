#' @title Run the r5rgui Shiny Application
#'
#' @description This function launches a Shiny application that provides a graphical
#' user interface for the 'r5r' package, allowing for interactive transit routing.
#'
#' @param r5r_network A pre-built 'r5r' network object. This object contains the
#'   street and transit network data required for routing calculations.
#' @param center A numeric vector of length 2, specifying the initial longitude
#'   and latitude for the map's center.
#' @param zoom An integer specifying the initial zoom level of the map.
#' @param departure_date A Date object specifying the initial departure date for
#'   the trip. Defaults to the current system date.
#'
#' @return This function does not return a value; it launches a Shiny application.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First, build the r5r network
#' options(java.parameters = "-Xmx4G")
#' data_path <- system.file("extdata/poa", package = "r5r")
#' r5r_network <- setup_r5(data_path = data_path)
#'
#' # Define map center and zoom
#' map_center <- c(-51.22, -30.05)
#' map_zoom <- 11
#'
#' # Launch the application
#' r5r_gui(r5r_network, center = map_center, zoom = map_zoom)
#'
#' # Launch with a specific departure date
#' r5r_gui(
#'   r5r_network,
#'   center = map_center,
#'   zoom = map_zoom,
#'   departure_date = as.Date("2019-05-13")
#' )
#' }
r5r_gui <- function(
  r5r_network,
  center,
  zoom,
  departure_date = Sys.Date()
) {
  # Pass arguments to the Shiny app environment
  app_dir <- system.file("shiny_app", package = "r5rgui")
  .GlobalEnv$.r5rgui_args <- list(
    r5r_network = r5r_network,
    center = center,
    zoom = zoom,
    departure_date = departure_date
  )
  on.exit(rm(".r5rgui_args", envir = .GlobalEnv))

  shiny::runApp(app_dir)
}
