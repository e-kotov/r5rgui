#' @title Run the r5rgui Shiny Application
#'
#' @description This function launches a Shiny application that provides a graphical
#' user interface for the 'r5r' package, allowing for interactive transit routing.
#'
#' @param r5r_network A pre-built 'r5r' network object. This object contains the street and transit network data required for routing calculations.
#' @param center A numeric vector of length 2, specifying the initial longitude and latitude for the map's center. If `NULL` (the default), the map will be centered on the bounding box of the `r5r_network`. If `{r5r}` is below version 2.4.0, calculating the bounding box may be slow.
#' @param zoom An integer specifying the initial zoom level of the map. If `NULL` (the default), the zoom level will be automatically calculated to fit the bounding box of the `r5r_network`. If `{r5r}` is below version 2.4.0, calculating the bounding box may be slow.
#' @param departure_date A Date object specifying the initial departure date for the trip. Defaults to the current system date.
#' @param mode A character vector specifying the initial transport modes. This is passed directly to the `mode` argument in [detailed_itineraries()][r5r::detailed_itineraries] (and other functions of [`r5r`][r5r::r5r]). Defaults to `c("WALK", "TRANSIT")`.
#'
#' @return This function does not return a value; it launches a Shiny application.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # First, build the r5r network
#'   library(r5r)
#'
#'   # Note: This requires a valid r5r network.
#'   # Using the sample data included in the r5r package:
#'   data_path <- system.file("extdata/poa", package = "r5r")
#'   r5r_network <- setup_r5(data_path = data_path)
#'
#'   # Launch the application without specifying center and zoom
#'   # The map will be automatically centered and zoomed to the network's extent
#'   r5r_gui(r5r_network)
#'
#'   # Launch with a specific departure date with auto-zoom and center
#'   r5r_gui(r5r_network, departure_date = as.Date("2019-05-13"))
#'
#'   # Launch with specific transport modes
#'   r5r_gui(r5r_network, mode = c("WALK", "BUS"))
#'
#'   # Manually define map center and zoom
#'   map_center <- c(-51.22, -30.05)
#'   map_zoom <- 11
#'   r5r_gui(r5r_network, center = map_center, zoom = map_zoom)
#' }
r5r_gui <- function(
  r5r_network,
  center = NULL,
  zoom = NULL,
  departure_date = Sys.Date(),
  mode = c("WALK", "TRANSIT")
) {
  if (!check_r5r_available()) {
    stop(
      "The 'r5r' package is required to run this demo. Please install it first.",
      call. = FALSE
    )
  }
  # Get the name of the r5r_network object as a string
  r5r_network_name <- deparse(substitute(r5r_network))

  # if center or zoom are not provided, calculate them from the network bbox
  if (is.null(center) || is.null(zoom)) {
    if (utils::packageVersion("r5r") >= "2.3.0999") {
      street_network_bbox_fun <- get("street_network_bbox", asNamespace("r5r"))
      bbox <- street_network_bbox_fun(r5r_network, output = "vector")
    } else {
      message(
        "Calculating network bounding box with a legacy method. This is slow."
      )
      message(
        "Please update 'r5r' to version 2.4.0 or newer for better performance."
      )
      bbox <- sf::st_bbox(r5r::street_network_to_sf(r5r_network)$edges)
    }
    center <- c(
      (bbox["xmin"] + bbox["xmax"]) / 2,
      (bbox["ymin"] + bbox["ymax"]) / 2
    )
    center <- unname(center)

    lon_range <- bbox["xmax"] - bbox["xmin"]
    lat_range <- bbox["ymax"] - bbox["ymin"]
    max_range <- max(lon_range, lat_range)
    zoom <- floor(log2(360 / max_range))
    if (zoom > 18) zoom <- 18
  }

  # Add resource path to serve logo from the man/figures directory
  assets_path <- system.file("assets", package = "r5rgui")
  shiny::addResourcePath("r5rgui_assets", assets_path)

  # Load the UI from its file
  ui_path <- system.file("shiny_app", "ui.R", package = "r5rgui")
  ui <- source(ui_path, local = TRUE)$value

  # Load the server factory from its file
  server_factory_path <- system.file(
    "shiny_app",
    "server.R",
    package = "r5rgui"
  )
  server_factory <- source(server_factory_path, local = TRUE)$value

  # Create a list of arguments to pass to the server
  app_args <- list(
    r5r_network = r5r_network,
    r5r_network_name = r5r_network_name,
    center = center,
    zoom = zoom,
    departure_date = departure_date,
    mode = mode
  )

  # Use the factory to create the final server function
  server <- server_factory(app_args)

  # Launch the application using shinyApp
  shiny::shinyApp(ui = ui, server = server)
}
