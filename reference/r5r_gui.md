# Run the r5rgui Shiny Application

This function launches a Shiny application that provides a graphical
user interface for the 'r5r' package, allowing for interactive transit
routing.

## Usage

``` r
r5r_gui(
  r5r_network,
  center = NULL,
  zoom = NULL,
  departure_date = Sys.Date(),
  mode = c("WALK", "TRANSIT")
)
```

## Arguments

- r5r_network:

  A pre-built 'r5r' network object, or a named list of such objects for
  comparison. If a list is provided, the list names will be used as
  labels in the GUI. If a single object is provided, its variable name
  will be used as the label.

- center:

  A numeric vector of length 2, specifying the initial longitude and
  latitude for the map's center. If `NULL` (the default), the map will
  be centered on the bounding box of the `r5r_network`. If `{r5r}` is
  below version 2.4.0, calculating the bounding box may be slow.

- zoom:

  An integer specifying the initial zoom level of the map. If `NULL`
  (the default), the zoom level will be automatically calculated to fit
  the bounding box of the `r5r_network`. If `{r5r}` is below version
  2.4.0, calculating the bounding box may be slow.

- departure_date:

  A Date object specifying the initial departure date for the trip.
  Defaults to the current system date.

- mode:

  A character vector specifying the initial transport modes. This is
  passed directly to the `mode` argument in
  [detailed_itineraries()](https://ipeagit.github.io/r5r/reference/detailed_itineraries.html)
  (and other functions of
  [`r5r`](https://ipeagit.github.io/r5r/reference/r5r.html)). Defaults
  to `c("WALK", "TRANSIT")`.

## Value

This function does not return a value; it launches a Shiny application.

## Examples

``` r
if (interactive()) {
  # First, build the r5r network
  library(r5r)

  # Note: This requires a valid r5r network.
  # Using the sample data included in the r5r package:
  data_path <- system.file("extdata/poa", package = "r5r")
  r5r_network <- setup_r5(data_path = data_path)

  # Launch the application without specifying center and zoom
  # The map will be automatically centered and zoomed to the network's extent
  r5r_gui(r5r_network)

  # Launch with a specific departure date with auto-zoom and center
  r5r_gui(r5r_network, departure_date = as.Date("2019-05-13"))

  # Launch with specific transport modes
  r5r_gui(r5r_network, mode = c("WALK", "BUS"))

  # Manually define map center and zoom
  map_center <- c(-51.22, -30.05)
  map_zoom <- 11
  r5r_gui(r5r_network, center = map_center, zoom = map_zoom)
  
  # Compare two networks
  # Note: For this example, we use the same network object twice. 
  # In a real scenario, you would use two different networks (e.g. current vs future).
  r5r_gui(list("Baseline" = r5r_network, "Scenario A" = r5r_network))
}
```
