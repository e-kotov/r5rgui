# Run the r5rgui Shiny Application

This function launches a Shiny application that provides a graphical
user interface for the 'r5r' package, allowing for interactive transit
routing.

## Usage

``` r
r5r_gui(r5r_network, center = NULL, zoom = NULL, departure_date = Sys.Date())
```

## Arguments

- r5r_network:

  A pre-built 'r5r' network object. This object contains the street and
  transit network data required for routing calculations.

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

## Value

This function does not return a value; it launches a Shiny application.

## Examples

``` r
if (FALSE) { # \dontrun{
# First, build the r5r network
library(r5r)
data_path <- system.file("extdata/poa", package = "r5r")
r5r_network <- setup_r5(data_path = data_path)

# Launch the application without specifying center and zoom
# The map will be automatically centered and zoomed to the network's extent
r5r_gui(r5r_network)

# Launch with a specific departure date with auto-zoom and center
r5r_gui(r5r_network, departure_date = as.Date("2019-05-13"))

# Manually define map center and zoom
map_center <- c(-51.22, -30.05)
map_zoom <- 11
r5r_gui(r5r_network, center = map_center, zoom = map_zoom)
} # }
```
