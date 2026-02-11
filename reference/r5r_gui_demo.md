# Run a Demonstration of the r5rgui Application

A simple wrapper function to launch the `r5rgui` Shiny application using
the sample Porto Alegre dataset included with the `r5r` package.

This function is designed for ease of use and demonstration purposes. It
automatically handles the setup of the `r5r_network` object and provides
default map settings (center and zoom) appropriate for the sample data.

## Usage

``` r
r5r_gui_demo(mode = c("WALK", "TRANSIT"))
```

## Arguments

- mode:

  A character vector specifying the initial transport modes. Defaults to
  `c("WALK", "TRANSIT")`.

## Value

This function does not return a value; it launches a Shiny application.

## Details

The function first locates the sample data within the `r5r` package,
then calls
[`r5r::setup_r5()`](https://ipeagit.github.io/r5r/reference/setup_r5.html)
to build the routing core. Finally, it launches the Shiny app by calling
[`r5r_gui()`](http://www.ekotov.pro/r5rgui/reference/r5r_gui.md) with
these pre-configured objects. Please note that `r5r` may need to
download the sample data on first use, which requires an internet
connection.

## Examples

``` r
if (interactive()) {
  # To run the demo application, simply call the function:
  r5r_gui_demo()

  # Run with specific transport modes
  r5r_gui_demo(mode = c("WALK", "BUS"))
}
```
