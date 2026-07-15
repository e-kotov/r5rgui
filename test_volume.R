library(plotly)
library(sf)

poa_grid_file <- system.file("extdata/poa/poa_hexgrid.csv", package = "r5r")
poa_grid <- read.csv(poa_grid_file)

# Just simulate a volume with Z from 0 to 23
x_vals <- c()
y_vals <- c()
z_vals <- c()
values <- c()

center_lon <- -51.2
center_lat <- -30.03
epsg <- 31982 # UTM 22S roughly

grid_sf <- sf::st_as_sf(poa_grid, coords = c("lon", "lat"), crs = 4326)
grid_proj <- sf::st_transform(grid_sf, epsg)
grid_coords <- sf::st_coordinates(grid_proj)

center_proj <- sf::st_transform(sf::st_sfc(sf::st_point(c(center_lon, center_lat)), crs = 4326), epsg)
center_xy <- sf::st_coordinates(center_proj)[1, c("X", "Y")]

x_base <- (grid_coords[, "X"] - center_xy["X"]) / 1000
y_base <- (grid_coords[, "Y"] - center_xy["Y"]) / 1000

for (z in 0:2) {
  x_vals <- c(x_vals, x_base)
  y_vals <- c(y_vals, y_base)
  z_vals <- c(z_vals, rep(z, length(x_base)))
  # Fake some data
  v <- poa_grid$jobs + z * 100
  values <- c(values, v)
}

p <- plot_ly(
  type = "volume",
  x = x_vals,
  y = y_vals,
  z = z_vals,
  value = values,
  isomin = 0,
  isomax = max(values, na.rm=TRUE),
  opacity = 0.1,
  surface_count = 10,
  slices = list(
    x = list(show = TRUE),
    y = list(show = TRUE),
    z = list(show = TRUE)
  )
)

htmlwidgets::saveWidget(p, "test_volume.html")
print("Saved to test_volume.html")
