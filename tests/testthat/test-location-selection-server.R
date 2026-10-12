test_that("named location inputs set routing coordinates", {
  skip_if_not_installed("shiny")

  factory <- source(
    system.file("shiny_app", "server.R", package = "r5rgui"),
    local = TRUE
  )$value
  choices <- data.frame(
    id = c("origin-a", "destination-b"),
    label = c("Origin A", "Destination B"),
    lon = c(-51.2, -51.1),
    lat = c(-30.0, -30.1)
  )
  server <- factory(list(
    r5r_network = list(Graph = "dummy network"),
    r5r_network_name = "network",
    center = c(-51.2, -30.0), zoom = 11,
    departure_date = as.Date("2025-01-10"),
    mode = c("WALK", "TRANSIT"),
    basemaps = list(Positron = "https://example.org/style.json"),
    transit_availability = list(Graph = NULL),
    location_choices = choices,
    location_polygons = NULL
  ))

  shiny::testServer(server, {
    session$setInputs(origin_location_internal = "")
    session$setInputs(destination_location_internal = "")
    session$flushReact()
    session$setInputs(origin_location_internal = "origin-a")
    expect_equal(locations$start, list(lat = -30.0, lon = -51.2))
    expect_identical(selected_location_ids$origin, "origin-a")

    session$setInputs(destination_location_internal = "destination-b")
    expect_equal(locations$end, list(lat = -30.1, lon = -51.1))
    expect_identical(selected_location_ids$destination, "destination-b")
  })
})
