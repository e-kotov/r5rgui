# --- Tests for Internal Server Logic ---

test_that("Server logic handles inputs correctly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("r5r")

  # 1. Load the server factory directly from source
  server_factory <- source(
    system.file("shiny_app", "server.R", package = "r5rgui"),
    local = TRUE
  )$value

  # 2. Create dummy arguments expected by the server factory
  dummy_args <- list(
    r5r_network = "dummy_network_obj",
    r5r_network_name = "dummy_network",
    center = c(0, 0),
    zoom = 10,
    departure_date = Sys.Date()
  )

  # 3. Instantiate the server function
  server <- server_factory(dummy_args)

  # 4. Use shiny::testServer to test the logic
  shiny::testServer(server, {

    # -- Test Initialization --
    # Check if date input was updated with the argument passed in dummy_args
    # Note: inputs set via update* are usually reflected in session,
    # but strict unit testing of updates often requires checking output or session$sendCustomMessage.
    # Here we verify the reactive logic flows.

    # -- Test: Map Click triggers Start Location update --

    # Simulate a click on the map (start point)
    session$setInputs(map_click = list(lng = -51.22, lat = -30.05))

    # Check if the reactive value 'locations$start' updated
    expect_equal(locations$start$lon, -51.22)
    expect_equal(locations$start$lat, -30.05)

    # Check if the text input update was triggered (value stored in input after update)
    # In testServer, we can check if the logic attempted to update the input.
    # Since testServer mocks the browser, we verify the internal reactive state is consistent.

    # -- Test: Right Click triggers End Location update --

    # Simulate a right click (end point)
    session$setInputs(js_right_click = list(lng = -51.18, lat = -30.02))

    expect_equal(locations$end$lon, -51.18)
    expect_equal(locations$end$lat, -30.02)

    # -- Test: Reset button clears locations --

    session$setInputs(reset = 1)

    expect_null(locations$start)
    expect_null(locations$end)

    # -- Test: Manual Text Input Updates Location --

    # User types in start coordinates manually
    session$setInputs(start_coords_input = "-30.10, -51.10")

    # Wait for observer to fire
    session$flushReact()

    expect_equal(locations$start$lat, -30.10)
    expect_equal(locations$start$lon, -51.10)

  })
})
