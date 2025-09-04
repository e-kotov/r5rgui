skip_if_not_installed("r5r")
skip_if_not_installed("shiny")
skip_if_not_installed("mockery")

# --- Test for r5r_gui_demo() ---

test_that("r5r_gui_demo() fails gracefully if r5r is not installed", {
  # This test was correct.
  mockery::stub(r5r_gui_demo, 'requireNamespace', FALSE)

  expect_error(
    r5r_gui_demo(),
    "The 'r5r' package is required to run this demo."
  )
})

test_that("r5r_gui_demo() attempts to launch a shiny app", {
  # Mock all external and slow calls made by r5r_gui_demo
  mockery::stub(r5r_gui_demo, 'r5r::build_network', TRUE)
  mockery::stub(r5r_gui_demo, 'system.file', "dummy/path")

  # --- THE CRITICAL FIX ---
  # Mock the `r5r_gui` function to return a valid shiny.appobj.
  # The server argument must be a function.
  mockery::stub(r5r_gui_demo, 'r5r_gui', function(...) {
    shiny::shinyApp(
      ui = shiny::fluidPage("mock UI"),
      server = function(input, output, session) {
        # An empty server function is valid
      }
    )
  })

  # The test now correctly checks for the class of the object returned
  # by the mocked r5r_gui call.
  expect_s3_class(r5r_gui_demo(), "shiny.appobj")
})
