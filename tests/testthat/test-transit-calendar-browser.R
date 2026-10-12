test_that("the real Shiny picker colors and disables days and bounds navigation", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  # Copy the production UI into an isolated fixture so the child R process tests
  # this checkout rather than a possibly older installed version of r5rgui.
  app_dir <- tempfile("transit-calendar-")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  file.copy(test_path("apps", "transit-calendar", "app.R"), app_dir)
  file.copy(
    system.file("shiny_app", "ui.R", package = "r5rgui"),
    file.path(app_dir, "calendar-ui.R")
  )
  app <- shinytest2::AppDriver$new(app_dir, name = "transit-calendar", load_timeout = 20000)
  on.exit(app$stop(), add = TRUE)
  app$run_js("window.calendarPicker = $('#departure_date_1_internal input').data('datepicker'); calendarPicker.show();")

  # Inspect actual rendered cells, including styles after Bootstrap's disabled
  # rules, rather than just checking that a CSS class name exists in source.
  cell <- function(date) {
    app$get_js(sprintf("(() => {
      const node = calendarPicker.picker.find('td.day[data-date=\"' + Date.parse('%sT00:00:00Z') + '\"]')[0];
      return {disabled: node.classList.contains('disabled'),
        level: Array.from(node.classList).find(value => value.startsWith('transit-')),
        background: getComputedStyle(node).backgroundColor, title: node.title};
    })()", date))
  }
  expect_equal(cell("2025-01-10")$level, "transit-low")
  expect_equal(cell("2025-01-11")$level, "transit-moderate")
  expect_equal(cell("2025-01-12")$level, "transit-high")
  expect_false(cell("2025-01-12")$disabled)
  expect_match(cell("2025-01-12")$title, "8/10 GTFS services active")
  gap <- cell("2025-01-13")
  expect_true(gap$disabled)
  expect_equal(gap$level, "transit-none")
  expect_equal(gap$background, "rgb(209, 213, 219)")
  expect_equal(cell("2025-01-10")$background, "rgb(222, 235, 247)")
  expect_equal(cell("2025-01-11")$background, "rgb(158, 202, 225)")
  expect_equal(cell("2025-01-12")$background, "rgb(33, 113, 181)")

  value <- app$get_value(input = "departure_date_1_internal")
  app$run_js("calendarPicker.picker.find('td.day[data-date=\"' + Date.parse('2025-01-13T00:00:00Z') + '\"]').click();")
  expect_identical(app$get_value(input = "departure_date_1_internal"), value)

  month <- function() app$get_js("calendarPicker.picker.find('.datepicker-days .datepicker-switch').text()")
  arrow_hidden <- function(arrow) app$get_js(sprintf(
    "getComputedStyle(calendarPicker.picker.find('.datepicker-days .%s')[0]).visibility === 'hidden'", arrow
  ))
  expect_equal(month(), "January 2025")
  expect_true(arrow_hidden("prev"))
  for (expected in c("February 2025", "March 2025", "April 2025")) {
    app$run_js("calendarPicker.picker.find('.datepicker-days .next').click();")
    expect_equal(month(), expected)
  }
  expect_true(arrow_hidden("next"))
  expect_false(cell("2025-04-10")$disabled)
  expect_true(cell("2025-04-11")$disabled)

  # Switching to a network without availability must clear the previous limits.
  app$click("clear_calendar")
  expect_false(arrow_hidden("prev"))
  expect_false(arrow_hidden("next"))
  expect_equal(month(), "January 2025")
  expect_false(cell("2025-01-13")$disabled)
  expect_null(cell("2025-01-10")$level)
})
