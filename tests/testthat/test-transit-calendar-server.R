# Exercise the calendar messages without loading or routing a real network.
calendar_server <- function(calendars) {
  factory <- source(
    system.file("shiny_app", "server.R", package = "r5rgui"),
    local = TRUE
  )$value
  factory(list(
    r5r_network = stats::setNames(rep(list("dummy"), length(calendars)), names(calendars)),
    r5r_network_name = names(calendars)[1],
    center = c(0, 0), zoom = 10,
    departure_date = as.Date("2025-01-10"),
    transit_availability = calendars
  ))
}

test_that("calendar messages bound active service and preserve interior gaps", {
  calendar <- data.frame(
    date = as.Date(c("2025-04-10", "2025-01-09", "2025-02-10", "2025-01-10", "2025-04-11")),
    active_services = c(8L, 0L, 0L, 1L, 0L),
    total_services = 10L,
    pct_active = c(0.8, 0, 0, 0.1, 0)
  )
  shiny::testServer(calendar_server(list(first = calendar, second = NULL)), {
    payload <- calendar_payload("first")
    expect_identical(payload$minDate, "2025-01-10")
    expect_identical(payload$maxDate, "2025-04-10")
    expect_identical(payload$dates, format(calendar$date, "%Y-%m-%d"))
    gap <- match("2025-02-10", payload$dates)
    expect_identical(payload$active[gap], 0L)
    expect_identical(payload$pct[gap], 0)
    expect_identical(payload$total, rep(10L, 5))

    messages <- list()
    session$sendCustomMessage <- function(type, message) {
      if (type == "updateTransitAvailability") messages[[length(messages) + 1L]] <<- message
    }
    session$setInputs(network_1_internal = "second", network_2_internal = "first")
    expect_true(any(vapply(messages, function(message) {
      identical(message$inputId, "departure_date_1_internal") &&
        identical(message$calendar$dates, character()) &&
        is.null(message$calendar$minDate) && is.null(message$calendar$maxDate)
    }, logical(1))))
    expect_true(any(vapply(messages, function(message) {
      identical(message$inputId, "departure_date_2_internal") &&
        identical(message$calendar, payload)
    }, logical(1))))
  })
})

test_that("missing, empty and zero-service calendars have no service bounds", {
  empty <- data.frame(
    date = as.Date(character()), active_services = integer(),
    total_services = integer(), pct_active = numeric()
  )
  zero <- data.frame(
    date = as.Date("2025-01-10"), active_services = 0L,
    total_services = 10L, pct_active = 0
  )
  shiny::testServer(calendar_server(list(missing = NULL, empty = empty, zero = zero)), {
    for (name in c("missing", "empty")) {
      payload <- calendar_payload(name)
      expect_identical(payload$dates, character())
      expect_identical(payload$active, integer())
      expect_null(payload$minDate)
      expect_null(payload$maxDate)
    }
    payload <- calendar_payload("zero")
    expect_identical(payload$dates, "2025-01-10")
    expect_identical(payload$active, 0L)
    expect_null(payload$minDate)
    expect_null(payload$maxDate)
  })
})
