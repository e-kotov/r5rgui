# Use the production UI (and its real Shiny datepicker), with no routing/network.
ui <- source("calendar-ui.R", local = TRUE)$value
ui <- shiny::tagList(ui, shiny::actionButton("clear_calendar", "Clear calendar"))

server <- function(input, output, session) {
  dates <- seq(as.Date("2025-01-10"), as.Date("2025-04-10"), by = "day")
  active <- integer(length(dates))
  active[c(1, 2, 3, length(active))] <- c(1L, 2L, 8L, 8L)
  calendar <- list(
    dates = format(dates, "%Y-%m-%d"), active = active,
    total = rep(10L, length(dates)), pct = active / 10,
    minDate = "2025-01-10", maxDate = "2025-04-10"
  )
  session$onFlushed(function() {
    shiny::updateDateInput(session, "departure_date_1_internal", value = as.Date("2025-01-10"))
    session$sendCustomMessage("updateTransitAvailability", list(
      inputId = "departure_date_1_internal", calendar = calendar
    ))
  }, once = TRUE)
  shiny::observeEvent(input$clear_calendar, {
    session$sendCustomMessage("updateTransitAvailability", list(
      inputId = "departure_date_1_internal", calendar = list(dates = character())
    ))
  })
}
shiny::shinyApp(ui, server)
