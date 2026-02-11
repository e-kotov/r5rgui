# --- UI DEFINITION (With notification styling) ---
ui <- shiny::fluidPage(
  # Row 1: App Title
  shiny::div(
    style = "padding: 10px 0; border-bottom: 1px solid #eee;",
    shiny::div(
      style = "display: flex; align-items: center;",
      shiny::img(
        src = "r5rgui_assets/logo.png",
        height = "40px",
        style = "margin-right: 15px;"
      ),
      shiny::h3(
        shiny::HTML(
          "<b>r5rgui</b> - Interactive Routing with <code>{r5r}</code>"
        ),
        style = "margin: 0;"
      )
    )
  ),
  
  # Row 2: Control Buttons
  shiny::div(
    style = "display: flex; justify-content: flex-end; align-items: center; padding: 10px 0; gap: 10px;",
    shiny::uiOutput("compare_mode_ui"),
    shiny::actionButton(
      "quit_app",
      "Quit",
      style = "background-color: #d9534f; color: white; border-width: 0px;"
    ),
    # Hidden input for Compare Mode state
    shiny::div(style = "display: none;", shiny::checkboxInput("compare_mode", NULL, value = FALSE))
  ),

  shiny::tags$head(
    # --- CSS for notifications and button positioning ---
    shiny::tags$style(shiny::HTML(
      "
      #shiny-notification-panel {
        top: 120px;
        right: 10px;
        left: auto;
        bottom: auto;
      }
      
      .map-wrapper {
        position: relative;
      }
    "
    )),
    shiny::tags$script(shiny::HTML(
      "
    Shiny.addCustomMessageHandler('updateCompareMode', function(value) {
      $('#compare_mode').prop('checked', value).change();
    });

    function initializeMapListeners(mapId) {
      const mapElement = document.getElementById(mapId);
      if (!mapElement) return;

      const observer = new MutationObserver((mutations, obs) => {
        const map = mapElement.map;
        if (map) {
          map.on('contextmenu', (e) => {
            e.preventDefault();
            Shiny.setInputValue('js_right_click', {
              lng: e.lngLat.lng,
              lat: e.lngLat.lat,
              nonce: Math.random()
            });
          });

          let startMarker = null;
          let endMarker = null;

          Shiny.addCustomMessageHandler('updateMarker', function(message) {
            const lngLat = [message.lng, message.lat];
            const markerId = message.id;

            const createDragEndCallback = (id) => {
              return (marker) => {
                const coords = marker.getLngLat();
                Shiny.setInputValue('marker_dragged', {
                  id: id,
                  lng: coords.lng,
                  lat: coords.lat,
                  nonce: Math.random()
                });
              };
            };

            if (markerId === 'start') {
              if (!startMarker) {
                startMarker = new maplibregl.Marker({ draggable: true, color: '#009E73' })
                  .setLngLat(lngLat)
                  .addTo(map);
                startMarker.on('dragend', () => createDragEndCallback('start')(startMarker));
              } else {
                startMarker.setLngLat(lngLat);
              }
            } else if (markerId === 'end') {
              if (!endMarker) {
                endMarker = new maplibregl.Marker({ draggable: true, color: '#D55E00' })
                  .setLngLat(lngLat)
                  .addTo(map);
                endMarker.on('dragend', () => createDragEndCallback('end')(endMarker));
              } else {
                endMarker.setLngLat(lngLat);
              }
            }
          });

          Shiny.addCustomMessageHandler('clearAllMarkers', function(message) {
              if(startMarker) {
                  startMarker.remove();
                  startMarker = null;
              }
              if(endMarker) {
                  endMarker.remove();
                  endMarker = null;
              }
          });

          obs.disconnect();
        }
      });

      observer.observe(mapElement, { childList: true, subtree: true });
    }

    $(document).on('shiny:connected', () => {
      initializeMapListeners('map');
    });
    "
    ))
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Trip Parameters"),
      
      # Normal Mode Input
      shiny::conditionalPanel(
        condition = "!input.compare_mode",
        shiny::selectInput(
          "mode",
          "Transport Modes",
          choices = c(
            "WALK", "BICYCLE", "CAR", "BICYCLE_RENT", "CAR_PARK",
            "TRANSIT", "TRAM", "SUBWAY", "RAIL", "BUS", "FERRY",
            "CABLE_CAR", "GONDOLA", "FUNICULAR"
          ),
          selected = c("WALK", "TRANSIT"),
          multiple = TRUE
        )
      ),
      
      # Compare Mode Inputs
      shiny::conditionalPanel(
        condition = "input.compare_mode",
        shiny::tabsetPanel(
          id = "compare_tabs_sidebar",
          type = "pills",
          shiny::tabPanel(
            "Route 1",
            shiny::div(style = "margin-top: 10px;"),
            shiny::selectInput(
              "mode_1",
              "Transport Modes",
              choices = c(
                "WALK", "BICYCLE", "CAR", "BICYCLE_RENT", "CAR_PARK",
                "TRANSIT", "TRAM", "SUBWAY", "RAIL", "BUS", "FERRY",
                "CABLE_CAR", "GONDOLA", "FUNICULAR"
              ),
              selected = c("WALK", "TRANSIT"),
              multiple = TRUE
            ),
            shiny::dateInput("departure_date_1", "Departure Date", value = "2019-05-13"),
            shiny::textInput("departure_time_1", "Departure Time (HH:MM)", value = "14:00"),
            shiny::numericInput("time_window_1", "Time Window (min)", value = 10, min = 1, max = 180),
            shiny::numericInput("max_walk_time_1", "Max Walk Time (min)", value = 15, min = 1, max = 120),
            shiny::numericInput("max_trip_duration_1", "Max Trip Duration (min)", value = 120, min = 5, max = 300)
          ),
          shiny::tabPanel(
            "Route 2",
            shiny::div(style = "margin-top: 10px;"),
            shiny::selectInput(
              "mode_2",
              "Transport Modes",
              choices = c(
                "WALK", "BICYCLE", "CAR", "BICYCLE_RENT", "CAR_PARK",
                "TRANSIT", "TRAM", "SUBWAY", "RAIL", "BUS", "FERRY",
                "CABLE_CAR", "GONDOLA", "FUNICULAR"
              ),
              selected = c("CAR"),
              multiple = TRUE
            ),
            shiny::dateInput("departure_date_2", "Departure Date", value = "2019-05-13"),
            shiny::textInput("departure_time_2", "Departure Time (HH:MM)", value = "14:00"),
            shiny::numericInput("time_window_2", "Time Window (min)", value = 10, min = 1, max = 180),
            shiny::numericInput("max_walk_time_2", "Max Walk Time (min)", value = 15, min = 1, max = 120),
            shiny::numericInput("max_trip_duration_2", "Max Trip Duration (min)", value = 120, min = 5, max = 300)
          )
        )
      ),

      shiny::conditionalPanel(
        condition = "!input.compare_mode",
        shiny::dateInput(
          "departure_date",
          "Departure Date",
          value = "2019-05-13"
        ),
        shiny::textInput(
          "departure_time",
          "Departure Time (HH:MM)",
          value = "14:00"
        ),
        shiny::numericInput(
          "time_window",
          "Time Window (minutes)",
          value = 10,
          min = 1,
          max = 180
        ),
        shiny::numericInput(
          "max_walk_time",
          "Max Walk Time (minutes)",
          value = 15,
          min = 1,
          max = 120
        ),
        shiny::numericInput(
          "max_trip_duration",
          "Max Trip Duration (minutes)",
          value = 120,
          min = 5,
          max = 300
        )
      ),
      shiny::hr(),
      shiny::h4("Route Selection"),
      shiny::helpText(
        "Left-click to set start. Right-click to set end. Drag markers or edit coordinates below."
      ),
      shiny::actionButton(
        "reset",
        "Reset Start/End Points",
        style = "width: 100%;"
      ),
      shiny::textInput(
        "start_coords_input",
        "Start (Lat, Lon)",
        placeholder = "e.g., -30.03, -51.22"
      ),
      shiny::textInput(
        "end_coords_input",
        "End (Lat, Lon)",
        placeholder = "e.g., -30.05, -51.18"
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::tags$style(
        type = "text/css",
        "#map {height: calc(50vh) !important;}"
      ),
      # --- Wrapper div for map and button ---
      shiny::div(
        class = "map-wrapper",
        mapgl::maplibreOutput("map"),
        shiny::div(
          style = "position: absolute; bottom: 10px; left: 10px; z-index: 10; display: flex; flex-direction: column; align-items: flex-start;",
          shiny::uiOutput("exec_time_overlay_ui"),
          shiny::uiOutput("copy_code_message_ui"),
          shiny::actionButton("copy_code", "Copy R Code")
        )
      ),
      shiny::div(
        class = "table-wrapper",
        shiny::conditionalPanel(
          condition = "!input.compare_mode",
          shiny::h4("Itinerary Details"),
          DT::dataTableOutput("itinerary_table")
        ),
        shiny::conditionalPanel(
          condition = "input.compare_mode",
          shiny::tabsetPanel(
            id = "compare_tabs_tables",
            shiny::tabPanel(
              "Itinerary 1",
              shiny::div(style = "margin-top: 10px;"),
              DT::dataTableOutput("itinerary_table_1")
            ),
            shiny::tabPanel(
              "Itinerary 2",
              shiny::div(style = "margin-top: 10px;"),
              DT::dataTableOutput("itinerary_table_2")
            )
          )
        )
      )
    )
  )
)
