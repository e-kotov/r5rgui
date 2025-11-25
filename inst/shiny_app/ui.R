# --- UI DEFINITION (With notification styling) ---
ui <- shiny::fluidPage(
  # Wrap the header in a div with relative positioning
  shiny::div(
    style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
    # Flex container for logo and title
    shiny::div(
      style = "display: flex; align-items: center;",
      shiny::img(
        src = "r5rgui_assets/logo.png",
        height = "50px",
        style = "margin-right: 15px;"
      ),
      shiny::h2(
        shiny::HTML(
          "<b>r5rgui</b> - Interactive Routing with <code>{r5r}</code> and <code>{mapgl}</code>"
        ),
        style = "margin: 0;"
      )
    ),
    # Quit button is now a flex item and will be vertically centered
    shiny::actionButton(
      "quit_app",
      "Quit",
      style = "background-color: #0e8bb2; color: white; border-width: 0px;"
    )
  ),
  shiny::tags$head(
    # --- CSS for notifications and button positioning ---
    shiny::tags$style(shiny::HTML(
      "
      #shiny-notification-panel {
        top: 70px;
        right: 10px;
        left: auto;
        bottom: auto;
      }
      
      /* Wrapper to create a positioning context for the button */
      .map-wrapper {
        position: relative;
      }
      
      /* Style for the button placed on the map */
      /* This is now handled by an inline style on the container div */
      /* .map-wrapper .btn {
        position: absolute;
        bottom: 10px;
        left: 10px;
        z-index: 10; /* Ensures the button is on top of the map */
      } */
    "
    )),
    shiny::tags$script(shiny::HTML(
      # MODIFIED HERE
      "
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
          shiny::uiOutput("copy_code_message_ui"),
          shiny::actionButton("copy_code", "Copy R Code")
        )
      ),
      shiny::hr(),
      shiny::h4("Itinerary Details"),
      DT::dataTableOutput("itinerary_table")
    )
  )
)
