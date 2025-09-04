# --- UI DEFINITION (With notification styling) ---
ui <- shiny::fluidPage(
  shiny::titlePanel("R5GUI - powered by {r5r}/R5 and {mapgl}"),
  tags$head(
    # --- CSS for notifications and button positioning ---
    tags$style(shiny::HTML(
      "
      #shiny-notification-panel {
        top: 10px;
        right: 10px;
        left: auto;
        bottom: auto;
      }
      
      /* Wrapper to create a positioning context for the button */
      .map-wrapper {
        position: relative;
      }
      
      /* Style for the button placed on the map */
      .map-wrapper .btn {
        position: absolute;
        bottom: 10px;
        left: 10px;
        z-index: 10; /* Ensures the button is on top of the map */
      }
    "
    )),
    tags$script(shiny::HTML(
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
      h4("Trip Parameters"),
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
      h4("Route Selection"),
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
      tags$style(type = "text/css", "#map {height: calc(60vh) !important;}"),
      # --- NEW: Wrapper div for map and button ---
      shiny::div(
        class = "map-wrapper",
        mapgl::maplibreOutput("map"),
        shiny::actionButton("copy_code", "Copy R Code")
      ),
      shiny::hr(),
      h4("Itinerary Details"),
      DT::dataTableOutput("itinerary_table")
    )
  )
)
