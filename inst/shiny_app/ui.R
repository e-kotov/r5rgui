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
    shiny::div(
      style = "display: none;",
      shiny::checkboxInput("compare_mode", NULL, value = FALSE)
    )
  ),

  shiny::tags$head(
    # --- CSS for notifications and layout ---
    shiny::tags$style(shiny::HTML(
      "
      #shiny-notification-panel {
        bottom: 20px;
        right: 10px;
        top: auto;
        left: auto;
      }
      
      .main-layout {
        display: flex;
        height: calc(100vh - 120px);
        gap: 0; /* Handled by resizers/margins */
        padding: 0 15px;
        overflow: hidden;
      }
      
      .sidebar-column {
        flex: 0 0 250px; /* Default width */
        overflow-y: auto;
        padding: 10px;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        min-width: 150px; /* Minimum width constraint */
        max-width: 600px;
      }
      
      /* Resizer Styles */
      .resizer {
        width: 10px;
        cursor: col-resize;
        background: transparent;
        z-index: 100;
        flex-shrink: 0;
        display: flex;
        justify-content: center;
        align-items: center;
        transition: background-color 0.2s;
        margin: 0 2px;
      }

      .resizer:hover, .resizer.resizing {
        background-color: #e9ecef;
      }
      
      /* Media Queries for narrower defaults */
      @media (max-width: 1200px) {
        .sidebar-column {
          flex-basis: 200px;
        }
      }
      
      @media (max-width: 992px) {
        .sidebar-column {
          flex-basis: 180px;
          font-size: 0.9em;
        }
      }
      
      .map-column {
        flex: 1;
        display: flex;
        flex-direction: column;
        min-width: 0;
      }

      .map-wrapper {
        position: relative;
        flex: 1;
      }
      
      #map {
        height: 100% !important;
      }
      
      .table-wrapper {
        height: 250px;
        overflow-y: auto;
        margin-top: 10px;
        border-top: 1px solid #eee;
      }
    "
    )),
    shiny::tags$script(shiny::HTML(
      "
    Shiny.addCustomMessageHandler('updateCompareMode', function(value) {
      $('#compare_mode').prop('checked', value).change();
    });

    // --- Resizer Logic ---
    $(document).on('shiny:connected', () => {
        // Event delegation for resizers (handles dynamic elements)
        document.addEventListener('mousedown', function(e) {
            if (e.target.classList.contains('resizer')) {
                const isLeft = e.target.id === 'resizer-left';
                // Select specific sidebar adjacent to the resizer
                const sidebar = isLeft 
                    ? document.querySelector('.sidebar-left') 
                    : document.querySelector('.sidebar-right');
                
                if (!sidebar) return;

                const startX = e.clientX;
                const startWidth = parseInt(window.getComputedStyle(sidebar).width, 10);
                
                const moveHandler = (e) => {
                    const dx = e.clientX - startX;
                    // For left sidebar: moving right (positive dx) increases width
                    // For right sidebar: moving right (positive dx) decreases width (since it's on the right)
                    // Wait, structure is: ResizerRight | SidebarRight. 
                    // Dragging ResizerRight to the right pushes the border right, which shrinks the sidebar.
                    // So: NewWidth = StartWidth - dx
                    
                    const newWidth = isLeft ? startWidth + dx : startWidth - dx;
                    
                    if (newWidth >= 150 && newWidth <= 600) {
                        sidebar.style.flexBasis = newWidth + 'px';
                        // Force map resize if needed (MapLibre usually handles container resize, but triggering event is safe)
                        window.dispatchEvent(new Event('resize')); 
                    }
                };

                const upHandler = () => {
                    document.removeEventListener('mousemove', moveHandler);
                    document.removeEventListener('mouseup', upHandler);
                    e.target.classList.remove('resizing');
                    document.body.style.removeProperty('user-select');
                    document.body.style.removeProperty('cursor');
                };

                document.addEventListener('mousemove', moveHandler);
                document.addEventListener('mouseup', upHandler);
                e.target.classList.add('resizing');
                document.body.style.userSelect = 'none';
                document.body.style.cursor = 'col-resize';
                e.preventDefault(); // Prevent text selection start
            }
        });
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

  shiny::div(
    class = "main-layout",

    # Left Sidebar: Route 1 / Normal
    shiny::div(
      class = "sidebar-column sidebar-left",
      shiny::h4(shiny::uiOutput("left_sidebar_title", inline = TRUE)),
      shiny::uiOutput("network_selector_1"),
      shiny::selectInput(
        "mode_1_internal", # We sync this with 'mode' or 'mode_1'
        "Transport Modes",
        choices = c(
          "WALK",
          "BICYCLE",
          "CAR",
          "BICYCLE_RENT",
          "CAR_PARK",
          "TRANSIT",
          "TRAM",
          "SUBWAY",
          "RAIL",
          "BUS",
          "FERRY",
          "CABLE_CAR",
          "GONDOLA",
          "FUNICULAR"
        ),
        selected = c("WALK", "TRANSIT"),
        multiple = TRUE
      ),
      shiny::dateInput(
        "departure_date_1_internal",
        "Departure Date",
        value = "2019-05-13"
      ),
      shiny::textInput(
        "departure_time_1_internal",
        "Departure Time (HH:MM)",
        value = "14:00"
      ),
      shiny::numericInput(
        "time_window_1_internal",
        "Time Window (min)",
        value = 10,
        min = 1,
        max = 180
      ),
      shiny::numericInput(
        "max_walk_time_1_internal",
        "Max Walk Time (min)",
        value = 15,
        min = 1,
        max = 120
      ),
      shiny::numericInput(
        "max_trip_duration_1_internal",
        "Max Trip Duration (min)",
        value = 120,
        min = 5,
        max = 300
      ),
      shiny::hr(),
      shiny::h4("Route Selection"),
      shiny::helpText("Left-click: Start. Right-click: End."),
      shiny::actionButton(
        "reset",
        "Reset Points",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      shiny::textInput(
        "start_coords_input",
        "Start (Lat, Lon)",
        placeholder = "-30.03, -51.22"
      ),
      shiny::textInput(
        "end_coords_input",
        "End (Lat, Lon)",
        placeholder = "-30.05, -51.18"
      )
    ),

    # Left Resizer
    shiny::div(id = "resizer-left", class = "resizer"),

    # Center Column: Map and Tables
    shiny::div(
      class = "map-column",
      shiny::div(
        class = "map-wrapper",
        mapgl::maplibreOutput("map"),
        shiny::div(
          style = "position: absolute; bottom: 10px; left: 10px; z-index: 10; display: flex; flex-direction: column; align-items: flex-start;",
          # Exec time is now in legends, but keep message and copy code here
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
              shiny::div(style = "padding-top:10px;"),
              DT::dataTableOutput("itinerary_table_1")
            ),
            shiny::tabPanel(
              "Itinerary 2",
              shiny::div(style = "padding-top:10px;"),
              DT::dataTableOutput("itinerary_table_2")
            )
          )
        )
      )
    ),

    # Right Sidebar: Route 2 (Only in Compare Mode)
    shiny::conditionalPanel(
      condition = "input.compare_mode",
      style = "display: flex; flex: 0 0 auto;", # Make wrapper a flex container so items align

      # Right Resizer (Inside conditional panel so it appears with sidebar)
      shiny::div(id = "resizer-right", class = "resizer"),

      shiny::div(
        class = "sidebar-column sidebar-right",
        shiny::h4("Route 2 Settings"),
        shiny::uiOutput("network_selector_2"),
        shiny::selectInput(
          "mode_2_internal",
          "Transport Modes",
          choices = c(
            "WALK",
            "BICYCLE",
            "CAR",
            "BICYCLE_RENT",
            "CAR_PARK",
            "TRANSIT",
            "TRAM",
            "SUBWAY",
            "RAIL",
            "BUS",
            "FERRY",
            "CABLE_CAR",
            "GONDOLA",
            "FUNICULAR"
          ),
          selected = c("CAR"),
          multiple = TRUE
        ),
        shiny::dateInput(
          "departure_date_2_internal",
          "Departure Date",
          value = "2019-05-13"
        ),
        shiny::textInput(
          "departure_time_2_internal",
          "Departure Time (HH:MM)",
          value = "14:00"
        ),
        shiny::numericInput(
          "time_window_2_internal",
          "Time Window (min)",
          value = 10,
          min = 1,
          max = 180
        ),
        shiny::numericInput(
          "max_walk_time_2_internal",
          "Max Walk Time (min)",
          value = 15,
          min = 1,
          max = 120
        ),
        shiny::numericInput(
          "max_trip_duration_2_internal",
          "Max Trip Duration (min)",
          value = 120,
          min = 5,
          max = 300
        )
      )
    )
  )
)
