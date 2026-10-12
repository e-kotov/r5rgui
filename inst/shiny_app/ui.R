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
          "<b>r5rgui</b> - Interactive Routing with <code>{r5r}</code> and <code>{mapgl}</code>"
        ),
        style = "margin: 0;"
      )
    )
  ),

  # Row 2: Control Buttons
  shiny::div(
    style = "display: flex; justify-content: flex-end; align-items: center; padding: 10px 0; gap: 10px;",
    shiny::uiOutput("compare_mode_ui"),
    shiny::uiOutput("mapgl_3d_ui"),
    shiny::uiOutput("cube_mode_ui"),
    shiny::actionButton(
      "quit_app",
      "Quit",
      style = "background-color: #d9534f; color: white; border-width: 0px;"
    ),
    # Hidden inputs for Compare Mode and 3D Cube state
    shiny::div(
      style = "display: none;",
      shiny::checkboxInput("compare_mode", NULL, value = FALSE),
      shiny::checkboxInput("show_cube", NULL, value = FALSE),
      shiny::checkboxInput("mapgl_3d_mode", NULL, value = FALSE)
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

      /* --- Map Control Stacking (Top & Bottom Right) --- */
      
      /* Target both containers to ensure they don't stretch items */
      .maplibregl-ctrl-top-right, .maplibregl-ctrl-bottom-right {
        display: flex !important;
        flex-direction: column !important;
        align-items: flex-end !important;
        pointer-events: none;
      }
      
      .maplibregl-ctrl-top-right > *, .maplibregl-ctrl-bottom-right > * {
        pointer-events: auto;
      }

      /* Consistent grouping for all control blocks */
      .maplibregl-ctrl-group {
        margin-bottom: 5px !important; /* Small gap between groups */
        border-radius: 4px !important;
        box-shadow: 0 1px 2px rgba(0,0,0,0.1) !important;
      }
      
      /* In bottom-right, the groups (Zoom, Fullscreen) need to be pushed up to clear the basemap selector panel */
      .maplibregl-ctrl-bottom-right .maplibregl-ctrl-group {
        margin-bottom: 75px !important; /* Combined height of selector + padding */
      }
      
      /* Wait, if multiple groups exist, only the bottom-most needs the large margin */
      .maplibregl-ctrl-bottom-right .maplibregl-ctrl-group:last-of-type {
        margin-bottom: 75px !important;
      }
      
      /* Overriding the generic margin if it's not the last one */
      .maplibregl-ctrl-bottom-right .maplibregl-ctrl-group {
        margin-bottom: 5px !important;
      }
      .maplibregl-ctrl-bottom-right .maplibregl-ctrl-group:nth-last-child(2) {
        margin-bottom: 75px !important; /* The one above attribution */
      }

      /* Attribution stays at the very bottom - push it slightly off the edges */
      .maplibregl-ctrl-attrib {
        background: rgba(255, 255, 255, 0.7) !important;
        margin: 5px !important;
        border-radius: 4px !important;
      }

      /* Styles for the floating basemap selector */
      .basemap-panel {
        background: rgba(255, 255, 255, 0.9);
        padding: 2px 5px !important;
        border-radius: 4px;
        box-shadow: 0 1px 2px rgba(0,0,0,0.1);
        border: 1px solid #ccc;
      }
      
      .basemap-panel .form-group {
        margin-bottom: 0 !important; /* Kill the 'fat lip' */
      }
      
      .basemap-panel .selectize-control {
        margin-bottom: 0 !important;
      }
    "
    )),
    shiny::tags$script(shiny::HTML(
      "
    Shiny.addCustomMessageHandler('updateCompareMode', function(value) {
      $('#compare_mode').prop('checked', value).change();
    });

    Shiny.addCustomMessageHandler('updateCubeMode', function(value) {
      $('#show_cube').prop('checked', value).change();
      // Trigger resize after layout updates
      setTimeout(function() {
        window.dispatchEvent(new Event('resize'));
        // Trigger resize twice to ensure Maplibre finishes loading
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 150);
      }, 100);
    });

    // --- Sidebar Persistence & Resizer Logic ---
    $(document).on('shiny:connected', () => {
        const loadWidths = () => {
            const leftWidth = localStorage.getItem('r5rgui-left-width');
            const rightWidth = localStorage.getItem('r5rgui-right-width');
            
            if (leftWidth) {
                const sidebar = document.querySelector('.sidebar-left');
                if (sidebar) {
                    sidebar.style.flexBasis = leftWidth + 'px';
                    sidebar.style.width = leftWidth + 'px';
                }
            }
            if (rightWidth) {
                const sidebar = document.querySelector('.sidebar-right');
                if (sidebar) {
                    sidebar.style.flexBasis = rightWidth + 'px';
                    sidebar.style.width = rightWidth + 'px';
                }
            }
            window.dispatchEvent(new Event('resize'));
        };

        // MutationObserver to catch when the right sidebar appears
        const observer = new MutationObserver((mutations) => {
            mutations.forEach((mutation) => {
                if (mutation.type === 'childList') {
                    const rightSidebar = document.querySelector('.sidebar-right');
                    if (rightSidebar) {
                        const savedWidth = localStorage.getItem('r5rgui-right-width');
                        if (savedWidth) {
                            rightSidebar.style.flexBasis = savedWidth + 'px';
                            rightSidebar.style.width = savedWidth + 'px';
                        }
                    }
                }
            });
        });
        observer.observe(document.querySelector('.main-layout'), { childList: true, subtree: true });

        loadWidths();

        document.addEventListener('mousedown', function(e) {
            if (e.target.classList.contains('resizer')) {
                const isLeft = e.target.id === 'resizer-left';
                const sidebar = isLeft 
                    ? document.querySelector('.sidebar-left') 
                    : document.querySelector('.sidebar-right');
                
                if (!sidebar) return;

                const startX = e.clientX;
                const startWidth = sidebar.offsetWidth;
                
                const moveHandler = (e) => {
                    const dx = e.clientX - startX;
                    const newWidth = isLeft ? startWidth + dx : startWidth - dx;
                    if (newWidth >= 150 && newWidth <= 600) {
                        sidebar.style.flexBasis = newWidth + 'px';
                        sidebar.style.width = newWidth + 'px';
                        window.dispatchEvent(new Event('resize')); 
                    }
                };

                const upHandler = () => {
                    document.removeEventListener('mousemove', moveHandler);
                    document.removeEventListener('mouseup', upHandler);
                    const finalWidth = sidebar.offsetWidth;
                    localStorage.setItem(isLeft ? 'r5rgui-left-width' : 'r5rgui-right-width', finalWidth);
                    e.target.classList.remove('resizing');
                    document.body.style.removeProperty('user-select');
                    document.body.style.removeProperty('cursor');
                };

                document.addEventListener('mousemove', moveHandler);
                document.addEventListener('mouseup', upHandler);
                e.target.classList.add('resizing');
                document.body.style.userSelect = 'none';
                document.body.style.cursor = 'col-resize';
                e.preventDefault();
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
          
          Shiny.addCustomMessageHandler('setPersistentStyle', function(message) {
            const mapElement = document.getElementById('map');
            if (!mapElement || !mapElement.map) return;
            const map = mapElement.map;
            
            const newStyleUrl = message.styleUrl;
            
            fetch(newStyleUrl)
              .then(response => response.json())
              .then(newStyle => {
                const currentStyle = map.getStyle();
                
                // Identify layers to preserve (specifically route layers)
                const layersToPreserve = currentStyle.layers.filter(l => 
                  l.id && (l.id.startsWith('route') || l.id === 'route_layer_1' || l.id === 'route_layer_2')
                );
                
                if (layersToPreserve.length === 0) {
                   map.setStyle(newStyle, { diff: true });
                   return;
                }

                // Identify sources used by these layers
                const sourcesToPreserve = {};
                layersToPreserve.forEach(l => {
                  if (l.source && currentStyle.sources[l.source]) {
                    sourcesToPreserve[l.source] = currentStyle.sources[l.source];
                  }
                });
                
                // Merge sources and layers
                newStyle.sources = Object.assign({}, newStyle.sources, sourcesToPreserve);
                newStyle.layers = newStyle.layers.concat(layersToPreserve);
                
                // Apply the new style with diff=true
                map.setStyle(newStyle, { diff: true });
                
              })
              .catch(err => {
                console.error('Error fetching/setting persistent style:', err);
                map.setStyle(newStyleUrl, { diff: true });
              });
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
      ),
      shiny::hr(),
      shiny::h4("3D Cube Options"),
      shiny::checkboxInput(
        "show_3d_isochrones",
        "Calculate reachability isochrones at transfers",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = "input.show_3d_isochrones",
        shiny::numericInput(
          "isochrone_cutoff_3d",
          "Isochrone Cutoff (min)",
          value = 15,
          min = 1,
          max = 60
        )
      ),
      shiny::checkboxInput(
        "show_3d_access_surface",
        "Show Accessibility raster planes at stops",
        value = FALSE
      ),

      # Accessibility settings shared by the plotly cube surfaces and the
      # MapGL extrusion cube. Choices are populated server-side from land_use.
      shiny::conditionalPanel(
        condition = "(input.show_3d_access_surface || input.mapgl_3d_mode) && !output.has_land_use",
        shiny::p(
          "No land use data available. Pass a grid with opportunity columns via the `land_use` argument of r5r_gui() to enable accessibility features.",
          class = "text-muted",
          style = "font-size: 0.9em; font-style: italic; margin-top: 10px;"
        )
      ),
      shiny::conditionalPanel(
        condition = "(input.show_3d_access_surface || input.mapgl_3d_mode) && output.has_land_use",
        shiny::h5("Accessibility Settings"),
        shiny::selectInput(
          "access_metric_3d",
          "Accessibility Metric",
          choices = c(
            "Cumulative Cutoff" = "cumulative",
            "Cumulative Interval" = "cumulative_interval",
            "Gravity (Exponential Decay)" = "gravity",
            "Cost to Closest" = "cost_to_closest",
            "Floating Catchment Area (2SFCA)" = "floating_catchment",
            "Spatial Availability" = "spatial_availability",
            "Balancing Cost" = "balancing_cost"
          ),
          selected = "cumulative"
        ),
        shiny::selectInput(
          "access_variable_3d",
          "Opportunity Variable",
          choices = NULL
        ),
        shiny::conditionalPanel(
          condition = "input.access_metric_3d == 'floating_catchment' || input.access_metric_3d == 'spatial_availability' || input.access_metric_3d == 'balancing_cost'",
          shiny::selectInput(
            "access_demand_3d",
            "Demand Variable",
            choices = NULL
          )
        ),
        shiny::conditionalPanel(
          condition = "input.access_metric_3d == 'cumulative' || input.access_metric_3d == 'cumulative_interval' || input.access_metric_3d == 'floating_catchment'",
          shiny::sliderInput(
            "access_cutoff_3d",
            "Accessibility Cutoff (min)",
            value = 30,
            min = 5,
            max = 60,
            step = 5
          )
        ),
        shiny::conditionalPanel(
          condition = "input.access_metric_3d == 'cost_to_closest'",
          shiny::numericInput(
            "access_n_closest_3d",
            "Number of Opportunities (N)",
            value = 1,
            min = 1,
            max = 50
          )
        )
      ),

      # MapGL extrusion cube: explicit compute button + temporal resolution
      shiny::conditionalPanel(
        condition = "input.mapgl_3d_mode && output.has_land_use",
        shiny::tags$hr(),
        shiny::h5("MapGL Space-Time Cube"),
        shiny::sliderInput(
          "cube_hours",
          "Departure Hours",
          min = 0,
          max = 23,
          value = c(0, 23),
          step = 1
        ),
        shiny::selectInput(
          "cube_hour_step",
          "Hour Step",
          choices = c("Every hour" = 1, "Every 2 hours" = 2, "Every 3 hours" = 3),
          selected = 1
        ),
        shiny::actionButton(
          "compute_mapgl_cube",
          "Compute Space-Time Cube",
          style = "width: 100%; margin-bottom: 10px; background-color: #21918c; color: white; border-width: 0px;"
        ),
        shiny::p(
          "Uses the transport modes and departure date from Trip Parameters. Results are cached per parameter set.",
          class = "text-muted",
          style = "font-size: 0.85em; font-style: italic;"
        )
      ),

      # Slicing controls apply to whichever cube is active
      shiny::conditionalPanel(
        condition = "input.mapgl_3d_mode || input.show_cube",
        shiny::tags$hr(),
        shiny::h5("Volume Slicing"),
        shiny::sliderInput(
          "slice_z",
          "Time Slice Range (Hour)",
          min = 0,
          max = 23,
          value = c(0, 23),
          step = 1,
          animate = shiny::animationOptions(interval = 1200, loop = TRUE)
        ),
        shiny::conditionalPanel(
          condition = "!input.mapgl_3d_mode && input.show_cube",
          shiny::sliderInput("slice_y", "N-S Slice % (Vertical)", min = 0, max = 100, value = c(0, 100), step = 1),
          shiny::sliderInput("slice_x", "E-W Slice % (Horizontal)", min = 0, max = 100, value = c(0, 100), step = 1)
        ),
        shiny::conditionalPanel(
          condition = "input.mapgl_3d_mode",
          shiny::p("Use the rectangle tool (click and drag in one motion) or the polygon tool on the map to slice the cube spatially; delete the shape to reset.", class = "text-muted", style = "font-size: 0.9em; font-style: italic; margin-top: 10px;")
        )
      )
    ),

    # Left Resizer
    shiny::div(id = "resizer-left", class = "resizer"),

    # Center Column: Map and Tables
    shiny::div(
      class = "map-column",
      shiny::div(
        class = "map-wrapper",
        style = "display: flex; flex-direction: row; height: 100%; width: 100%; position: relative;",
        # Map Container
        shiny::div(
          id = "map_container",
          style = "height: 100%; position: relative; flex: 1; min-width: 0;",
          mapgl::maplibreOutput("map"),
          shiny::div(
            style = "position: absolute; bottom: 35px; left: 10px; z-index: 10; display: flex; flex-direction: column; align-items: flex-start;",
            # Exec time is now in legends, but keep message and copy code here
            shiny::uiOutput("copy_code_message_ui"),
            shiny::actionButton("copy_code", "Copy R Code")
          ),
          # Basemap Selector
          shiny::absolutePanel(
            bottom = 40,
            right = 10,
            width = "auto",
            draggable = FALSE,
            style = "z-index: 500;",
            shiny::div(class = "basemap-panel", shiny::uiOutput("basemap_ui"))
          )
        ),
        # 3D Space-Time Cube Container
        shiny::conditionalPanel(
          condition = "input.show_cube",
          style = "flex: 1; height: 100%; border-left: 1px solid #ccc; background: white; overflow: hidden; position: relative; min-width: 0;",
          shiny::conditionalPanel(
            condition = "!input.compare_mode",
            style = "height: 100%;",
            plotly::plotlyOutput("space_time_cube", height = "100%")
          ),
          shiny::conditionalPanel(
            condition = "input.compare_mode",
            style = "height: 100%;",
            shiny::tabsetPanel(
              id = "compare_cube_tabs",
              shiny::tabPanel(
                "Route 1 Cube",
                shiny::div(style = "padding-top:5px; height: 100%;"),
                plotly::plotlyOutput("space_time_cube_1", height = "100%")
              ),
              shiny::tabPanel(
                "Route 2 Cube",
                shiny::div(style = "padding-top:5px; height: 100%;"),
                plotly::plotlyOutput("space_time_cube_2", height = "100%")
              )
            )
          )
        )
      ),
      shiny::div(
        class = "table-wrapper",
        shiny::conditionalPanel(
          condition = "!input.compare_mode",
          shiny::div(
            style = "padding: 10px 0 0 0;",
            DT::dataTableOutput("itinerary_table")
          )
        ),
        shiny::conditionalPanel(
          condition = "input.compare_mode",
          shiny::tabsetPanel(
            id = "compare_tabs_tables",
            shiny::tabPanel(
              "Route 1 Details",
              shiny::div(style = "padding-top:10px;"),
              DT::dataTableOutput("itinerary_table_1")
            ),
            shiny::tabPanel(
              "Route 2 Details",
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
