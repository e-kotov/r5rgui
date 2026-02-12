# Changelog

## r5rgui (development version)

## r5rgui 0.2.0

- **New Compare Mode**: Added a toggle to switch between Normal and
  Compare modes, allowing users to compare two routes side-by-side with
  independent settings.
- **Multi-Graph Support**:
  [`r5r_gui()`](http://www.ekotov.pro/r5rgui/reference/r5r_gui.md) now
  accepts a named list of `r5r_network` objects, enabling comparison of
  different network scenarios (e.g., “Current” vs. “Future”).
- **Enhanced Visualization**:
  - Map legends now display routing execution times and summary
    statistics (total duration/distance).
  - In Compare Mode, two independent legends are shown in the top
    corners for easy comparison.
  - Route 1 and Route 2 use distinct color palettes (Blue/Orange
    vs. Green/Teal).
- **UI Improvements**:
  - Refactored layout into a 3-column design with collapsible and
    resizable sidebars.
  - Moved control buttons to a dedicated toolbar.
  - Repositioned notifications to the bottom-right to prevent overlap.
  - `mapgl` basemaps support
- **Reproducible Code**: Updated “Copy R Code” to generate script for
  both single and dual routing requests, correctly referencing specific
  network objects from the list.
- Added `Quit` button to exit the application.
- Improved error handling and notifications for routing failures.

## r5rgui 0.1.0

CRAN release: 2025-11-25

- `Copy R Code` button to copy the code to reproduce the exact route
  currently displayed on the map (2025-09-05)
