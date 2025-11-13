## Resubmission

This is a resubmission. In this version, I have addressed the comments from the CRAN team:

*   Package and software names ('r5r', 'shiny') are now correctly quoted in the DESCRIPTION file.
*   A reference to the `r5r` package methodology has been added to the DESCRIPTION file as requested.
*   The package has been refactored to no longer modify the .GlobalEnv. The Shiny application now uses a "factory pattern" to pass arguments from the main function to the server logic, which is a much safer and more robust implementation. The tests have been updated accordingly.

All local `R CMD check --as-cran` tests pass without ERRORs, WARNINGs, or NOTEs.

Thank you for your time and feedback.
