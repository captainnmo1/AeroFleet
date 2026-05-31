# run_app.R
# launches the AeroFleet shiny dashboard
# call AeroFleet::run_app() after installing the package to open the simulator


#' Launch the AeroFleet Shiny Dashboard
#'
#' Starts the interactive aircraft route profitability simulator.
#' Opens the dashboard in your default web browser.
#'
#' @return No return value, called for side effects
#' @export
run_app <- function() {

# find the app folder that was bundled inside the package
app_dir <- system.file("app", package = "AeroFleet")

# stop with a clear message if the folder cant be found
if (app_dir == "") {
stop("Could not find the app folder. Try reinstalling the package.")}

# launch the dashboard
shiny::runApp(app_dir, display.mode = "normal")}
