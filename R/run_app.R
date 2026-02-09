# Main entry point for ecoreview Shiny application

#' Run the ecoreview Shiny application
#'
#' Launches an interactive Shiny interface for reviewing and validating
#' ecological data extracted with ecoextract.
#'
#' @param title Application title displayed in the browser tab and header
#' @param app_name Short name for the application (used in export filenames)
#' @param github_url Optional GitHub repository URL for source link. Set to NULL to hide.
#' @param export_prefix Prefix for exported CSV filenames (default: "ecoextract")
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches the Shiny application (does not return)
#'
#' @examples
#' \dontrun{
#' # Run with defaults
#' run_app()
#'
#' # Customize for a specific project
#' run_app(
#'   title = "ChiroScan: Bat Interaction Review",
#'   app_name = "ChiroScan",
#'   github_url = "https://github.com/n8layman/bat-interactions",
#'   export_prefix = "bat_interactions"
#' )
#' }
#'
#' @export
run_app <- function(
  title = "EcoReview: Data Validation",
  app_name = "EcoReview",
  github_url = "https://github.com/n8layman/ecoreview",
  export_prefix = "ecoextract",
  ...
) {
  # Store configuration in options for the app to access
  # Capture user's working directory before app changes it
  user_wd <- getwd()

  options(
    ecoreview.title = title,
    ecoreview.app_name = app_name,
    ecoreview.github_url = github_url,
    ecoreview.export_prefix = export_prefix,
    ecoreview.user_working_dir = user_wd
  )

 # Find the app directory
  app_dir <- system.file("app", package = "ecoreview")

  if (app_dir == "") {
    stop("Could not find app directory. Make sure ecoreview is installed correctly.")
  }

  shiny::runApp(app_dir, ...)
}

#' Get ecoreview configuration option
#'
#' Helper function to retrieve configuration options set by run_app()
#'
#' @param name Option name (without "ecoreview." prefix)
#' @param default Default value if option is not set
#' @return The option value or default
#' @keywords internal
get_ecoreview_option <- function(name, default = NULL) {
  getOption(paste0("ecoreview.", name), default)
}
