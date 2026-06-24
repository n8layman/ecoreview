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
#' @param priority_cols Character vector of column names to display first (left-most).
#'   Columns not present in the data are silently ignored. All other columns follow
#'   in their original order. NULL (default) uses the data frame column order.
#' @param visible_cols Character vector of column names to show. All other columns
#'   are hidden from the table display (the underlying data is unchanged). NULL
#'   (default) shows all columns.
#' @param pdf_dir Optional path to the directory containing PDF files. Use
#'   when the `.db` file is stored separately from the PDFs or when the app
#'   is launched from a different working directory. If the path stored in
#'   the database does not resolve, the app falls back to looking for the
#'   file's basename here. NULL (default) uses the path stored in the database.
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches the Shiny application (does not return)
#'
#' @examples
#' \dontrun{
#' # Run with defaults
#' run_app()
#'
#' # Customize for a specific project with priority and visible columns
#' run_app(
#'   title = "ChiroScan: Bat Interaction Review",
#'   app_name = "ChiroScan",
#'   github_url = "https://github.com/n8layman/bat-interactions",
#'   export_prefix = "bat_interactions",
#'   priority_cols = c("Pathogen_Name", "Host_Name",
#'                     "Detection_Result_Direction", "Observation_Type"),
#'   visible_cols  = c("Pathogen_Name", "Host_Name",
#'                     "Detection_Result_Direction", "Observation_Type",
#'                     "Host_Species", "Pathogen_Species")
#' )
#' }
#'
#' @export
run_app <- function(
  title = "EcoReview: Data Validation",
  app_name = "EcoReview",
  github_url = "https://github.com/n8layman/ecoreview",
  export_prefix = "ecoextract",
  priority_cols = NULL,
  visible_cols = NULL,
  pdf_dir = NULL,
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
    ecoreview.user_working_dir = user_wd,
    ecoreview.priority_cols = priority_cols,
    ecoreview.visible_cols = visible_cols,
    ecoreview.pdf_dir = pdf_dir
  )

  # Find the app directory
  app_dir <- system.file("app", package = "ecoreview")

  if (app_dir == "") {
    stop("Could not find app directory. Make sure ecoreview is installed correctly.")
  }

  shiny::runApp(app_dir, launch.browser = TRUE, ...)
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
