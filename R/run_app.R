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

#' Find project root by walking up from a starting directory
#'
#' Walks up the directory tree from \code{start_dir} until it finds a
#' directory containing a project marker (\code{.git}, \code{*.Rproj},
#' \code{DESCRIPTION}, \code{.here}, \code{.Rprofile}). Returns \code{NULL}
#' if no marker is found before the filesystem root.
#'
#' @param start_dir Directory to start from
#' @return Absolute path of the project root, or NULL
#' @keywords internal
find_project_root <- function(start_dir) {
  markers <- c(".git", ".here", ".Rprofile", "DESCRIPTION")
  dir <- normalizePath(start_dir, mustWork = FALSE)
  repeat {
    if (any(file.exists(file.path(dir, markers))) ||
        length(list.files(dir, pattern = "\\.Rproj$")) > 0) {
      return(dir)
    }
    parent <- dirname(dir)
    if (parent == dir) return(NULL)
    dir <- parent
  }
}

#' Resolve a PDF file path using a cascade of fallback strategies
#'
#' Given a path as stored in the database, tries in order:
#' \enumerate{
#'   \item The stored path as-is
#'   \item Relative to the directory containing the \code{.db} file
#'   \item Relative to the project root (found by walking up from the db dir)
#'   \item Relative to the working directory when \code{run_app()} was called
#'   \item \code{basename} resolved inside \code{pdf_dir} (explicit override)
#' }
#'
#' @param stored_path File path as stored in the database
#' @param db_conn Path to the \code{.db} file (used to anchor relative paths)
#' @param pdf_dir Optional explicit PDF directory override
#' @param user_wd Working directory captured at \code{run_app()} time
#' @return Resolved absolute path, or \code{NULL} if not found
#' @keywords internal
resolve_pdf_path <- function(stored_path, db_conn, pdf_dir = NULL,
                             user_wd = NULL) {
  if (is.null(stored_path) || !nzchar(stored_path)) return(NULL)

  try_path <- function(p) if (!is.null(p) && nzchar(p) && file.exists(p)) p else NULL

  db_dir <- if (!is.null(db_conn) && nzchar(db_conn)) dirname(db_conn) else NULL

  # 1. Explicit pdf_dir override (highest priority — user said exactly where PDFs are)
  if (!is.null(pdf_dir) && nzchar(pdf_dir)) {
    p <- try_path(file.path(pdf_dir, basename(stored_path)))
    if (!is.null(p)) return(normalizePath(p))
  }

  # 2. As stored
  if (!is.null(try_path(stored_path))) return(normalizePath(stored_path))

  # 3. Relative to db file location
  if (!is.null(db_dir)) {
    p <- try_path(file.path(db_dir, stored_path))
    if (!is.null(p)) return(normalizePath(p))
  }

  # 4. Relative to project root (walk up from db dir looking for .git / .Rproj etc.)
  if (!is.null(db_dir)) {
    root <- find_project_root(db_dir)
    if (!is.null(root)) {
      p <- try_path(file.path(root, stored_path))
      if (!is.null(p)) return(normalizePath(p))
    }
  }

  # 5. Relative to run_app() working directory
  if (!is.null(user_wd)) {
    p <- try_path(file.path(user_wd, stored_path))
    if (!is.null(p)) return(normalizePath(p))
  }

  NULL
}
