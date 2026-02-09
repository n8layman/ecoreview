# File handling utilities for Shiny app

#' Create temporary PDF file for web display
#'
#' @param source_path Source PDF path
#' @param session_token Session token for unique filename
#' @return Path to temporary file in www/ directory
#' @export
create_temp_pdf_for_display <- function(source_path, session_token) {
  # Check source file exists
  if (!file.exists(source_path)) {
    warning("Source PDF not found: ", source_path)
    return(list(temp_path = NULL, web_path = NULL, error = "Source file not found"))
  }

  # Ensure www directory exists
  if (!dir.exists("www")) dir.create("www", recursive = TRUE)

  # Create unique temporary filename
  temp_filename <- paste0("temp_", session_token, ".pdf")
  temp_path <- file.path("www", temp_filename)

  # Copy file to temp location
  copy_success <- file.copy(source_path, temp_path, overwrite = TRUE)

  if (!copy_success) {
    warning("Failed to copy PDF from ", source_path, " to ", temp_path)
    return(list(temp_path = NULL, web_path = NULL, error = "Copy failed"))
  }

  return(list(
    temp_path = temp_path,
    web_path = temp_filename,
    error = NULL
  ))
}

#' Clean up old temporary files
#'
#' @param current_session_token Current session token to preserve
#' @export
cleanup_temp_files <- function(current_session_token = NULL) {
  if (!dir.exists("www")) return()

  # Find all temp PDF files
  temp_files <- list.files("www", pattern = "^temp_.*\\.pdf$", full.names = TRUE)

  if (!is.null(current_session_token)) {
    # Exclude current session file
    current_session_file <- paste0("temp_", current_session_token, ".pdf")
    temp_files <- temp_files[!grepl(current_session_file, temp_files)]
  }

  if (length(temp_files) > 0) {
    files_removed <- sum(file.remove(temp_files[file.exists(temp_files)]))
    message("Cleaned up ", files_removed, " temporary PDF files")
  }
}
