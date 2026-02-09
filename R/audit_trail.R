# Audit trail database operations

#' Get audit trail for document
#'
#' @param document_id Document ID
#' @param db_conn Database connection path (optional, uses global if not provided)
#' @return Dataframe of audit entries
#' @export
get_audit_trail <- function(document_id, db_conn = NULL) {
  # Try to get db_conn from parent environment if not provided
  if (is.null(db_conn)) {
    # Return empty if no connection
    return(tibble::tibble())
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_conn)
  tryCatch({
    # Check if human_edits table exists
    tables <- DBI::dbListTables(con)
    if (!"human_edits" %in% tables) {
      return(tibble::tibble())
    }

    result <- DBI::dbGetQuery(con, "
      SELECT he.*, i.occurrence_id
      FROM human_edits he
      JOIN interactions i ON he.interaction_id = i.id
      WHERE i.document_id = ?
      ORDER BY he.edit_timestamp DESC
    ", params = list(document_id))
    return(tibble::as_tibble(result))
  }, error = function(e) {
    return(tibble::tibble())
  }, finally = {
    DBI::dbDisconnect(con)
  })
}

#' Process delete with renumbering
#'
#' @param interaction_id ID of interaction to delete
#' @param document_id Document ID
#' @param db_conn Database connection path
#' @return List with success status and renumber results
#' @export
process_delete_with_renumbering <- function(interaction_id, document_id, db_conn = NULL) {
  if (is.null(db_conn)) {
    return(list(success = FALSE, message = "No database connection"))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_conn)
  tryCatch({
    # Get the interaction being deleted
    deleted_interaction <- DBI::dbGetQuery(con, "
      SELECT * FROM interactions WHERE id = ?
    ", params = list(interaction_id))

    if (nrow(deleted_interaction) == 0) {
      return(list(success = FALSE, message = "Interaction not found"))
    }

    # Mark as deleted
    DBI::dbExecute(con, "
      UPDATE interactions
      SET deleted_by_human = 1, deleted_at = ?
      WHERE id = ?
    ", params = list(as.character(Sys.time()), interaction_id))

    return(list(
      success = TRUE,
      message = "Interaction deleted",
      renumber_result = list(audit_entries = list())
    ))
  }, error = function(e) {
    return(list(success = FALSE, message = e$message))
  }, finally = {
    DBI::dbDisconnect(con)
  })
}
