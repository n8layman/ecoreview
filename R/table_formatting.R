# DataTable formatting and styling functions

#' Create a styled DataTable with consistent options
#'
#' @param data Dataframe to display
#' @param height Table height (default: "600px")
#' @param page_length Number of rows per page (default: 15)
#' @return Styled DT::datatable object
#' @export
create_styled_datatable <- function(data, height = "600px", page_length = 15) {
  DT::datatable(data,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      scrollY = height,
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      columnDefs = list(list(className = 'dt-nowrap', targets = "_all")),
      dom = 'frtip',
      fixedColumns = FALSE
    ),
    rownames = FALSE,
    editable = list(target = 'cell'),
    selection = 'single',
    class = 'cell-border stripe compact'
  )
}

#' Apply deleted row styling to DataTable
#'
#' @param dt DataTable object
#' @param data Source dataframe (must contain deleted_by_human column)
#' @return DataTable with deleted row styling applied
#' @export
apply_deleted_row_styling <- function(dt, data) {
  if (!"deleted_by_human" %in% names(data)) return(dt)

  deleted_rows <- which(data$deleted_by_human == TRUE)
  if (length(deleted_rows) > 0) {
    dt <- dt %>% DT::formatStyle(
      columns = 1:ncol(data),
      target = "row",
      backgroundColor = DT::styleEqual(TRUE, "#f8d7da", default = ""),
      color = DT::styleEqual(TRUE, "#721c24", default = ""),
      valueColumns = "deleted_by_human"
    )
  }

  return(dt)
}

#' Apply restored interaction styling to DataTable
#'
#' @param dt DataTable object
#' @param data Source dataframe
#' @param restored_ids Vector of interaction IDs that are restored
#' @return DataTable with restored row styling applied
#' @export
apply_restored_row_styling <- function(dt, data, restored_ids) {
  if (length(restored_ids) == 0) return(dt)

  dt %>% DT::formatStyle(
    columns = "id",
    backgroundColor = DT::styleEqual(restored_ids, rep("#d1ecf1", length(restored_ids))),
    borderLeft = DT::styleEqual(restored_ids, rep("4px solid #17a2b8", length(restored_ids)))
  )
}

#' Apply edited cell styling to DataTable
#'
#' @param dt DataTable object
#' @param data Source dataframe
#' @param edited_cells Dataframe with interaction_id and column_name
#' @return DataTable with edited cell styling
#' @export
apply_edited_styling <- function(dt, data, edited_cells) {
  if (is.null(edited_cells) || nrow(edited_cells) == 0) return(dt)
  # For now just return dt - styling edits is complex
  return(dt)
}

#' Get all edited cells for styling
#'
#' @param document_id Document ID to get edits for
#' @param pending_edits List of pending edits not yet saved
#' @return Dataframe with interaction_id and column_name columns
#' @export
get_all_edited_cells <- function(document_id, pending_edits) {
  all_edited_cells <- data.frame(interaction_id = integer(), column_name = character(), stringsAsFactors = FALSE)

  # Add pending edits (filter out system operations)
  if (length(pending_edits) > 0) {
    human_edits <- pending_edits[sapply(pending_edits, function(x) {
      edit_type <- x$edit_type
      is.null(edit_type) || is.na(edit_type) || edit_type == "edit"
    })]

    if (length(human_edits) > 0) {
      pending_df <- data.frame(
        interaction_id = sapply(human_edits, function(x) x$interaction_id),
        column_name = sapply(human_edits, function(x) x$column_name),
        stringsAsFactors = FALSE
      )
      all_edited_cells <- rbind(all_edited_cells, pending_df)
    }
  }

  return(unique(all_edited_cells))
}

#' Get restored interaction IDs for styling
#'
#' @param document_id Document ID to check
#' @return Vector of interaction IDs that are restored
#' @export
get_restored_interaction_ids <- function(document_id) {
  # For now return empty - would need db_conn parameter
  return(integer(0))
}
