# Value conversion utilities for DataTable cell editing

#' Convert edited cell value to the appropriate type
#'
#' @param value The new value as a string from the cell edit
#' @param target_type The class of the target column
#' @return The converted value in the appropriate type
#' @export
convert_edit_value <- function(value, target_type) {
  if (is.null(value) || (is.character(value) && value == "")) {
    return(NA)
  }

  primary_type <- target_type[1]

  tryCatch({
    switch(primary_type,
      "integer" = as.integer(value),
      "numeric" = as.numeric(value),
      "double" = as.double(value),
      "logical" = tolower(as.character(value)) %in% c("true", "1", "yes", "t"),
      "Date" = as.Date(value),
      "POSIXct" = as.POSIXct(value),
      as.character(value)
    )
  }, error = function(e) as.character(value))
}
