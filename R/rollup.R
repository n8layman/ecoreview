# Rolled-up view of records: group within a document and sum quantities

#' Roll up records by key columns
#'
#' Groups each document's records by caller-supplied key columns and sums the
#' named numeric columns. The granular records are not changed; the result is
#' a read-only summary computed from them. Nothing is assumed about the
#' extraction schema beyond the columns named here.
#'
#' For each group, columns are filled as follows:
#' \itemize{
#'   \item \code{document_id} and \code{by} columns: the group's values.
#'   \item \code{sum} columns: the sum of the members' values (\code{NA} only
#'     when every member is \code{NA}). A \code{<col>_values} column follows
#'     each one, holding every member's value as a JSON array (\code{null}
#'     for a missing value), in member order.
#'   \item Every other column: the members' distinct non-missing values as a
#'     JSON array, \code{NA} when there are none. Values that are themselves
#'     JSON arrays (such as the evidence column) are merged element by
#'     element, so the evidence column holds the union of every member's
#'     evidence and highlighting still finds every source line.
#'   \item \code{n_records}, added last: the number of records in the group.
#' }
#' Columns keep their order in \code{records}. Soft-deleted records
#' (non-missing \code{deleted_by_user}) are left out, and the \code{id} and
#' \code{deleted_by_user} columns are dropped.
#'
#' @param records Data frame of records, as returned by
#'   \code{ecoextract::get_records()}.
#' @param by Character vector of columns that define a group within a document.
#' @param sum Character vector of numeric columns to sum within a group.
#' @return A data frame with one row per group, in order of each group's first
#'   record. Attribute \code{"member_rows"} is a list giving, for each group, the
#'   row numbers of its members in \code{records}.
#' @export
rollup_records <- function(records, by, sum = character(0)) {
  by <- as.character(by)
  sum <- setdiff(as.character(sum), by)
  missing_cols <- setdiff(c(by, sum), names(records))
  if (length(missing_cols) > 0) {
    stop("Columns not found in records: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  non_numeric <- sum[!vapply(records[sum], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("Columns to sum must be numeric: ", paste(non_numeric, collapse = ", "),
         call. = FALSE)
  }

  records <- as.data.frame(records, stringsAsFactors = FALSE)
  row_nums <- seq_len(nrow(records))
  if ("deleted_by_user" %in% names(records)) {
    row_nums <- row_nums[is.na(records$deleted_by_user)]
  }
  live <- records[row_nums, , drop = FALSE]

  keys <- c(intersect("document_id", names(live)), by)
  key_strings <- if (nrow(live) > 0) {
    do.call(paste, c(lapply(live[keys], function(x) {
      ifelse(is.na(x), "\u0001NA", as.character(x))
    }), sep = "\u0002"))
  } else character(0)
  groups <- unname(split(seq_len(nrow(live)), factor(key_strings, levels = unique(key_strings))))
  first_rows <- vapply(groups, `[`, integer(1), 1L)

  in_cols <- setdiff(names(live), c("id", "deleted_by_user"))
  out <- live[first_rows, keys, drop = FALSE]
  for (col in setdiff(in_cols, keys)) {
    x <- live[[col]]
    if (col %in% sum) {
      out[[col]] <- vapply(groups, function(g) {
        if (all(is.na(x[g]))) NA_real_ else base::sum(x[g], na.rm = TRUE)
      }, numeric(1))
      out[[paste0(col, "_values")]] <- vapply(groups, function(g) {
        as.character(jsonlite::toJSON(x[g], na = "null", digits = NA))
      }, character(1))
    } else {
      out[[col]] <- vapply(groups, function(g) .rollup_values(x[g]), character(1))
    }
  }
  out$n_records <- lengths(groups)
  out <- out[, c(unlist(lapply(in_cols, function(col) {
    if (col %in% sum) c(col, paste0(col, "_values")) else col
  })), "n_records"), drop = FALSE]

  rownames(out) <- NULL
  attr(out, "member_rows") <- lapply(groups, function(g) row_nums[g])
  out
}

# Members' distinct non-missing values as a JSON array, NA when there are
# none. Character values holding a JSON array of scalars are merged element
# by element.
.rollup_values <- function(x) {
  x <- x[!is.na(x)]
  if (is.character(x)) {
    x <- unlist(lapply(x, .rollup_json_elements))
    x <- x[!is.na(x) & nzchar(x)]
  }
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  as.character(jsonlite::toJSON(x, digits = NA))
}

# Elements of a JSON array of scalars, as strings; the value itself otherwise
.rollup_json_elements <- function(raw) {
  if (!startsWith(trimws(raw), "[")) return(raw)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (!is.list(parsed) ||
      !all(vapply(parsed, function(p) is.null(p) || (is.atomic(p) && length(p) == 1L),
                  logical(1)))) {
    return(raw)
  }
  vapply(parsed, function(p) if (is.null(p)) NA_character_ else as.character(p),
         character(1))
}
