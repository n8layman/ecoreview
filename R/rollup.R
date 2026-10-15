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
#'     when every member is \code{NA}).
#'   \item \code{evidence_col}: the union of every member's evidence, as a
#'     JSON array, so evidence highlighting still finds every source line.
#'   \item Every other column: the shared value when members agree, otherwise
#'     the distinct values joined with \code{" | "}.
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
#' @param evidence_col Name of the evidence column (a JSON array of strings, or
#'   a single string) whose values are combined across members.
#' @return A data frame with one row per group, in order of each group's first
#'   record. Attribute \code{"member_rows"} is a list giving, for each group, the
#'   row numbers of its members in \code{records}.
#' @export
rollup_records <- function(records, by, sum = character(0),
                           evidence_col = "all_supporting_source_sentences") {
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

  out_cols <- setdiff(names(live), c("id", "deleted_by_user"))
  out <- live[first_rows, out_cols, drop = FALSE]
  for (col in setdiff(out_cols, keys)) {
    x <- live[[col]]
    out[[col]] <- if (col %in% sum) {
      vapply(groups, function(g) {
        if (all(is.na(x[g]))) NA_real_ else base::sum(x[g], na.rm = TRUE)
      }, numeric(1))
    } else if (identical(col, evidence_col)) {
      vapply(groups, function(g) .rollup_evidence(x[g]), character(1))
    } else {
      .rollup_shared(x, groups)
    }
  }
  out$n_records <- lengths(groups)

  rownames(out) <- NULL
  attr(out, "member_rows") <- lapply(groups, function(g) row_nums[g])
  out
}

# Shared value when members agree (keeping the column's type), otherwise
# the distinct values joined with " | ".
.rollup_shared <- function(x, groups) {
  vals <- lapply(groups, function(g) unique(x[g]))
  if (all(lengths(vals) == 1L)) {
    return(x[vapply(groups, `[`, integer(1), 1L)])
  }
  vapply(vals, function(v) {
    if (length(v) == 1L) return(as.character(v))
    paste(ifelse(is.na(v), "NA", as.character(v)), collapse = " | ")
  }, character(1))
}

# Union of members' evidence as a JSON array, NA when there is none.
.rollup_evidence <- function(x) {
  sentences <- unlist(lapply(x[!is.na(x)], function(raw) {
    parsed <- tryCatch(jsonlite::fromJSON(raw), error = function(e) raw)
    as.character(parsed)
  }))
  sentences <- unique(sentences[!is.na(sentences) & nzchar(sentences)])
  if (length(sentences) == 0) return(NA_character_)
  as.character(jsonlite::toJSON(sentences))
}
