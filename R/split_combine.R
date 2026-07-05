# Split and combine SQLite databases for parallel review
#
# Tools for distributing review work across multiple people and
# reassembling the results into a single database.

#' Split a database into roughly equal pieces for parallel review
#'
#' Distributes documents evenly across `n` part databases so that multiple
#' reviewers can work independently. Each part receives the full schema plus
#' the subset of documents (and all associated records and audit trail
#' entries) assigned to that part.
#'
#' After reviewers finish, recombine with [combine_db()].
#'
#' @param db_path Path to the source SQLite database.
#' @param n Integer >= 2. Number of parts to create.
#' @param output_dir Directory for output part files. Defaults to the same
#'   directory as `db_path`.
#' @param prefix Filename prefix for part files. Defaults to the source
#'   filename (without extension). Parts are named
#'   `{prefix}_part_01.db`, `{prefix}_part_02.db`, etc.
#' @param random Logical. If `FALSE` (default), documents are assigned in
#'   ascending `document_id` order so each part gets a consecutive block of
#'   IDs. If `TRUE`, documents are shuffled randomly before assignment.
#' @param seed Optional integer seed for reproducible random assignment when
#'   `random = TRUE`. Ignored when `random = FALSE`.
#' @return Character vector of paths to the created part databases
#'   (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Sequential split (default): part 1 gets lowest IDs, part 2 next block, etc.
#' parts <- split_db("extractions.db", n = 4)
#'
#' # Random split with a reproducible seed
#' parts <- split_db("extractions.db", n = 4, random = TRUE, seed = 42,
#'                   output_dir = "review_parts")
#' }
split_db <- function(db_path, n, output_dir = NULL,
                     prefix = NULL, random = FALSE, seed = NULL) {

  db_path <- normalizePath(db_path, mustWork = TRUE)

  if (!is.numeric(n) || length(n) != 1 || n < 2 || n != round(n)) {
    stop("`n` must be a single integer >= 2")
  }
  n <- as.integer(n)

  if (is.null(output_dir)) {
    output_dir <- dirname(db_path)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  if (is.null(prefix)) {
    prefix <- tools::file_path_sans_ext(basename(db_path))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # --- Introspect source schema ---
  # Filter out SQLite internal tables (sqlite_sequence, sqlite_stat*, etc.)
  # which appear in dbListTables but cannot be created manually.
  tables <- DBI::dbListTables(con)
  tables <- tables[!grepl("^sqlite_", tables, ignore.case = TRUE)]
  if (length(tables) == 0) stop("Database has no tables")

  col_info <- lapply(tables, function(tbl) {
    DBI::dbGetQuery(con, sprintf("PRAGMA table_info(\"%s\")", tbl))
  })
  names(col_info) <- tables

  create_stmts <- lapply(tables, function(tbl) {
    DBI::dbGetQuery(
      con,
      "SELECT sql FROM sqlite_master WHERE type='table' AND name=?",
      params = list(tbl)
    )$sql[[1]]
  })
  names(create_stmts) <- tables

  # --- Validate documents table ---
  if (!"documents" %in% tables) {
    stop("No 'documents' table found in database")
  }

  doc_ids <- DBI::dbGetQuery(
    con, "SELECT document_id FROM documents ORDER BY document_id"
  )$document_id
  n_docs <- length(doc_ids)

  if (n_docs < n) {
    stop(sprintf(
      paste0("Cannot split %d document(s) into %d parts",
             " (fewer documents than parts)."),
      n_docs, n
    ))
  }

  # --- Assign documents to parts ---
  if (isTRUE(random)) {
    if (!is.null(seed)) set.seed(seed)
    doc_ids <- sample(doc_ids)
  }

  # cut() produces balanced groups; label with integer part index
  part_idx <- as.integer(
    cut(seq_along(doc_ids), breaks = n, labels = FALSE)
  )
  part_doc_lists <- split(doc_ids, part_idx)

  # --- Classify tables by their key columns ---
  has_doc_id <- vapply(
    col_info, function(ci) "document_id" %in% ci$name, logical(1)
  )
  has_interaction_id <- vapply(
    col_info, function(ci) "interaction_id" %in% ci$name, logical(1)
  )

  # --- Create parts ---
  n_digits     <- nchar(as.character(n))
  fmt          <- paste0("%s_part_%0", n_digits, "d.db")
  output_paths <- character(n)

  for (i in seq_len(n)) {
    part_path        <- file.path(output_dir, sprintf(fmt, prefix, i))
    output_paths[i]  <- part_path

    if (file.exists(part_path)) file.remove(part_path)

    part_con <- DBI::dbConnect(RSQLite::SQLite(), part_path)

    tryCatch({
      # Recreate schema
      for (tbl in tables) {
        stmt <- create_stmts[[tbl]]
        if (!is.na(stmt) && nzchar(stmt)) {
          DBI::dbExecute(part_con, stmt)
        }
      }

      group_doc_ids <- part_doc_lists[[i]]

      # Collect interaction ids for this part (for interaction_id-keyed tables)
      part_interaction_ids <- character(0)
      int_tables <- tables[
        has_doc_id & !has_interaction_id & tables != "documents"
      ]
      for (int_tbl in int_tables) {
        ci     <- col_info[[int_tbl]]
        pk_col <- ci$name[ci$pk > 0]
        # Only harvest PKs called 'id' (i.e. the interactions/records table)
        if (length(pk_col) == 1 && pk_col == "id") {
          rows <- .fetch_by_doc_id(con, int_tbl, group_doc_ids)
          if (nrow(rows) > 0) {
            part_interaction_ids <- c(part_interaction_ids, rows$id)
          }
        }
      }

      # Copy rows for each table
      for (tbl in tables) {
        if (has_doc_id[tbl]) {
          rows <- .fetch_by_doc_id(con, tbl, group_doc_ids)
        } else if (
          has_interaction_id[tbl] && length(part_interaction_ids) > 0
        ) {
          rows <- .fetch_by_interaction_id(
            con, tbl, part_interaction_ids
          )
        } else {
          # Lookup / config tables: copy in full
          rows <- DBI::dbGetQuery(
            con, sprintf("SELECT * FROM \"%s\"", tbl)
          )
        }

        if (nrow(rows) > 0) {
          DBI::dbWriteTable(part_con, tbl, rows, append = TRUE)
        }
      }

      cat(sprintf(
        "Part %d/%d: %d documents -> %s\n",
        i, n, length(group_doc_ids), basename(part_path)
      ))

    }, error = function(e) {
      DBI::dbDisconnect(part_con)
      if (file.exists(part_path)) file.remove(part_path)
      stop(sprintf("Error creating part %d: %s", i, e$message))
    })

    DBI::dbDisconnect(part_con)
  }

  cat(sprintf(
    "\nSplit complete: %d documents across %d parts in '%s'\n",
    n_docs, n, output_dir
  ))

  invisible(output_paths)
}


#' Combine part databases back into a single database
#'
#' Merges multiple part databases (created by [split_db()]) into one,
#' verifying that all parts share an identical schema before proceeding.
#'
#' `db_paths` can be supplied in two ways:
#' * A character **vector** of file paths (existing behaviour).
#' * A **single directory path** — all files in that directory whose names
#'   match `pattern` are collected automatically and sorted before merging.
#'
#' **Schema check**: table names, column names, and column types must match
#' across all parts. Mismatched tables or columns raise an error;
#' mismatched column types raise a warning (SQLite's flexible typing often
#' makes this harmless, but you should investigate).
#'
#' **Overlap check**: document IDs are expected to be disjoint across parts.
#' Any overlap is reported as a warning; overlapping rows are still inserted
#' once (the duplicate is skipped with `INSERT OR IGNORE`).
#'
#' **Primary-key remapping**: tables that carry their own auto-increment
#' integer primary key *and* reference `interaction_id`
#' (e.g. `human_edits`) are inserted without their original PK so SQLite
#' assigns new non-conflicting values. All foreign-key relationships are
#' preserved because interaction IDs themselves are unique across parts.
#'
#' @param db_paths A character vector of >= 2 part database file paths,
#'   **or** a single directory path. When a directory is supplied,
#'   `pattern` is used to discover the part files within it.
#' @param output_path Path for the combined output database. Must not
#'   already exist unless `overwrite = TRUE`.
#' @param overwrite If `TRUE`, delete `output_path` before combining.
#'   Default `FALSE`.
#' @param pattern Regular expression used to filter files when `db_paths`
#'   is a directory. Defaults to `"_part_\\d+\\.db$"`, which matches the
#'   naming convention produced by [split_db()].
#' @return Path to the combined database (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Supply a directory — part files discovered automatically
#' combine_db("review_parts/", output_path = "extractions_reviewed.db")
#'
#' # Supply paths explicitly
#' parts <- list.files("review_parts", pattern = "_part_\\d+\\.db$",
#'                     full.names = TRUE)
#' combine_db(parts, output_path = "extractions_reviewed.db")
#' }
combine_db <- function(db_paths, output_path, overwrite = FALSE,
                       pattern = "_part_\\d+\\.db$") {

  # --- Accept a single directory path ---
  if (length(db_paths) == 1 && dir.exists(db_paths)) {
    dir_path <- db_paths
    db_paths <- sort(
      list.files(dir_path, pattern = pattern, full.names = TRUE)
    )
    if (length(db_paths) == 0) {
      stop(sprintf(
        "No files matching pattern '%s' found in directory: %s",
        pattern, dir_path
      ))
    }
    cat(sprintf(
      "Found %d part file(s) matching '%s' in '%s'\n",
      length(db_paths), pattern, dir_path
    ))
  }

  if (length(db_paths) < 2) {
    stop("`db_paths` must contain at least 2 database paths")
  }

  db_paths    <- vapply(
    db_paths, normalizePath, character(1), mustWork = TRUE
  )
  output_path <- normalizePath(output_path, mustWork = FALSE)

  if (output_path %in% db_paths) {
    stop("`output_path` must not be the same as one of the part paths")
  }

  if (file.exists(output_path)) {
    if (!overwrite) {
      stop(sprintf(
        "Output already exists: %s\nUse overwrite = TRUE to replace it.",
        output_path
      ))
    }
    file.remove(output_path)
  }

  # ---- Schema check ----
  cat(sprintf("Checking schemas across %d parts...\n", length(db_paths)))

  schemas    <- lapply(db_paths, .read_schema)
  ref_schema <- schemas[[1]]
  ref_tables <- sort(names(ref_schema))

  for (i in seq_along(db_paths)[-1]) {
    this_schema <- schemas[[i]]
    this_tables <- sort(names(this_schema))
    src_label   <- basename(db_paths[i])
    ref_label   <- basename(db_paths[1])

    # Table-level check
    missing_tbls <- setdiff(ref_tables, this_tables)
    extra_tbls   <- setdiff(this_tables, ref_tables)
    if (length(missing_tbls) > 0 || length(extra_tbls) > 0) {
      msg <- sprintf(
        "Table mismatch between '%s' and '%s':", ref_label, src_label
      )
      if (length(missing_tbls) > 0)
        msg <- paste0(msg, sprintf(
          "\n  Missing in %s: %s",
          src_label, paste(missing_tbls, collapse = ", ")
        ))
      if (length(extra_tbls) > 0)
        msg <- paste0(msg, sprintf(
          "\n  Extra in %s: %s",
          src_label, paste(extra_tbls, collapse = ", ")
        ))
      stop(msg)
    }

    # Column-level check for each shared table
    shared_tables <- intersect(ref_tables, this_tables)
    for (tbl in shared_tables) {
      ref_ci   <- ref_schema[[tbl]]
      this_ci  <- this_schema[[tbl]]

      ref_cols  <- sort(ref_ci$name)
      this_cols <- sort(this_ci$name)

      if (!identical(ref_cols, this_cols)) {
        missing_cols <- setdiff(ref_cols, this_cols)
        extra_cols   <- setdiff(this_cols, ref_cols)
        msg <- sprintf(
          "Column mismatch in table '%s' ('%s' vs '%s'):",
          tbl, ref_label, src_label
        )
        if (length(missing_cols) > 0)
          msg <- paste0(msg, sprintf(
            "\n  Missing in %s: %s",
            src_label, paste(missing_cols, collapse = ", ")
          ))
        if (length(extra_cols) > 0)
          msg <- paste0(msg, sprintf(
            "\n  Extra in %s: %s",
            src_label, paste(extra_cols, collapse = ", ")
          ))
        stop(msg)
      }

      # Type check (warning only — SQLite typing is flexible)
      ref_types  <- ref_ci$type[order(ref_ci$name)]
      this_types <- this_ci$type[order(this_ci$name)]
      if (!identical(ref_types, this_types)) {
        mismatched <- ref_cols[ref_types != this_types]
        warning(sprintf(
          paste0("Type mismatch in table '%s', column(s): %s",
                 " ('%s' vs '%s'). Proceeding."),
          tbl, paste(mismatched, collapse = ", "),
          ref_label, src_label
        ))
      }
    }
  }

  cat("Schema check passed.\n")

  # ---- Overlap check ----
  all_doc_id_sets <- lapply(db_paths, function(path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(
      con, "SELECT document_id FROM documents"
    )$document_id
  })

  all_doc_ids <- unlist(all_doc_id_sets)
  overlap_ids <- all_doc_ids[duplicated(all_doc_ids)]
  if (length(overlap_ids) > 0) {
    n_overlap   <- length(overlap_ids)
    overlap_ex  <- paste(
      utils::head(unique(overlap_ids), 5), collapse = ", "
    )
    overlap_etc <- if (length(unique(overlap_ids)) > 5) " ..." else ""
    warning(sprintf(
      paste0("%d document_id(s) appear in more than one part;",
             " duplicates will be skipped: %s%s"),
      n_overlap, overlap_ex, overlap_etc
    ))
  }

  # ---- Build output database ----
  ref_con <- DBI::dbConnect(RSQLite::SQLite(), db_paths[1])
  on.exit(DBI::dbDisconnect(ref_con), add = TRUE)

  out_con <- DBI::dbConnect(RSQLite::SQLite(), output_path)
  on.exit(DBI::dbDisconnect(out_con), add = TRUE)

  tables   <- DBI::dbListTables(ref_con)
  tables   <- tables[!grepl("^sqlite_", tables, ignore.case = TRUE)]
  col_info <- lapply(tables, function(tbl) {
    DBI::dbGetQuery(ref_con, sprintf("PRAGMA table_info(\"%s\")", tbl))
  })
  names(col_info) <- tables

  # Recreate schema in output DB
  for (tbl in tables) {
    stmt <- DBI::dbGetQuery(
      ref_con,
      "SELECT sql FROM sqlite_master WHERE type='table' AND name=?",
      params = list(tbl)
    )$sql[[1]]
    if (!is.na(stmt) && nzchar(stmt)) {
      DBI::dbExecute(out_con, stmt)
    }
  }

  # Classify tables
  has_doc_id <- vapply(
    col_info, function(ci) "document_id" %in% ci$name, logical(1)
  )
  has_interaction_id <- vapply(
    col_info, function(ci) "interaction_id" %in% ci$name, logical(1)
  )
  pk_cols        <- lapply(col_info, function(ci) ci$name[ci$pk > 0])
  names(pk_cols) <- tables

  # Tables that need PK remapping: have interaction_id AND an independent
  # integer PK (e.g. human_edits). We strip their PK on insert so SQLite
  # auto-assigns new values, avoiding collisions between parts while
  # preserving FK relationships via interaction_id.
  needs_pk_remap <- vapply(tables, function(tbl) {
    pk <- pk_cols[[tbl]]
    has_interaction_id[tbl] &&
      length(pk) > 0 &&
      !all(pk %in% c("interaction_id", "document_id"))
  }, logical(1))

  # ---- Merge data from each part ----
  for (i in seq_along(db_paths)) {
    src_con <- DBI::dbConnect(RSQLite::SQLite(), db_paths[i])

    tryCatch({
      for (tbl in tables) {
        rows <- DBI::dbGetQuery(
          src_con, sprintf("SELECT * FROM \"%s\"", tbl)
        )
        if (nrow(rows) == 0) next

        if (has_doc_id[tbl]) {
          # Rows are disjoint across parts (by design); INSERT OR IGNORE
          # guards against any unexpected overlap.
          .insert_or_ignore(out_con, tbl, rows)

        } else if (needs_pk_remap[tbl]) {
          # Drop the part-local PK; let SQLite assign a fresh one.
          pk         <- pk_cols[[tbl]]
          rows_no_pk <- rows[, setdiff(names(rows), pk), drop = FALSE]
          DBI::dbWriteTable(out_con, tbl, rows_no_pk, append = TRUE)

        } else {
          # Lookup / config tables: insert from first part only.
          if (i == 1) {
            DBI::dbWriteTable(out_con, tbl, rows, append = TRUE)
          }
        }
      }

      n_docs     <- DBI::dbGetQuery(
        src_con, "SELECT COUNT(*) AS n FROM documents"
      )$n
      n_reviewed <- DBI::dbGetQuery(
        src_con,
        "SELECT COUNT(*) AS n FROM documents WHERE reviewed_at IS NOT NULL"
      )$n

      cat(sprintf(
        "  Part %d/%d (%s): %d documents, %d reviewed\n",
        i, length(db_paths), basename(db_paths[i]), n_docs, n_reviewed
      ))

    }, error = function(e) {
      DBI::dbDisconnect(src_con)
      stop(sprintf(
        "Error merging part %d (%s): %s",
        i, basename(db_paths[i]), e$message
      ))
    })

    DBI::dbDisconnect(src_con)
  }

  # ---- Summary ----
  total_docs <- DBI::dbGetQuery(
    out_con, "SELECT COUNT(*) AS n FROM documents"
  )$n
  total_reviewed <- DBI::dbGetQuery(
    out_con,
    "SELECT COUNT(*) AS n FROM documents WHERE reviewed_at IS NOT NULL"
  )$n

  int_tbl <- intersect(
    c("interactions", "records"), DBI::dbListTables(out_con)
  )
  total_interactions <- if (length(int_tbl) > 0) {
    DBI::dbGetQuery(
      out_con, sprintf("SELECT COUNT(*) AS n FROM \"%s\"", int_tbl[1])
    )$n
  } else {
    NA_integer_
  }

  int_fmt <- if (is.na(total_interactions)) {
    "?"
  } else {
    format(total_interactions, big.mark = ",")
  }
  cat(sprintf(
    "\nCombine complete: %d documents (%d/%d reviewed), %s records -> '%s'\n",
    total_docs, total_reviewed, total_docs,
    int_fmt, basename(output_path)
  ))

  invisible(output_path)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Read table schema from a SQLite database
#' @param db_path Path to SQLite database
#' @return Named list; each element is a data frame with columns
#'   `name`, `type`, `pk`
#' @keywords internal
.read_schema <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  tables  <- DBI::dbListTables(con)
  tables  <- tables[!grepl("^sqlite_", tables, ignore.case = TRUE)]
  ci_list <- lapply(tables, function(tbl) {
    ci <- DBI::dbGetQuery(
      con, sprintf("PRAGMA table_info(\"%s\")", tbl)
    )
    ci[, c("name", "type", "pk")]
  })
  names(ci_list) <- tables
  ci_list
}

#' Fetch rows from a table filtered by a vector of document_ids
#' @keywords internal
.fetch_by_doc_id <- function(con, tbl, doc_ids) {
  if (length(doc_ids) == 0) return(data.frame())
  placeholders <- paste(rep("?", length(doc_ids)), collapse = ", ")
  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM \"%s\" WHERE document_id IN (%s)",
      tbl, placeholders
    ),
    params = as.list(doc_ids)
  )
}

#' Fetch rows from a table filtered by a vector of interaction ids
#' @keywords internal
.fetch_by_interaction_id <- function(con, tbl, interaction_ids) {
  if (length(interaction_ids) == 0) return(data.frame())
  placeholders <- paste(
    rep("?", length(interaction_ids)), collapse = ", "
  )
  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM \"%s\" WHERE interaction_id IN (%s)",
      tbl, placeholders
    ),
    params = as.list(interaction_ids)
  )
}

#' INSERT rows with OR IGNORE to safely skip duplicate PKs
#' @keywords internal
.insert_or_ignore <- function(con, tbl, rows) {
  if (nrow(rows) == 0) return(invisible(NULL))
  cols    <- names(rows)
  col_str <- paste(sprintf("\"%s\"", cols), collapse = ", ")
  ph_str  <- paste(rep("?", length(cols)), collapse = ", ")
  sql     <- sprintf(
    "INSERT OR IGNORE INTO \"%s\" (%s) VALUES (%s)",
    tbl, col_str, ph_str
  )
  for (r in seq_len(nrow(rows))) {
    DBI::dbExecute(
      con, sql, params = unname(as.list(rows[r, , drop = FALSE]))
    )
  }
  invisible(NULL)
}
