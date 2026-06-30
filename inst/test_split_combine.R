# test_split_combine.R
#
# Round-trip and error-detection tests for split_db() / combine_db().
#
# Happy paths:
#   1. split_db() produces correct part files
#   2. combine_db() via directory scan reconstructs exact source data
#   3. combine_db() via explicit path vector gives same result
#
# Sad paths:
#   4. combine_db() with only one path -> error
#   5. Output already exists without overwrite -> error
#   6. Part has extra column -> schema error, no output created
#   7. Part has extra table  -> schema error, no output created
#
# Run with: Rscript inst/test_split_combine.R

devtools::load_all(quiet = TRUE)
library(DBI)
library(RSQLite)

pass_sym <- "\u2713"
fail_sym <- "\u2717"

pass_count <- 0L
fail_count <- 0L

# ---- Test helpers -----------------------------------------------------------

check <- function(label, expr) {
  result <- tryCatch(isTRUE(expr), error = function(e) {
    cat(sprintf(
      "  %s %s\n    unexpected error: %s\n",
      fail_sym, label, conditionMessage(e)
    ))
    fail_count <<- fail_count + 1L
    invisible(FALSE)
  })
  if (isTRUE(result)) {
    cat(sprintf("  %s %s\n", pass_sym, label))
    pass_count <<- pass_count + 1L
  } else {
    cat(sprintf("  %s %s\n", fail_sym, label))
    fail_count <<- fail_count + 1L
  }
  invisible(result)
}

expect_error_matching <- function(label, pattern, expr) {
  caught <- tryCatch(
    { force(expr); NULL },
    error = function(e) conditionMessage(e)
  )
  if (is.null(caught)) {
    cat(sprintf(
      "  %s %s  (expected error, got none)\n", fail_sym, label
    ))
    fail_count <<- fail_count + 1L
  } else if (grepl(pattern, caught, ignore.case = TRUE)) {
    cat(sprintf("  %s %s\n    caught: %s\n", pass_sym, label, caught))
    pass_count <<- pass_count + 1L
  } else {
    cat(sprintf(
      "  %s %s\n    wrong error: %s\n", fail_sym, label, caught
    ))
    fail_count <<- fail_count + 1L
  }
}

# ---- Database helpers -------------------------------------------------------

make_dummy_db <- function(path, n_docs = 12L, interactions_per_doc = 3L) {
  if (file.exists(path)) file.remove(path)
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))

  dbExecute(con, "
    CREATE TABLE documents (
      document_id      TEXT    PRIMARY KEY,
      file_name        TEXT    NOT NULL,
      title            TEXT,
      publication_year INTEGER,
      reviewed_at      TEXT
    )")

  dbExecute(con, "
    CREATE TABLE interactions (
      id               INTEGER PRIMARY KEY AUTOINCREMENT,
      document_id      TEXT    NOT NULL
                       REFERENCES documents(document_id),
      record_id        TEXT,
      host_species     TEXT,
      pathogen_species TEXT,
      deleted_by_human INTEGER DEFAULT 0,
      deleted_at       TEXT
    )")

  dbExecute(con, "
    CREATE TABLE human_edits (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      interaction_id INTEGER NOT NULL REFERENCES interactions(id),
      edit_timestamp TEXT    NOT NULL,
      column_name    TEXT    NOT NULL,
      old_value      TEXT,
      new_value      TEXT
    )")

  hosts     <- c("Gallus gallus", "Mus musculus", "Bos taurus", "Sus scrofa")
  pathogens <- c(
    "Influenza A", "Salmonella enterica", "SARS-CoV-2", "E. coli"
  )

  for (d in seq_len(n_docs)) {
    doc_id <- sprintf("doc_%03d", d)
    dbExecute(con, "INSERT INTO documents VALUES (?, ?, ?, ?, ?)",
      params = list(
        doc_id,
        sprintf("paper_%03d.pdf", d),
        sprintf("Title of Paper %d", d),
        2000L + d,
        if (d <= 3L) format(Sys.time()) else NA
      ))

    for (r in seq_len(interactions_per_doc)) {
      dbExecute(con,
        "INSERT INTO interactions
           (document_id, record_id, host_species, pathogen_species)
         VALUES (?, ?, ?, ?)",
        params = list(
          doc_id,
          sprintf("%s_r%d", doc_id, r),
          hosts[((d + r) %% length(hosts)) + 1L],
          pathogens[((d * r) %% length(pathogens)) + 1L]
        ))
      int_id <- dbGetQuery(
        con, "SELECT last_insert_rowid() AS id"
      )$id

      # Add a human edit for every other interaction
      if ((d + r) %% 2L == 0L) {
        dbExecute(con,
          "INSERT INTO human_edits
             (interaction_id, edit_timestamp,
              column_name, old_value, new_value)
           VALUES (?, ?, ?, ?, ?)",
          params = list(
            int_id,
            format(Sys.time()),
            "host_species",
            hosts[((d + r) %% length(hosts)) + 1L],
            hosts[((d + r + 1L) %% length(hosts)) + 1L]
          ))
      }
    }
  }
  invisible(path)
}

read_sorted <- function(db_path, tbl, sort_cols) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  df  <- dbGetQuery(con, sprintf('SELECT * FROM "%s"', tbl))
  df[do.call(order, lapply(sort_cols, function(col) df[[col]])), ]
}

count_rows <- function(db_path, tbl) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', tbl))$n
}

# ============================================================
# Setup
# ============================================================
tmp      <- tempdir()
src_path <- file.path(tmp, "source.db")
n_docs   <- 12L
n_parts  <- 4L
ints_per <- 3L

make_dummy_db(src_path, n_docs = n_docs, interactions_per_doc = ints_per)
n_src_edits <- count_rows(src_path, "human_edits")


# ============================================================
# TEST 1 — Happy path: split_db() produces correct parts
# ============================================================
cat("\n=== TEST 1: split_db() — happy path ===\n\n")

parts <- split_db(src_path, n = n_parts, output_dir = tmp)
cat("\n")

check("Returns correct number of paths",
      length(parts) == n_parts)
check("All part files exist on disk",
      all(file.exists(parts)))
check("Part files follow _part_N naming",
      all(grepl("_part_\\d+\\.db$", basename(parts))))

part_doc_counts <- vapply(parts, count_rows, integer(1), tbl = "documents")
check("Documents distributed evenly (3 per part)",
      all(part_doc_counts == n_docs / n_parts))

all_ids <- unlist(lapply(parts, function(p) {
  con <- dbConnect(SQLite(), p)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT document_id FROM documents")$document_id
}))
check("No document_id appears in more than one part",
      !anyDuplicated(all_ids))
check("All source document_ids accounted for",
      setequal(
        all_ids,
        read_sorted(src_path, "documents", "document_id")$document_id
      ))

part_int_counts <- vapply(
  parts, count_rows, integer(1), tbl = "interactions"
)
check("Interaction counts correct per part (3 docs x 3 interactions)",
      all(part_int_counts == (n_docs / n_parts) * ints_per))

part_edit_counts <- vapply(
  parts, count_rows, integer(1), tbl = "human_edits"
)
check("human_edits distributed (sum equals source count)",
      sum(part_edit_counts) == n_src_edits)

fk_ok <- vapply(parts, function(p) {
  con <- dbConnect(SQLite(), p)
  on.exit(dbDisconnect(con))
  edits <- dbGetQuery(
    con, "SELECT interaction_id FROM human_edits"
  )$interaction_id
  ints  <- dbGetQuery(con, "SELECT id FROM interactions")$id
  all(edits %in% ints)
}, logical(1))
check("human_edits FK intact in every part", all(fk_ok))


# ============================================================
# TEST 2 — Happy path: combine via directory scan
# ============================================================
cat("\n=== TEST 2: combine_db() via directory — happy path ===\n\n")

out_path <- file.path(tmp, "combined_dir.db")

# source.db is in tmp too but does not match _part_\d+\.db$ so it is skipped
combine_db(tmp, output_path = out_path, overwrite = TRUE)
cat("\n")

src_docs <- read_sorted(src_path, "documents",    "document_id")
out_docs <- read_sorted(out_path, "documents",    "document_id")
check("documents: same row count",    nrow(out_docs) == nrow(src_docs))
check("documents: identical content", identical(out_docs, src_docs))

src_ints <- read_sorted(src_path, "interactions", "id")
out_ints <- read_sorted(out_path, "interactions", "id")
check("interactions: same row count",    nrow(out_ints) == nrow(src_ints))
check("interactions: identical content", identical(out_ints, src_ints))

# human_edits: IDs are remapped, compare everything else
edits_cols <- setdiff(
  names(read_sorted(src_path, "human_edits", "id")), "id"
)
src_edits <- read_sorted(
  src_path, "human_edits", c("interaction_id", "column_name", "old_value")
)
out_edits <- read_sorted(
  out_path, "human_edits", c("interaction_id", "column_name", "old_value")
)
check("human_edits: same row count",
      nrow(out_edits) == nrow(src_edits))
check("human_edits: content matches (excl. remapped id)",
      identical(out_edits[, edits_cols], src_edits[, edits_cols]))
check("human_edits FK integrity in combined DB",
      all(out_edits$interaction_id %in% out_ints$id))


# ============================================================
# TEST 3 — Happy path: combine via explicit path vector
# ============================================================
cat("\n=== TEST 3: combine_db() via explicit paths — happy path ===\n\n")

out_path2 <- file.path(tmp, "combined_explicit.db")
combine_db(parts, output_path = out_path2)
cat("\n")

out_docs2 <- read_sorted(out_path2, "documents",    "document_id")
out_ints2 <- read_sorted(out_path2, "interactions", "id")
check("documents identical via explicit paths",
      identical(out_docs2, src_docs))
check("interactions identical via explicit paths",
      identical(out_ints2, src_ints))


# ============================================================
# TEST 4 — Sad path: too few paths supplied
# ============================================================
cat("\n=== TEST 4: Too few paths — sad path ===\n\n")

no_out4 <- file.path(tmp, "nope4.db")
expect_error_matching(
  "combine_db() with 1 file path errors",
  pattern = "at least 2",
  expr    = combine_db(parts[1], output_path = no_out4)
)
check("No output created (1-path error)", !file.exists(no_out4))


# ============================================================
# TEST 5 — Sad path: output already exists, overwrite = FALSE
# ============================================================
cat("\n=== TEST 5: Output exists, no overwrite flag — sad path ===\n\n")

expect_error_matching(
  "combine_db() refuses to overwrite without flag",
  pattern = "already exists|overwrite",
  expr    = combine_db(parts, output_path = out_path2)
)
check("Existing output file not clobbered",
      file.exists(out_path2) &&
        nrow(read_sorted(out_path2, "documents", "document_id")) == n_docs)


# ============================================================
# TEST 6 — Sad path: extra column in one part
# ============================================================
cat("\n=== TEST 6: Extra column in one part — sad path ===\n\n")

parts_copy <- file.path(tmp, paste0("copy_", basename(parts)))
invisible(file.copy(parts, parts_copy))

con_bad <- dbConnect(SQLite(), parts_copy[2])
dbExecute(con_bad, "ALTER TABLE documents ADD COLUMN reviewer_notes TEXT")
dbDisconnect(con_bad)
cat(sprintf("Added 'reviewer_notes' to %s\n\n", basename(parts_copy[2])))

no_out6 <- file.path(tmp, "nope6.db")
expect_error_matching(
  "combine_db() errors on extra column",
  pattern = "Column mismatch|reviewer_notes",
  expr    = combine_db(parts_copy, output_path = no_out6, overwrite = TRUE)
)
check("No output file created after column mismatch", !file.exists(no_out6))


# ============================================================
# TEST 7 — Sad path: extra table in one part
# ============================================================
cat("\n=== TEST 7: Extra table in one part — sad path ===\n\n")

parts_copy2 <- file.path(tmp, paste0("copy2_", basename(parts)))
invisible(file.copy(parts, parts_copy2))

con_bad2 <- dbConnect(SQLite(), parts_copy2[3])
dbExecute(
  con_bad2,
  "CREATE TABLE review_notes (id INTEGER PRIMARY KEY, note TEXT)"
)
dbDisconnect(con_bad2)
cat(sprintf("Added 'review_notes' table to %s\n\n", basename(parts_copy2[3])))

no_out7 <- file.path(tmp, "nope7.db")
expect_error_matching(
  "combine_db() errors on extra table",
  pattern = "Table mismatch|review_notes",
  expr    = combine_db(parts_copy2, output_path = no_out7, overwrite = TRUE)
)
check("No output file created after table mismatch", !file.exists(no_out7))


# ============================================================
# TEST 8 — Happy path: uneven split (10 docs into 3 parts)
# ============================================================
cat("\n=== TEST 8: Uneven split — 10 docs into 3 parts ===\n\n")

uneven_src  <- file.path(tmp, "uneven_source.db")
uneven_out  <- file.path(tmp, "uneven_combined.db")
n_uneven    <- 10L
n_uneven_p  <- 3L

make_dummy_db(uneven_src, n_docs = n_uneven, interactions_per_doc = ints_per)

parts_u <- split_db(uneven_src, n = n_uneven_p, output_dir = tmp,
                    prefix = "uneven")
cat("\n")

check("Returns 3 part files", length(parts_u) == n_uneven_p)
check("All part files exist", all(file.exists(parts_u)))

uneven_counts <- vapply(parts_u, count_rows, integer(1), tbl = "documents")
cat(sprintf(
  "  Part sizes: %s (total %d)\n",
  paste(uneven_counts, collapse = ", "), sum(uneven_counts)
))

check("All 10 documents accounted for (no losses or duplicates)",
      sum(uneven_counts) == n_uneven)

check("Distribution is maximally balanced (max - min <= 1)",
      max(uneven_counts) - min(uneven_counts) <= 1L)

check("Expected split shape: one part of 4, two parts of 3",
      all(sort(uneven_counts) == c(3L, 3L, 4L)))

all_uids <- unlist(lapply(parts_u, function(p) {
  con <- dbConnect(SQLite(), p)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT document_id FROM documents")$document_id
}))
check("No document_id overlap across parts", !anyDuplicated(all_uids))

# Round-trip
combine_db(parts_u, output_path = uneven_out)
cat("\n")

u_src_docs <- read_sorted(uneven_src, "documents",    "document_id")
u_out_docs <- read_sorted(uneven_out, "documents",    "document_id")
u_src_ints <- read_sorted(uneven_src, "interactions", "id")
u_out_ints <- read_sorted(uneven_out, "interactions", "id")

check("Uneven combine: documents identical",    identical(u_out_docs, u_src_docs))
check("Uneven combine: interactions identical", identical(u_out_ints, u_src_ints))

u_edits_cols <- setdiff(
  names(read_sorted(uneven_src, "human_edits", "id")), "id"
)
u_src_edits <- read_sorted(
  uneven_src, "human_edits", c("interaction_id", "column_name", "old_value")
)
u_out_edits <- read_sorted(
  uneven_out, "human_edits", c("interaction_id", "column_name", "old_value")
)
check("Uneven combine: human_edits content matches",
      identical(u_out_edits[, u_edits_cols], u_src_edits[, u_edits_cols]))


# ============================================================
# Summary
# ============================================================
cat(sprintf(
  "\n=== Results: %d passed, %d failed ===\n\n",
  pass_count, fail_count
))

if (fail_count > 0L) quit(status = 1L)
