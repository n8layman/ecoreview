# test_add_record_no_existing.R
#
# Regression tests for the "Error saving: missing value where TRUE/FALSE needed"
# crash that occurs when a user adds a record to a document that had zero
# LLM-extracted records and then clicks "Verify Records".
#
# Root causes
# -----------
# Bug 1 (ecoreview inst/app/app.R):
#   The record-ID renaming loop runs when publication_year or first_author_lastname
#   changed. For a document with NULL metadata (common when LLM extracted nothing),
#   orig_year is NULL vs new_year is NA_integer_ -> !identical() is TRUE, so the
#   loop runs. Inside, old_id is NA_character_ for user-added rows.
#       if (old_id != new_record_id)   # NA != x -> NA -> if(NA) -> ERROR
#   Fixed by adding !is.na(old_id) guard.
#
# Bug 2 (ecoextract R/getters.R, tracked in ecoextract issue #NNN):
#   save_document() guards the diff/insert block with nrow(original_df) > 0.
#   When original_df is a 0-row tibble (no prior records), the block is skipped
#   entirely and user-added rows (id = NA) are never inserted.
#   Fix: remove the nrow() guard so diff_records() runs even for empty original_df.
#
# Test 1: end-to-end save with 0-row original_df inserts the new row (requires Bug 2 fix)
# Test 2: verify the saved row has the right field values
# Test 3: NA record_id in the renaming loop does not throw (Bug 1 fix, ecoreview only)
#
# Run with: Rscript inst/test_add_record_no_existing.R

devtools::load_all(quiet = TRUE)
library(DBI)
library(RSQLite)

pass_sym <- "✓"
fail_sym <- "✗"

pass_count <- 0L
fail_count <- 0L

# ---- helpers ----------------------------------------------------------------

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

check_no_error <- function(label, expr) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(err)) {
    cat(sprintf("  %s %s\n", pass_sym, label))
    pass_count <<- pass_count + 1L
  } else {
    cat(sprintf("  %s %s\n    error: %s\n", fail_sym, label, err))
    fail_count <<- fail_count + 1L
  }
}

# Create a minimal ecoextract DB with one document and no records.
# Uses init_ecoextract_database() for the real schema, then inserts a document
# row directly via SQL to avoid needing an actual PDF file on disk.
make_empty_doc_db <- function(path) {
  if (file.exists(path)) file.remove(path)
  ecoextract::init_ecoextract_database(path)
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    INSERT INTO documents
      (file_name, file_path, file_hash, file_size, upload_timestamp,
       first_author_lastname, publication_year)
    VALUES
      ('test.pdf', 'test.pdf', 'abc123', 0, datetime('now'),
       NULL, NULL)
  ")
  dbGetQuery(con, "SELECT document_id FROM documents LIMIT 1")$document_id
}

# Simulate the ecoreview addRowBtn handler:
# take a 0-row template tibble and append one new row with all NAs, then set
# the fields the handler populates.
make_user_added_row <- function(template_df, document_id) {
  new_row <- template_df[1, ]
  new_row[1, ] <- NA
  new_row$id          <- NA_integer_
  new_row$document_id <- as.integer(document_id)
  new_row$record_id   <- NA_character_   # assigned by ecoextract on save
  if ("prompt_hash"        %in% names(new_row)) new_row$prompt_hash        <- "manually_added"
  if ("llm_model_version"  %in% names(new_row)) new_row$llm_model_version  <- "manually_added"
  # Populate required NOT NULL fields
  if ("all_supporting_source_sentences" %in% names(new_row))
    new_row$all_supporting_source_sentences <- '["Test sentence from source."]'
  if ("prompt_hash" %in% names(new_row) && is.na(new_row$prompt_hash))
    new_row$prompt_hash <- "manually_added"
  # Fill in one schema field so there is distinguishable content
  if ("bat_species_scientific_name" %in% names(new_row))
    new_row$bat_species_scientific_name <- "Tadarida brasiliensis"
  rbind(template_df, new_row)
}

# ============================================================
# Setup
# ============================================================
tmp      <- tempdir()
db_path  <- file.path(tmp, "test_add_record.db")

cat("\n=== Setup: create DB with one zero-record document ===\n\n")
doc_id <- make_empty_doc_db(db_path)
cat(sprintf("  document_id = %d\n", doc_id))

# Replicate what ecoreview does on document load: get records (0 rows)
original_df  <- ecoextract::get_records(doc_id, db_conn = db_path)
cat(sprintf("  initial record count = %d (should be 0)\n", nrow(original_df)))

# Replicate addRowBtn: append one user-added row
records_df <- make_user_added_row(original_df, doc_id)
cat(sprintf("  records_df after add = %d row(s)\n\n", nrow(records_df)))


# ============================================================
# TEST 1: save_document does not error with 0-row original_df
# ============================================================
cat("=== TEST 1: save_document() does not error (0-row original_df) ===\n\n")

check_no_error(
  "save_document() completes without error",
  ecoextract::save_document(
    document_id = doc_id,
    records_df  = records_df,
    original_df = original_df,
    db_conn     = db_path
  )
)


# ============================================================
# TEST 2: the user-added row is present in the DB after save
# (requires Bug 2 fix in ecoextract)
# ============================================================
cat("\n=== TEST 2: user-added row persists after save ===\n\n")

saved <- ecoextract::get_records(doc_id, db_conn = db_path)

check("DB now contains 1 record",
      nrow(saved) == 1L)

check("saved row has added_by_user = 1",
      isTRUE("added_by_user" %in% names(saved)) && saved$added_by_user[1] == 1L)

check("record_id was auto-generated (non-NA)",
      isTRUE(!is.na(saved$record_id[1]) && nzchar(saved$record_id[1])))

if ("bat_species_scientific_name" %in% names(saved)) {
  check("field value preserved after save",
        isTRUE(saved$bat_species_scientific_name[1] == "Tadarida brasiliensis"))
}


# ============================================================
# TEST 4: second verify does not duplicate the row
# ============================================================
cat("\n=== TEST 4: second verify does not duplicate ===\n\n")

# Simulate what ecoreview does after first verify: reload from DB
reloaded <- ecoextract::get_records(doc_id, db_conn = db_path)

check_no_error(
  "second save_document() (double-click verify) completes without error",
  ecoextract::save_document(
    document_id = doc_id,
    records_df  = reloaded,
    original_df = reloaded,
    db_conn     = db_path
  )
)

after_second_verify <- ecoextract::get_records(doc_id, db_conn = db_path)
check("still only 1 record after second verify (no duplicate)",
      nrow(after_second_verify) == 1L)


# ============================================================
# TEST 5: delete after verify soft-deletes, does not duplicate
# ============================================================
cat("\n=== TEST 5: delete after verify removes row, does not duplicate ===\n\n")

# Simulate ecoreview delete: set deleted_by_user on the in-memory row
deleted_df <- reloaded
deleted_df$deleted_by_user <- as.character(Sys.time())

# records_df passed to save_document excludes soft-deleted rows
records_after_delete <- deleted_df[is.na(deleted_df$deleted_by_user), , drop = FALSE]

check_no_error(
  "save_document() with deleted row completes without error",
  ecoextract::save_document(
    document_id = doc_id,
    records_df  = records_after_delete,
    original_df = reloaded,
    db_conn     = db_path
  )
)

after_delete <- ecoextract::get_records(doc_id, db_conn = db_path)
check("record count still 1 after delete verify (not duplicated)",
      nrow(after_delete) == 1L)
check("row is marked deleted_by_user",
      isTRUE(!is.na(after_delete$deleted_by_user[1])))


# ============================================================
# TEST 3: record-ID renaming loop with NA old_id does not throw
# (ecoreview Bug 1 fix — inline simulation of the app.R loop)
# ============================================================
cat("\n=== TEST 3: NA record_id in renaming loop does not throw ===\n\n")

# Simulate a df that includes one user-added row (record_id = NA)
mock_df <- tibble::tibble(
  record_id = c("Smith_2020_1_r1", NA_character_)
)

new_author <- "Smith"
new_year   <- 2021L

check_no_error(
  "renaming loop with NA record_id does not throw (fixed guard)",
  {
    for (i in seq_len(nrow(mock_df))) {
      old_id <- mock_df$record_id[i]
      parts  <- strsplit(old_id, "_")[[1]]
      if (length(parts) >= 4L) {
        combo_num  <- parts[length(parts) - 1L]
        record_num <- sub("^r", "", parts[length(parts)])
      } else {
        combo_num  <- "1"
        record_num <- as.character(i)
      }
      new_record_id <- paste0(new_author, "_", new_year, "_",
                              combo_num, "_r", record_num)
      # The fix: guard against NA old_id before comparison
      if (!is.na(old_id) && old_id != new_record_id) {
        mock_df$record_id[i] <- new_record_id
      }
    }
  }
)

check("existing record_id was updated",
      isTRUE(mock_df$record_id[1] == "Smith_2021_1_r1"))

check("NA record_id was left untouched",
      isTRUE(is.na(mock_df$record_id[2])))


# ============================================================
# TEST 5b: delete using stale in-memory data (id=NA) after first verify
# Simulates the user deleting immediately before the reload updates the UI.
# The stale records_df has the row with id=NA and deleted_by_user set.
# ============================================================
cat("\n=== TEST 5b: delete with stale id=NA data does not duplicate ===\n\n")

db_path_5b <- file.path(tempdir(), "test_stale_delete.db")
if (file.exists(db_path_5b)) file.remove(db_path_5b)
doc_id_5b   <- make_empty_doc_db(db_path_5b)
orig_5b     <- ecoextract::get_records(doc_id_5b, db_conn = db_path_5b)
recs_5b     <- make_user_added_row(orig_5b, doc_id_5b)

# First verify — row gets inserted
ecoextract::save_document(
  document_id = doc_id_5b,
  records_df  = recs_5b,
  original_df = orig_5b,
  db_conn     = db_path_5b
)

# Simulate stale state: user deletes using the pre-reload in-memory data (id=NA)
stale_df <- recs_5b
stale_df$deleted_by_user <- as.character(Sys.time())
stale_records_after_delete <- stale_df[is.na(stale_df$deleted_by_user), , drop = FALSE]

# The fix: reload original_df fresh from DB before saving (mirrors app.R fix)
fresh_original_5b <- ecoextract::get_records(doc_id_5b, db_conn = db_path_5b)

check_no_error(
  "second verify (delete via stale id=NA data) does not error",
  ecoextract::save_document(
    document_id = doc_id_5b,
    records_df  = stale_records_after_delete,
    original_df = fresh_original_5b,
    db_conn     = db_path_5b
  )
)

after_stale_delete <- ecoextract::get_records(doc_id_5b, db_conn = db_path_5b)
check("no duplicate rows after stale-data delete verify",
      nrow(after_stale_delete) == 1L)
check("row is marked deleted after stale-data delete verify",
      isTRUE(!is.na(after_stale_delete$deleted_by_user[1])))


# ============================================================
# TEST 6: DB in different directory from PDFs (path resolution unaffected)
# ============================================================
cat("\n=== TEST 6: DB in different dir from PDFs does not break add/verify ===\n\n")

db_path2 <- file.path(tempdir(), "subdir_db", "test2.db")
dir.create(dirname(db_path2), showWarnings = FALSE)
doc_id2 <- make_empty_doc_db(db_path2)

orig2    <- ecoextract::get_records(doc_id2, db_conn = db_path2)
recs2    <- make_user_added_row(orig2, doc_id2)

check_no_error(
  "save_document() works when DB is in a subdirectory",
  ecoextract::save_document(
    document_id = doc_id2,
    records_df  = recs2,
    original_df = orig2,
    db_conn     = db_path2
  )
)

saved2 <- ecoextract::get_records(doc_id2, db_conn = db_path2)
check("row saved correctly with DB in separate directory",
      nrow(saved2) == 1L)

reloaded2 <- ecoextract::get_records(doc_id2, db_conn = db_path2)
check_no_error(
  "second verify (double-click) does not error with DB in separate directory",
  ecoextract::save_document(
    document_id = doc_id2,
    records_df  = reloaded2,
    original_df = reloaded2,
    db_conn     = db_path2
  )
)
check("no duplicate with DB in separate directory",
      nrow(ecoextract::get_records(doc_id2, db_conn = db_path2)) == 1L)


# ============================================================
# TEST 7: NULL document_content does not break add/verify round-trip
# ============================================================
cat("\n=== TEST 7: NULL document_content does not break add/verify ===\n\n")

db_path3 <- file.path(tempdir(), "test_null_content.db")
if (file.exists(db_path3)) file.remove(db_path3)
ecoextract::init_ecoextract_database(db_path3)
con3 <- DBI::dbConnect(RSQLite::SQLite(), db_path3)
DBI::dbExecute(con3,
  "INSERT INTO documents
     (file_name, file_path, file_hash, file_size, upload_timestamp,
      first_author_lastname, publication_year, document_content)
   VALUES
     ('null_content.pdf', 'null_content.pdf', 'def456', 0, datetime('now'),
      'Smith', 2020, NULL)"
)
doc_id3 <- DBI::dbGetQuery(con3, "SELECT document_id FROM documents LIMIT 1")$document_id
DBI::dbDisconnect(con3)

orig3 <- ecoextract::get_records(doc_id3, db_conn = db_path3)
recs3 <- make_user_added_row(orig3, doc_id3)

check_no_error(
  "save_document() succeeds when document_content is NULL",
  ecoextract::save_document(
    document_id = doc_id3,
    records_df  = recs3,
    original_df = orig3,
    db_conn     = db_path3
  )
)

saved3    <- ecoextract::get_records(doc_id3, db_conn = db_path3)
reloaded3 <- saved3

check("row saved when document_content is NULL", nrow(saved3) == 1L)

check_no_error(
  "second verify does not error when document_content is NULL",
  ecoextract::save_document(
    document_id = doc_id3,
    records_df  = reloaded3,
    original_df = reloaded3,
    db_conn     = db_path3
  )
)
check("no duplicate when document_content is NULL",
      nrow(ecoextract::get_records(doc_id3, db_conn = db_path3)) == 1L)


# ============================================================
# Summary
# ============================================================
cat(sprintf(
  "\n=== Results: %d passed, %d failed ===\n\n",
  pass_count, fail_count
))

if (fail_count > 0L) quit(status = 1L)
