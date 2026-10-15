records <- data.frame(
  id = c("a", "b", "c", "d", "e"),
  document_id = c(1L, 1L, 1L, 1L, 2L),
  record_id = c("r1", "r2", "r3", "r4", "r5"),
  species = c("Bufo", "Bufo", "Rana", "Bufo", "Bufo"),
  site = c("A", "A", "A", "A", "A"),
  count = c(5, 7, 2, NA, 4),
  method = c("net", "trap", "net", "net", "net"),
  life_stage = c("adult", "adult", "juvenile", "adult", "adult"),
  notes = c("small", NA, "", "large", NA),
  all_supporting_source_sentences = c('["s1","s2"]', '["s2","s3"]', '["s4"]', NA, "plain"),
  human_edited = c(TRUE, FALSE, FALSE, TRUE, FALSE),
  added_by_user = c(NA, 1L, NA, NA, NA),
  deleted_by_user = NA_character_,
  stringsAsFactors = FALSE
)

test_that("one row per document and key combination, sums match members", {
  out <- rollup_records(records, by = c("species", "site"), sum = "count",
                        concat = "notes")
  expect_equal(nrow(out), 3)
  expect_equal(out$document_id, c(1L, 1L, 2L))
  expect_equal(out$species, c("Bufo", "Rana", "Bufo"))
  expect_equal(out$n_records, c(3L, 1L, 1L))
  expect_equal(out$count, c(12, 2, 4))
  expect_equal(attr(out, "member_rows"), list(c(1L, 2L, 4L), 3L, 5L))
  expect_false("id" %in% names(out))
  expect_false("deleted_by_user" %in% names(out))
})

test_that("differing values are flagged, agreeing values kept", {
  out <- rollup_records(records, by = c("species", "site"), sum = "count")
  expect_equal(out$method, c("mixed: net; trap", "net", "net"))
  expect_equal(out$life_stage, c("adult", "juvenile", "adult"))
})

test_that("agreeing values keep the column type", {
  out <- rollup_records(records, by = "species", sum = "count")
  expect_type(out$site, "character")
  expect_equal(out$site, c("A", "A", "A"))
})

test_that("evidence is the union of members' evidence", {
  out <- rollup_records(records, by = c("species", "site"), sum = "count")
  expect_equal(out$all_supporting_source_sentences,
               c('["s1","s2","s3"]', '["s4"]', '["plain"]'))
})

test_that("free text is concatenated, record ids joined, flags counted", {
  out <- rollup_records(records, by = c("species", "site"), sum = "count",
                        concat = "notes")
  expect_equal(out$notes, c("small | large", NA, NA))
  expect_equal(out$record_id, c("r1; r2; r4", "r3", "r5"))
  expect_equal(out$human_edited, c("2 of 3", "0 of 1", "0 of 1"))
  expect_equal(out$added_by_user, c("1 of 3", "0 of 1", "0 of 1"))
})

test_that("soft-deleted records are left out", {
  rec <- records
  rec$deleted_by_user[2] <- "2026-10-15 10:00:00"
  out <- rollup_records(rec, by = c("species", "site"), sum = "count")
  expect_equal(out$count[1], 5)
  expect_equal(out$n_records[1], 2L)
  expect_equal(attr(out, "member_rows")[[1]], c(1L, 4L))
  expect_equal(out$method[1], "net")
})

test_that("missing keys group together and all-NA sums stay NA", {
  rec <- records
  rec$species[c(1, 2)] <- NA
  rec$count[c(1, 2)] <- NA
  out <- rollup_records(rec, by = "species", sum = "count")
  expect_equal(nrow(out), 4)
  expect_true(is.na(out$species[1]))
  expect_true(is.na(out$count[1]))
})

test_that("bad column names and non-numeric sums error", {
  expect_error(rollup_records(records, by = "nope"), "not found")
  expect_error(rollup_records(records, by = "species", sum = "method"), "numeric")
})

test_that("empty records give an empty rollup", {
  out <- rollup_records(records[0, ], by = "species", sum = "count")
  expect_equal(nrow(out), 0)
  expect_equal(attr(out, "member_rows"), list())
})
