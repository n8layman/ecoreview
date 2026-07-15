# ecoreview news

## 0.1.41 (2026-07-06)

### Improvements

- **`run_app(db_path = ...)` parameter**: pass a path to a SQLite database and
  the app connects to it automatically on startup, skipping the connect modal.
- **Man pages regenerated**: `split_db.Rd` now documents the `random` and
  updated `seed` parameters. `run_app.Rd` documents `db_path`.

---

## 0.1.40 (2026-07-06)

### Improvements

- **Document count badge uses unique non-NULL document IDs**: total and
  unreviewed counts now count distinct `document_id` values, excluding NULLs.
  Previously `nrow()` was used, so NULL-id rows (from malformed databases)
  inflated both counts.

---

## 0.1.39 (2026-07-06)

### Bug fixes

- **Crash on startup with certain databases**: "missing value where TRUE/FALSE
  needed" in the document-selector observer when `need_force_load` evaluated to
  `NA` (triggered when a document's `document_id` was `NA`, making the `==`
  comparison return `NA`). Guarded with `isTRUE()`.

---

## 0.1.38 (2026-07-05)

### Improvements

- **Curated CSV exports**: records export drops wide/unreadable columns
  (`all_supporting_source_sentences`, `extraction_reasoning`,
  `refinement_reasoning`). Documents export is now a fixed narrow view:
  `document_id`, `file_name`, `first_author_lastname`, `publication_year`,
  `title`, then pipeline statuses (`ocr_status`, `metadata_status`,
  `extraction_status`, `refinement_status`). Missing columns are silently
  skipped via `dplyr::any_of()`.

---

## 0.1.36 (2026-07-05)

### Improvements

- **Separate CSV export buttons**: the single "Export CSV" button is now two
  buttons -- "Export Records CSV" (records joined with document metadata,
  sorted by document_id/record_id) and "Export Documents CSV" (documents
  table only via `ecoextract::get_documents()`).

---

## 0.1.35 (2026-07-05)

### Improvements

- **`split_db` explicit `random` flag**: `split_db()` gains a `random`
  parameter (default `FALSE`). When `FALSE`, documents are split in ascending
  `document_id` order (sequential blocks). When `TRUE`, documents are shuffled
  before splitting; combine with `seed` for reproducibility. Previously the
  mode was inferred from whether `seed` was set.

---

## 0.1.33 (2026-07-05)

### Improvements

- **Document ID mode sorts numerically**: when "Show document IDs" is checked,
  the dropdown sorts by `document_id` (numeric) instead of filename, so IDs
  appear in order.

---

## 0.1.32 (2026-07-05)

### Improvements

- **Toggle document IDs in dropdown**: a "Show document IDs" checkbox below the
  document selector switches the dropdown labels between filenames and raw
  `document_id` integers, making it easy to navigate directly to a known ID.

---

## 0.1.31 (2026-07-02)

### Bug fixes

- **CSV export groups rows by document**: records were exported in insertion
  order, so manually-added rows (inserted after initial extraction) appeared
  at the end of the file rather than alongside other records from the same
  paper. Export now sorts by `document_id` then `record_id`.

---

## 0.1.30 (2026-07-01)

### Improvements

- **Package versions in title bar**: the header now shows both
  `ecoreview vX.Y.Z | ecoextract vX.Y.Z` so users can report exact
  versions without digging through R session info.

---

## 0.1.29 (2026-06-30)

### Improvements

- **Schema migration warning on connect**: when opening a database whose
  `records.id` column is `INTEGER` (pre-UUID schema), a modal appears on
  connect. Two severity levels:
  - `pk = 0` (plain integer, no auto-assign): blocking modal — saves will
    silently duplicate records on every verify. User must run
    `ecoextract::migrate_ecoextract_database(path)` before continuing.
  - `pk = 1` (auto-assign works, but not UUID): dismissible advisory modal
    — saving is safe but databases cannot be merged until migrated.

---

## 0.1.27 (2026-06-30)

### Improvements

- **UUID-ready id handling**: `get_all_edited_cells()` and
  `get_restored_interaction_ids()` now initialise with `character(0)` instead
  of `integer(0)`, and `split_combine.R` accumulates interaction ids into a
  character vector. Prepares ecoreview for ecoextract's UUID v4 record ids
  without type-coercion issues.

---

## 0.1.26 (2026-06-30)

### Improvements

- **Narrow `id` column in DataTable**: the `id` column is capped at 60 px with
  text-overflow ellipsis so it does not dominate the view. All other columns
  keep the existing 200 px cap. Hover tooltip still shows the full value.

---

## 0.1.25 (2026-06-30)

### Bug fixes

- **Row duplication when double-clicking Verify** (Smithsonian/OHP issue #4):
  after the first verify, `values$original_df` was never updated from its
  initial 0-row state. Each subsequent verify call saw the user-added row
  (id = NA) as "added" again and re-inserted it, creating duplicate records.
  Fixed by reloading `original_df` fresh from the database immediately before
  each `save_document()` call, so the diff always compares against the true
  current DB state. Regression test added at
  `inst/test_add_record_no_existing.R` (test 5b).

---

## 0.1.24 (2026-06-30)

### Bug fixes

- **`resolve_pdf_path` not exported**: the function was marked `@keywords internal`
  but called via `ecoreview::` in the app, causing an error on installed builds.
  Exported properly; `find_project_root` exported as well for consistency.

---

## 0.1.23 (2026-06-30)

### Bug fixes

- **"missing value where TRUE/FALSE needed" when adding records to zero-record documents**
  (Smithsonian/OHP issue #4): the record-ID renaming loop (triggered when
  publication year or first author changed) compared `old_id != new_record_id`
  without guarding against `NA`. User-added rows have `record_id = NA` before
  their first save; `NA != x` returns `NA`, and `if(NA)` throws. Fixed with
  `!is.na(old_id) &&` guard. Regression test added at
  `inst/test_add_record_no_existing.R`.

---

## 0.1.22 (2026-06-09)

### Improvements

- **Robust PDF path resolution** (issue #26): the PDF viewer now tries five
  fallback strategies in order before showing "PDF Not Available":
  1. Explicit `pdf_dir` override (highest priority — set in the connect modal
     or via `run_app(pdf_dir = ...)`)
  2. Stored path as-is
  3. Stored path relative to the `.db` file's directory
  4. Stored path relative to the **project root** (found by walking up from
     the db directory until a `.git`, `.Rproj`, `DESCRIPTION`, or `.here`
     marker is found)
  5. Stored path relative to the working directory when `run_app()` was called
- **PDF Folder field in connect modal**: a Browse button lets users set the
  PDF directory interactively without restarting the app.

---

## 0.1.21 (2026-06-09)

### Improvements

- **`pdf_dir` parameter for `run_app()`** (issue #26): initial implementation
  — basename fallback only.

---

## 0.1.20 (2026-06-09)

### Improvements

- **Per-column row filtering** (issue #15): enabled DataTables `filter = 'top'`
  so each column header gets a search input (text box for free-text columns,
  select dropdown for low-cardinality ones). Filtering is entirely client-side
  (`server = FALSE`) and does not affect data indices, so row selection, OCR
  highlighting, and sentence match lookups all remain correct.

---

## 0.1.19 (2026-06-09)

### Improvements

- **"Show All" button in columns modal** (issue #18): added a "Show All"
  button alongside the existing "Hide All" button. Clicking it moves every
  column from the Hidden bucket back to Visible in one step.

---

## 0.1.18 (2026-06-09)

### Improvements

- **Table row sentence tooltip shows match status** (issue #23): hovering over
  a cell in the `all_supporting_source_sentences` column now renders an HTML
  tooltip identical to the OCR pane tooltip — matched sentences are listed
  normally and unmatched sentences are marked with ⚠️ in italic/muted style.
  Uses the `ecrUnmatchedByRow` JS global (already set by `setEvidenceIndex`)
  keyed by the DataTables data-row index. Other column tooltips are unchanged.

### Bug fixes

- **`dtUpdateCell` "Non-table node initialisation" warning**: the JS handler
  was calling `$('#interactiveTable').DataTable()` on the outer Shiny output
  `<div>`, not the inner `<table>`. Changed to
  `$('#interactiveTable table').DataTable()` so the existing API instance is
  returned rather than DataTables attempting re-initialisation on a div.

---

## 0.1.17 (2026-06-09)

### New features

- **Infinite scroll** (issue #21): replaced DataTables pagination with a single
  scrollable container. All rows for the current document are visible without
  page navigation (`paging = FALSE`, pagination controls removed from `dom`).
  The existing `scrollY` container provides the bounded scroll area.

---

## 0.1.16 (2026-06-09)

### New features and bug fixes

- **OS-style multi-row selection** (issue #19): switched to the DataTables Select
  extension with `style = 'os'`. Plain click selects a single row; Ctrl/Cmd+click
  adds or removes rows from the selection; Shift+click selects a range. Multi-row
  delete shows a confirmation modal before soft-deleting all selected rows.
- **Cell edits preserve scroll & sort** (issue #20, #22): cell edits now push a
  targeted `dtUpdateCell` JS message (`api.cell().data().draw(false)`) instead of
  triggering a full table re-render. Scroll position, sort order, and page are
  preserved after every edit.
- **OCR pane no longer scrolls on double-click-to-edit**: a `last_scrolled_row`
  variable tracks which row was last scrolled to. Re-clicking or double-clicking
  the same row does not trigger another `scrollIntoView`.

### Bug fixes

- **`replaceData` replaced by client-side updates**: the table now renders with
  `server = FALSE` (required by the Select extension). `DT::replaceData` calls
  have been replaced with `table_trigger()` for row-count changes (delete/add)
  and `dtUpdateCell` for in-place cell edits.

---

## 0.1.15 (2026-06-09)

### Improvements

- **Verified document indicator**: document names in the dropdown are prefixed
  with ✓ when `reviewed_at` is set, making it easy to spot already-verified
  files when the filter is off.
- **Copyable filename**: a small selectable text line below the dropdown always
  shows the current document's filename so it can be triple-clicked and copied
  without opening the dropdown.

### Bug fixes

- **Uncheck "Show only unreviewed" now shows all docs**: the dropdown observer
  was an `observe()` whose reactive dependency on `input$show_unreviewed_only`
  was indirect (via `filtered_documents()`). In certain execution paths the
  transitive link was not established, so unchecking had no effect. Replaced
  with `observeEvent(list(all_documents(), input$show_unreviewed_only), ...)`
  so the checkbox is an explicit trigger. The handler is auto-isolated, which
  also eliminates the `values$document_id` feedback loop from document
  navigation.

---

## 0.1.14 (2026-06-09)

### Bug fixes

- **OCR pane scroll preserved on cell edit** (issue #20): the row-selection
  observer was an `observe()` that depended on `values$extracted_df`. Every
  cell edit mutated `values$extracted_df`, re-triggering the observer with the
  same selected row and causing `scrollIntoView` to jump the OCR pane back to
  the first highlighted sentence. Fixed by switching to
  `observeEvent(input$interactiveTable_rows_selected)` and isolating
  `extracted_df` reads so the observer fires only on actual row-selection
  changes. The JS handler also now respects a `scroll: false` flag so the
  same-row re-highlight path (e.g. after a delete) can suppress the scroll.
- **PDF scroll preserved on add/delete/toggle** (issue #20): `output$pdfViewer`
  depended on `all_documents()` which in turn depended on `values$edit_trigger`.
  Incrementing `edit_trigger` on add/delete/show_deleted caused a full iframe
  re-render, resetting the PDF to page 1. Fixed by reading `all_documents()`
  via `shiny::isolate()` inside `pdfViewer` so it only re-renders when the
  selected document changes.
- **Table sort order preserved on add/delete/show_deleted** (issue #22): those
  handlers called `table_trigger()`, destroying and recreating the entire
  DataTable instance (and therefore its sort state). Replaced with a
  `refresh_table_proxy()` helper that calls `DT::replaceData()`, the same
  in-place update already used by cell edits. Sort column, sort direction,
  scroll position, and current page are all preserved.

### Improvements

- **OCR highlighting updates on sentences edit**: editing the
  `all_supporting_source_sentences` cell now rebuilds the evidence span index
  and resends `setEvidenceIndex` to the client. All other column edits leave
  the OCR HTML completely untouched.

---

## 0.1.13 (2026-06-09)

### Bug fixes

- `build_evidence_index`: fixed ev_id corruption where `sentence_to_id` was
  set before the injection-success check. A failed injection rolled back the
  counter but left the mapping pointing at the next successfully-injected
  sentence's span, causing unrelated text to be highlighted for that row.
- `build_evidence_index`: strip `<strong>/<em>/<b>/<i>` from the working HTML
  before matching/injection so that bold OCR headings like
  `**Table 1** Detection...` (rendered as `<strong>Table 1</strong>
  Detection...`) no longer block fixed-string span injection.
- `html_to_plain_text`: replace `<td>/<th>` tags with spaces before stripping
  so adjacent table cells are not concatenated. Previously, evidence sentences
  drawn from table rows (e.g. `Other infection | 20 | 2 0.391 ± 0.32 ...`)
  could never match because `infection</td><td>20` stripped to `infection20`
  with no separator between tokens.
- `find_best_match_in_html`: rewrote token-matching to insert
  `[^A-Za-z0-9]*` between every pair of adjacent characters within a token so
  that OCR line-break hyphens (`amphis-tome`) correctly match the un-hyphenated
  evidence word (`amphistome`).

---

## 0.1.12 (2026-06-09)

### Improvements

- OCR viewer now renders page footers and table footnotes. Content in
  `page$other` (e.g. `page_footer`, abbreviation keys) is rendered below the
  table it annotates. Items of type `title` are skipped to avoid duplication.

---

## 0.1.11 (2026-06-08)

### Bug fixes

- Fixed regression introduced in 0.1.10 where stripping all whitespace from
  HTML before plain-text extraction inadvertently broke the fixed-string span
  injection, causing nothing to be highlighted.

---

## 0.1.10 (2026-06-08)

### Bug fixes

- Evidence sentences split across OCR line breaks now match. The working HTML
  is whitespace-normalised (`\\s+` → single space) before plain-text extraction
  so that a sentence broken across lines in the raw OCR still aligns with the
  flat plain text used for matching.

---

## 0.1.9 (2026-06-07)

### Improvements

- OCR tooltip redesigned: two-column layout when four or more sentences are
  present, vertically centred on the cursor, and a click-to-expand scrollable
  modal showing all supporting sentences in full.

### Bug fixes

- Tooltip and sentence modal were non-functional: the IIFE created the modal
  element via `document.body.appendChild()` before `<body>` existed (script
  runs in `<head>`), aborting the entire IIFE and silently dropping all event
  listeners. Fixed with lazy initialisation (`getSentModal()`) mirroring the
  existing `getTooltip()` pattern.

---

## 0.1.8 (2026-06-02)

### Improvements

- Evidence matching simplified to a single punctuation-normalised, case-insensitive
  regex pass. Replaces the previous three-tier approach (exact → word-regex →
  sliding-window cosine). All non-alphanumeric runs in the evidence are treated as
  equivalent to any non-alphanumeric separator in the OCR text, so "Table 1."
  matches "TABLE 1:" without any fuzzy scoring and therefore no false positives.
- Markdown tables from Mistral OCR now render as proper HTML tables. `commonmark`
  is called with `extensions = TRUE` to enable GFM pipe-table support.
- LaTeX-style superscripts used by Mistral OCR (`^{a}`, `^{b}`, …) are converted
  to `<sup>` tags before rendering.

### Bug fixes

- Evidence span highlighting was completely broken: `row_map` was sending bare
  integers `[0, 1, 2]` to the JS handler which expected `[{id, tier}]` objects,
  so every `querySelectorAll('[data-ev-id="undefined"]')` found nothing.
- Tooltip no longer shows a ✓ checkmark for matched sentences; the warning ⚠️
  is kept only for sentences not found in the OCR text.

---

## 0.1.7 (2026-06-01)

### Bug fixes
- OCR viewer and table no longer block on document load. `build_evidence_index`
  was running a O(n×m) cosine-trigram sliding window inside a high-priority
  Shiny observer, preventing `renderDataTable` from executing. Evidence span
  injection is now deferred via `session$onFlushed()` so the OCR and table
  appear immediately; spans are injected in the following flush.
- `table_trigger` is now incremented outside the records `tryCatch` so the
  table always re-renders when switching documents, even if record loading
  fails.
- `render_tensorlake_html` now supports the Mistral OCR format
  (`{"index": N, "markdown": "..."}` per page) in addition to the tensorlake
  format. Previously, documents OCR'd by Mistral showed a blank preview.

---

## 0.1.6 (2026-05-28)

### Improvements
- OCR highlighting rewritten: evidence spans are now pre-injected into the
  HTML once per document load (`build_evidence_index`) rather than re-rendered
  on every row selection. Row switching is a pure client-side CSS toggle with
  no server round-trip, eliminating the OCR panel flash.
- `find_best_match_in_html` gains an `exact_only` parameter; `build_evidence_index`
  uses exact substring matching only (instant) while `get_highlight_matches`
  retains the fuzzy sliding-window path for other callers.
- `html_to_plain_text` helper added for clean entity-decoded plain text
  extraction used by the span injection pipeline.
- All third-party JS/CSS assets vendored locally (`inst/app/www/`): split.js
  1.6.5 and mark.js 8.11.1. Font Awesome CDN replaced with the `fontawesome`
  R package. Eliminates all runtime CDN dependencies.
- `fontawesome` added to `Imports`.

### Bug fixes
- Removed polyfill.io script tag (CDN was compromised in 2024 and injected
  malicious JavaScript).
- Removed MathJax CDN dependency (unused after OCR rendering refactor).

---

## 0.1.5 (2026-05-27)

### Improvements
- split.js CDN URL pinned to v1.6.5 (was unpinned, a supply-chain risk).

---

## 0.1.4 (2026-05-26)

### Bug fixes
- Removed MathJax CDN script and config block.
- Removed polyfill.io script (compromised CDN, initial removal).

---

## 0.1.3 (2026-05-25)

### Improvements
- Package version number displayed next to the GitHub link in the title bar.
- Hover tooltip on OCR highlighted sentences shows the full list of supporting
  sentences for the selected row.

### Bug fixes
- Fixed unescaped double quotes in tooltip JS string that caused `app.R` parse
  errors.

---

## 0.1.2 and earlier

See git log for earlier history.
