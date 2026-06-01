# ecoreview news

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
