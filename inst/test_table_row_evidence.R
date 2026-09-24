# test_table_row_evidence.R
#
# Tests for highlighting evidence copied from a markdown table row (#34).
#
# Evidence like "PDRR | PHODOPUS ... | LIV C | CZ" spans several <td>s once the
# OCR markdown is rendered, so a single span cannot wrap it. build_evidence_index()
# now matches such evidence to one <tr> and wraps each matched cell under one
# evidence id, falling back to sentence matching when no row matches.
#
# Run with: Rscript inst/test_table_row_evidence.R

devtools::load_all(quiet = TRUE)

pass_sym <- "✓"
fail_sym <- "✗"

pass_count <- 0L
fail_count <- 0L

check <- function(label, expr) {
  result <- tryCatch(isTRUE(expr), error = function(e) {
    cat(sprintf("  %s %s\n    unexpected error: %s\n", fail_sym, label, conditionMessage(e)))
    FALSE
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

# Text of the cells wrapped with a given ev_id, in order
spans_for <- function(html, id) {
  m <- regmatches(html, gregexpr(paste0('data-ev-id="', id, '">([^<]*)</span>'), html))[[1]]
  sub('.*">', "", sub("</span>$", "", m))
}

index_for <- function(html, evidence) {
  df <- data.frame(ev = vapply(evidence, function(e) as.character(jsonlite::toJSON(e)), character(1)))
  build_evidence_index(html, df, evidence_col = "ev", min_evidence_chars = 3)
}

md <- paste(
  "Shipment of hamsters and chinchillas.",
  "",
  "| Box number | Count | Species | Sex |",
  "|---|---|---|---|",
  "| Box number | 001 - 100 pcs. | Phodopus roborowski - Robo hamster | male |",
  "| Box number | 002 - 100 pcs. | Phodopus roborowski - Robo hamster | male |",
  "| Box number | 003 - 10 pcs. | Chinchilla lanigera - Chinchilla grey | female |",
  "",
  "| Code | Species | Unit | Source | Country | Venomous |",
  "|---|---|---|---|---|---|",
  "| PDRR | PHODOPUS ROBOROVSKII ROBOROVSKIS DESERT HAMSTER |  | LIV C | CZ | ☐ |",
  sep = "\n"
)
html <- render_tensorlake_html(jsonlite::toJSON(list(list(markdown = md)), auto_unbox = TRUE))

cat("\n=== Row evidence is matched to one <tr> ===\n")
r <- index_for(html, list("PDRR | PHODOPUS ROBOROVSKII ROBOROVSKIS DESERT HAMSTER |  | LIV C | CZ | ☐"))
check("record gets one evidence id", length(r$row_map[["0"]]) == 1L)
check("each non-empty cell is wrapped under that id",
      identical(spans_for(r$html, 0), c("PDRR", "PHODOPUS ROBOROVSKII ROBOROVSKIS DESERT HAMSTER", "LIV C", "CZ")))
check("nothing reported unmatched", length(r$unmatched_by_row[["0"]]) == 0L)

cat("\n=== Short and similar cells anchor to the right row ===\n")
r <- index_for(html, list("Box number | 002 - 100 pcs. | Phodopus roborowski - Robo hamster | male"))
check("box 002 evidence lands on the 002 row, not the near-identical 001 row",
      "002 - 100 pcs." %in% spans_for(r$html, 0) && !"001 - 100 pcs." %in% spans_for(r$html, 0))
check("short cell 'male' is highlighted within that row only", sum(spans_for(r$html, 0) == "male") == 1L)

cat("\n=== OCR noise and partial rows ===\n")
r <- index_for(html, list("Box number | 003 - 10 pcs. | Chinchila lanigera - Chinchilla grey | female"))
check("a misspelled long cell still matches its row", "003 - 10 pcs." %in% spans_for(r$html, 0))
r <- index_for(html, list("Box number | 003 - 10 pcs. | Chinchilla lanigera - Chinchilla grey | male"))
check("3 of 4 cells matching (75%) is enough", length(spans_for(r$html, 0)) == 3L)
r <- index_for(html, list("Box number | 009 - 10 pcs. | Mus musculus | male"))
check("a row matching too few cells is left unmatched", length(r$unmatched_by_row[["0"]]) == 1L)

cat("\n=== Header rows and duplicate evidence ===\n")
r <- index_for(html, list("Code | Species | Unit | Source | Country | Venomous"))
check("header row evidence matches the header <tr>", identical(spans_for(r$html, 0), c("Code", "Species", "Unit", "Source", "Country", "Venomous")))
r <- index_for(html, list("PDRR | PHODOPUS ROBOROVSKII ROBOROVSKIS DESERT HAMSTER | LIV C | CZ",
                          "PDRR | PHODOPUS ROBOROVSKII ROBOROVSKIS DESERT HAMSTER | LIV C | CZ"))
check("two records quoting the same row share one evidence id",
      identical(r$row_map[["0"]], r$row_map[["1"]]) && length(spans_for(r$html, 1)) == 0L)

cat("\n=== Fallbacks and unchanged paths ===\n")
para <- render_tensorlake_html(jsonlite::toJSON(list(list(
  markdown = "Box number 050 - 10 pcs. Chinchilla lanigera - Chinchilla grey male")), auto_unbox = TRUE))
r <- index_for(para, list("Box number | 050 - 10 pcs. | Chinchilla lanigera - Chinchilla grey | male"))
check("pipe evidence quoting a paragraph falls back to sentence matching", length(r$row_map[["0"]]) == 1L)
r <- index_for(html, list("Shipment of hamsters and chinchillas"))
check("evidence without pipes uses sentence matching", identical(spans_for(r$html, 0), "Shipment of hamsters and chinchillas"))
stripped <- gsub('<span class="ecr-ev" data-ev-id="[0-9]+">|</span>', "",
                 index_for(html, list("Box number | 001 - 100 pcs. | Phodopus roborowski - Robo hamster | male"))$html)
check("HTML is unchanged apart from the added spans, including non-ASCII text",
      identical(stripped, gsub("\\s+", " ", html, perl = TRUE)) && grepl("☐", stripped))

cat(sprintf("\n=== Results: %d passed, %d failed ===\n\n", pass_count, fail_count))

if (fail_count > 0L) quit(status = 1L)
