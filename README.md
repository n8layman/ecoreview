# ecoreview

Shiny app for reviewing and validating ecological data extracted with the [ecoextract](https://github.com/n8layman/ecoextract) package.

## Overview

**ecoreview** is part of a three-package workflow for extracting and validating structured data from scientific literature:

1. **[ohseer](https://github.com/n8layman/ohseer)** - OCR processing to extract text from PDFs
2. **[ecoextract](https://github.com/n8layman/ecoextract)** - LLM-powered structured data extraction from OCR text
3. **ecoreview** - Human-in-the-loop review and validation interface (this package)

After ecoextract processes your documents, use ecoreview to review, edit, and validate the extracted records with a side-by-side document viewer.

## Installation

```r
# Install from GitHub
renv::install("n8layman/ecoreview")

# or with remotes
remotes::install_github("n8layman/ecoreview")
```

## Usage

### Basic Usage

```r
library(ecoreview)

# Launch with defaults (looks for ecoextract_records.db in current directory)
run_app()
```

### Customized Usage

```r
library(ecoreview)

# Launch with custom configuration
run_app(
  title = "ChiroScan: Bat Interaction Review",
  app_name = "ChiroScan",
  github_url = "https://github.com/n8layman/bat-interactions",
  export_prefix = "bat_interactions"
)
```

### Column Display Configuration

Use `priority_cols` to pin the most important columns to the left, and `visible_cols` to
show only the columns reviewers need. Neither parameter affects the underlying database —
they are display-only settings.

```r
run_app(
  title = "ChiroScan: Bat Interaction Review",
  # Show these columns first (left-most), in this order
  priority_cols = c("Pathogen_Name", "Host_Name",
                    "Detection_Result_Direction", "Observation_Type"),
  # Show only these columns on load (others hidden but still editable via Columns button)
  visible_cols  = c("Pathogen_Name", "Host_Name",
                    "Detection_Result_Direction", "Observation_Type",
                    "Host_Species", "Pathogen_Species")
)
```

Reviewers can further adjust column order and visibility at any time using the **Columns**
button above the table, which opens a drag-and-drop panel with Visible and Hidden zones.

## Features

- **Side-by-side document review**: View OCR text or original PDF alongside extracted records
- **Inline cell editing**: Edit extracted data directly in the table with change tracking
- **Column management**: Reorder and show/hide columns via a drag-and-drop panel — database is never modified
- **Evidence highlighting**: Click a record to highlight supporting text in the document
- **Audit trail**: Full history of all edits made during review
- **Accuracy metrics**: Precision, recall, and F1 scores calculated from verified documents
- **Document navigation**: Toggle the dropdown between filename and document ID display to jump directly to a known ID
- **Export Records CSV**: Downloads all non-deleted records joined with document metadata, with wide columns (OCR text, reasoning) stripped for readability
- **Export Documents CSV**: Downloads a summary of all documents — ID, filename, author, year, title, and pipeline statuses (OCR, metadata, extraction, refinement)
- **Save Database**: Download the current SQLite file with all edits applied
- **Schema migration warning**: Alerts on connect if the database uses a pre-UUID schema that requires migration

## Requirements

- R >= 4.0
- [ecoextract](https://github.com/n8layman/ecoextract) package (and its database output)

## Getting Started with the Full Workflow

If you're new to the ecosystem, start with [ecoextract](https://github.com/n8layman/ecoextract):

```r
# 1. Install the extraction packages
pak::pak("n8layman/ohseer")
pak::pak("n8layman/ecoextract")

# 2. Process your PDFs
library(ecoextract)
results <- process_documents(
  pdf_path = "path/to/pdfs/",
  db_conn = "ecoextract_records.db"
)

# 3. Review and validate the results
renv::install("n8layman/ecoreview")
library(ecoreview)
run_app(db_path = "ecoextract_records.db")
```

See the [ecoextract documentation](https://github.com/n8layman/ecoextract) for complete workflow details, including API setup, custom schemas, and parallel processing.

## Parallel / Team Review

`ecoreview::split_db()` divides a database into `n` roughly-equal part files so multiple reviewers can each work through their own subset independently. `ecoreview::combine_db()` merges the finished parts back into a single database, accepting either a directory path (auto-discovers `*_part_N.db` files) or an explicit vector of paths. Before writing any data, `combine_db()` validates that all parts share the same schema and errors with a clear message if they differ, preventing silent data corruption from mismatched versions. Opening the combined database in `run_app()` gives full accuracy metrics across all reviewers — verified documents, field-level edits, and deletion flags are all preserved exactly, so the accuracy modal reflects the complete picture of the review.

```r
# Split into 4 sequential parts (lowest document IDs first) — written next to the source file
parts <- ecoreview::split_db("ecoextract_records.db", n = 4)
# → ecoextract_records_part_1.db ... ecoextract_records_part_4.db

# Random assignment instead of sequential
parts <- ecoreview::split_db("ecoextract_records.db", n = 4, random = TRUE, seed = 42)

# Each reviewer launches the app pointed at their own part
ecoreview::run_app(db_path = "ecoextract_records_part_2.db")

# Once all reviewers are done, recombine (supply directory or explicit paths)
ecoreview::combine_db(
  "path/to/parts/",
  output_path = "ecoextract_records_reviewed.db"
)
```

## License

GPL-3
