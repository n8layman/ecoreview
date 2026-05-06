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

## Features

- **Side-by-side document review**: View OCR text or original PDF alongside extracted records
- **Inline cell editing**: Edit extracted data directly in the table with change tracking
- **Evidence highlighting**: Click a record to highlight supporting text in the document
- **Audit trail**: Full history of all edits made during review
- **Accuracy metrics**: Precision, recall, and F1 scores calculated from verified documents
- **Export options**: Download reviewed data as CSV or save the database file

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
pak::pak("n8layman/ecoreview")
library(ecoreview)
run_app(db_path = "ecoextract_records.db")
```

See the [ecoextract documentation](https://github.com/n8layman/ecoextract) for complete workflow details, including API setup, custom schemas, and parallel processing.

## Parallel / Team Review

When a database is too large for one person to review, split it into equal-sized parts, distribute them to reviewers, then recombine once everyone is done.

```r
library(ecoreview)

# Split into 4 parts (one per reviewer) — output written next to the source file
parts <- split_db("ecoextract_records.db", n = 4)
# → ecoextract_records_part_1.db ... ecoextract_records_part_4.db

# Each reviewer launches the app pointed at their own part
run_app(db_path = "ecoextract_records_part_2.db")

# Once all reviewers are done, combine from the directory
combine_db(
  "path/to/parts/",            # scans for *_part_N.db files automatically
  output_path = "ecoextract_records_reviewed.db"
)

# Or supply paths explicitly
combine_db(
  c("ecoextract_records_part_1.db",
    "ecoextract_records_part_2.db",
    "ecoextract_records_part_3.db",
    "ecoextract_records_part_4.db"),
  output_path = "ecoextract_records_reviewed.db"
)
```

`combine_db()` validates that all parts share the same schema before merging and errors with a clear message if they differ, so accidental structural changes (e.g. a schema upgrade applied to only one part) are caught before any data is written to the output file.

## License

GPL-3
