# ecoreview

Shiny app for reviewing and validating ecological data extracted with the ecoextract package.

## Installation

```r
# Install from GitHub
remotes::install_github("n8layman/ecoreview")
```

## Usage

### Basic Usage

```r
library(ecoreview)

# Launch with defaults
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
- [ecoextract](https://github.com/n8layman/ecoextract) package

## License

GPL-3
