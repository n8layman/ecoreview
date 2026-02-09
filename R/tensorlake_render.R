# Tensorlake OCR JSON rendering functions for Shiny

#' Render tensorlake JSON content to HTML for Shiny display
#'
#' @param doc_content JSON string or parsed list from tensorlake OCR
#' @return HTML string for display in Shiny htmlOutput
#' @export
render_tensorlake_html <- function(doc_content) {
  if (is.null(doc_content) || (is.character(doc_content) && doc_content == "") ||
      (is.character(doc_content) && is.na(doc_content))) {
    return("<div style='color: #999; padding: 20px; text-align: center;'>No OCR content available</div>")
  }

  # Parse JSON if needed
  if (is.character(doc_content)) {
    doc_content <- tryCatch({
      jsonlite::fromJSON(doc_content, simplifyVector = FALSE)
    }, error = function(e) {
      # If not valid JSON, return as plain text
      return(list(list(text = doc_content)))
    })
  }

  # Helper: convert page elements to Markdown
  build_page_markdown <- function(page) {
    md_parts <- list()

    # Page header (citation/title)
    if (!is.null(page$page_header)) {
      md_parts <- c(md_parts, unlist(page$page_header))
    }

    # Section headers
    if (!is.null(page$section_header)) {
      md_parts <- c(md_parts, unlist(page$section_header))
    }

    # Main text
    if (!is.null(page$text)) {
      md_parts <- c(md_parts, page$text)
    }

    # Combine Markdown content
    paste(md_parts, collapse = "\n\n")
  }

  # Convert tables to HTML
  convert_tables <- function(tables) {
    if (is.null(tables) || length(tables) == 0) return("")

    table_html <- sapply(tables, function(tbl) {
      if (!is.null(tbl$html)) {
        tbl$html  # Use raw HTML if available
      } else if (!is.null(tbl$rows)) {
        # Convert structured table to HTML
        header_html <- if (!is.null(tbl$headers)) {
          paste0("<thead><tr>", paste0("<th>", tbl$headers, "</th>", collapse = ""), "</tr></thead>")
        } else ""

        rows_html <- paste0(sapply(tbl$rows, function(row) {
          paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")
        }), collapse = "")

        paste0("<table class='tensorlake-table'>", header_html, "<tbody>", rows_html, "</tbody></table>")
      } else ""
    })

    paste(table_html, collapse = "\n")
  }

  # Build HTML per page
  pages_html <- sapply(doc_content, function(page) {
    page_md <- build_page_markdown(page)
    md_html <- tryCatch({
      commonmark::markdown_html(page_md)
    }, error = function(e) {
      paste0("<p>", htmltools::htmlEscape(page_md), "</p>")
    })

    table_html <- convert_tables(page$tables)

    page_num <- if (!is.null(page$page_number)) {
      paste0("<div class='page-number'>Page ", page$page_number, "</div>")
    } else ""

    paste0("<div class='ocr-page'>", page_num, md_html, table_html, "</div>")
  })

  # Return combined HTML
  paste(pages_html, collapse = "\n")
}
