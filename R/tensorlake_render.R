# Tensorlake OCR JSON rendering functions for Shiny

#' Render tensorlake JSON content to HTML for Shiny display
#'
#' @param doc_content JSON string or parsed list from tensorlake OCR
#' @param ocr_images Optional OCR images, as stored in the ecoextract
#'   \code{documents.ocr_images} column (JSON string or parsed list with one
#'   entry per page). Each page's image placeholders (\code{![...](<id>)})
#'   are replaced with the stored image of the same \code{id} on that page.
#'   Placeholders with no stored image are shown as plain text rather than a
#'   broken image.
#' @return HTML string for display in Shiny htmlOutput
#' @export
render_tensorlake_html <- function(doc_content, ocr_images = NULL) {
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

  images_data <- parse_ocr_images(ocr_images)

  # Helper: convert page elements to Markdown
  build_page_markdown <- function(page) {
    # Mistral OCR format: {"index": N, "markdown": "...", "tables": [...]}
    if (!is.null(page$markdown)) {
      md <- page$markdown
      # Replace [tbl-N.md](tbl-N.md) placeholder links with actual table content
      if (!is.null(page$tables) && length(page$tables) > 0) {
        for (tbl in page$tables) {
          if (!is.null(tbl$id) && !is.null(tbl$content)) {
            md <- gsub(paste0("[", tbl$id, "](", tbl$id, ")"),
                       tbl$content, md, fixed = TRUE)
          }
        }
      }
      return(md)
    }

    # Tensorlake format: page_header / section_header / text fields
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
  pages_html <- vapply(seq_along(doc_content), function(page_idx) {
    page <- doc_content[[page_idx]]
    page_md <- build_page_markdown(page)
    if (!is.null(images_data) && page_idx <= length(images_data$pages)) {
      page_md <- embed_page_images(page_md, images_data$pages[[page_idx]]$images)
    }
    # Convert LaTeX-style superscripts used by Mistral OCR (e.g. ^{a}) to HTML
    page_md <- gsub("\\^\\{([^}]*)\\}", "<sup>\\1</sup>", page_md)
    md_html <- tryCatch({
      commonmark::markdown_html(page_md, extensions = TRUE)
    }, error = function(e) {
      paste0("<p>", htmltools::htmlEscape(page_md), "</p>")
    })

    table_html <- convert_tables(page$tables)

    # Page footer / other content (table footnotes, abbreviation keys) — rendered
    # AFTER tables so footnotes appear below the table they annotate.
    footer_html <- ""
    if (!is.null(page$other) && length(page$other) > 0) {
      footer_parts <- vapply(page$other, function(item) {
        # Skip "title" — already rendered in page_header / text
        if (identical(item$type, "title")) return("")
        if (!is.null(item$content) && nchar(trimws(item$content)) > 0) item$content else ""
      }, character(1))
      footer_parts <- footer_parts[nchar(trimws(footer_parts)) > 0]
      if (length(footer_parts) > 0) {
        footer_md <- paste(footer_parts, collapse = "\n\n")
        footer_md <- gsub("\\^\\{([^}]*)\\}", "<sup>\\1</sup>", footer_md)
        footer_rendered <- tryCatch({
          commonmark::markdown_html(footer_md, extensions = TRUE)
        }, error = function(e) {
          paste0("<p>", htmltools::htmlEscape(footer_md), "</p>")
        })
        footer_html <- paste0("<hr style='margin:8px 0;border-color:#dee2e6;'>", footer_rendered)
      }
    }

    page_num <- if (!is.null(page$page_number)) {
      paste0("<div class='page-number'>Page ", page$page_number, "</div>")
    } else if (!is.null(page$index)) {
      paste0("<div class='page-number'>Page ", page$index + 1L, "</div>")
    } else ""

    paste0("<div class='ocr-page'>", page_num, replace_missing_images(md_html),
           table_html, footer_html, "</div>")
  }, character(1))

  # Return combined HTML
  paste(pages_html, collapse = "\n")
}

#' Parse stored OCR images (internal)
#'
#' @param ocr_images JSON string or parsed list from \code{documents.ocr_images}
#' @return Parsed list with a \code{pages} element, or NULL
#' @keywords internal
parse_ocr_images <- function(ocr_images) {
  if (is.null(ocr_images) || length(ocr_images) == 0L) return(NULL)
  if (is.character(ocr_images)) {
    if (is.na(ocr_images[1]) || !nzchar(ocr_images[1])) return(NULL)
    ocr_images <- tryCatch(jsonlite::fromJSON(ocr_images[1], simplifyVector = FALSE),
                           error = function(e) NULL)
  }
  if (!is.list(ocr_images) || length(ocr_images$pages) == 0L) return(NULL)
  ocr_images
}

#' Embed one page's stored images in its markdown (internal)
#'
#' Replaces each placeholder whose link target is a stored image's \code{id}
#' with an \code{<img>} tag holding that image's data. Images are matched by
#' \code{id}, not by position: Mistral numbers image ids across the whole
#' document, so a page's first image is not necessarily \code{img-0}.
#'
#' @param page_md Markdown for one page
#' @param images The page's \code{images} list from \code{ocr_images}, each
#'   with \code{id} and \code{image_base64}
#' @return Markdown with placeholders replaced
#' @keywords internal
embed_page_images <- function(page_md, images) {
  for (img in images) {
    id <- img$id
    data <- img$image_base64
    if (is.null(id) || is.null(data) || !nzchar(id) || !nzchar(data)) next
    if (!grepl("^data:image/", data)) data <- paste0("data:image/png;base64,", data)
    pattern <- paste0("!\\[[^]]*\\]\\(", gsub("([][{}()*+?.\\\\^$|#-])", "\\\\\\1", id, perl = TRUE), "\\)")
    tag <- paste0('<img src="', data, '" alt="', htmltools::htmlEscape(id, attribute = TRUE),
                  '" style="max-width: 100%; height: auto;" />')
    page_md <- gsub(pattern, tag, page_md, perl = TRUE)
  }
  page_md
}

#' Show unresolved image placeholders as text (internal)
#'
#' Replaces \code{<img>} tags whose source is a bare relative file name (an
#' OCR placeholder such as \code{img-0.jpeg} with no stored image) with their
#' name in muted text, so the viewer shows no broken image icons.
#'
#' @param html HTML string
#' @return HTML string
#' @keywords internal
replace_missing_images <- function(html) {
  pattern <- '<img\\b[^>]*\\bsrc="(?!data:|https?:|/)([^"]*)"[^>]*>'
  m <- gregexpr(pattern, html, perl = TRUE, useBytes = TRUE)
  if (m[[1]][1] == -1L) return(html)
  tags <- regmatches(html, m)[[1]]
  names <- sub('.*\\bsrc="([^"]*)".*', "\\1", tags, perl = TRUE)
  regmatches(html, m) <- list(paste0('<span style="color:#6c757d;font-style:italic;">[',
                                     htmltools::htmlEscape(names), ']</span>'))
  Encoding(html) <- "UTF-8"
  html
}
