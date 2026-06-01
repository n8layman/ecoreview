# Text highlighting functions for markdown rendering
# Pure functions for text highlighting and similarity matching

# Color palette for different evidence sentences
HIGHLIGHT_COLORS <- c(
  "#fff3cd",  # Yellow (original)
  "#cce5ff",  # Light blue
  "#d4edda",  # Light green
  "#f8d7da",  # Light red/pink
  "#e2d5f1",  # Light purple
  "#d1ecf1",  # Cyan
  "#ffeeba",  # Orange
  "#c3e6cb"   # Mint
)

HIGHLIGHT_BORDERS <- c(
  "#ffc107",  # Yellow border
  "#007bff",  # Blue border
  "#28a745",  # Green border
  "#dc3545",  # Red border
  "#6f42c1",  # Purple border
  "#17a2b8",  # Cyan border
  "#fd7e14",  # Orange border
  "#20c997"   # Mint border
)

#' Highlight similar text using direct text matching
#'
#' @param text The HTML text to highlight
#' @param all_source_sentences Vector of sentences to match against
#' @param similarity_threshold Minimum similarity score for highlighting
#' @return Text with highlighted matches
#' @export
highlight_similar_text <- function(text, all_source_sentences, similarity_threshold = 0.7) {
  if (length(all_source_sentences) == 0) return(text)

  working_text <- text
  color_index <- 1
  num_colors <- length(HIGHLIGHT_COLORS)

  # For each evidence sentence, try to find and highlight it in the text
  for (evidence in all_source_sentences) {
    if (is.null(evidence) || is.na(evidence) || nchar(evidence) < 10) next

    clean_evidence <- clean_sentence_for_comparison(evidence)

    # Try exact match first (after cleaning)
    # Extract text content between HTML tags and try to match
    match_result <- find_best_match_in_html(working_text, clean_evidence, similarity_threshold)

    if (!is.null(match_result)) {
      # Get color for this evidence sentence
      bg_color <- HIGHLIGHT_COLORS[((color_index - 1) %% num_colors) + 1]
      border_color <- HIGHLIGHT_BORDERS[((color_index - 1) %% num_colors) + 1]

      # Wrap the matched text with highlight using inline styles for unique colors
      highlighted <- paste0(
        '<mark class="text-highlight" style="background-color: ', bg_color,
        ' !important; border: 2px solid ', border_color, ' !important; box-shadow: 0 0 8px ',
        border_color, '40;">',
        match_result, '</mark>'
      )
      # Only replace first occurrence to avoid double-highlighting
      working_text <- sub(fixed = TRUE, match_result, highlighted, working_text)
      color_index <- color_index + 1
    }
  }

  return(working_text)
}

#' Strip HTML tags and decode entities to plain text
#'
#' @param html_text Raw HTML string
#' @return Plain text as it would appear in the rendered DOM
#' @keywords internal
html_to_plain_text <- function(html_text) {
  plain <- stringr::str_remove_all(html_text, "<[^>]+>")
  plain <- stringr::str_replace_all(plain, "&nbsp;",  " ")
  plain <- stringr::str_replace_all(plain, "&amp;",   "&")
  plain <- stringr::str_replace_all(plain, "&lt;",    "<")
  plain <- stringr::str_replace_all(plain, "&gt;",    ">")
  plain <- stringr::str_replace_all(plain, "&quot;",  "\"")
  plain <- stringr::str_replace_all(plain, "&#39;",   "'")
  stringr::str_squish(plain)
}

#' Find best matching span in plain text for an evidence fragment
#'
#' The evidence fragment IS the unit to locate. We search the plain text
#' (what mark.js sees in the DOM) using a sliding window of the same length
#' as the fragment so mark.js can find it directly.
#'
#' @param plain_text Plain text of the document (pre-stripped HTML)
#' @param clean_evidence Cleaned evidence fragment
#' @param threshold Similarity threshold
#' @return Matched text span from plain_text, or NULL
#' @keywords internal
find_best_match_in_html <- function(plain_text, clean_evidence, threshold) {
  ev_len <- nchar(clean_evidence)
  if (ev_len < 10 || nchar(plain_text) < ev_len) return(NULL)

  # Exact substring match first (case-insensitive)
  hit_pos <- regexpr(tolower(clean_evidence), tolower(plain_text), fixed = TRUE)[[1]]
  if (hit_pos > 0) {
    return(substr(plain_text, hit_pos, hit_pos + ev_len - 1))
  }

  # Sliding window of the same length as the evidence fragment.
  # Using cosine similarity on trigrams — more reliable than Jaro-Winkler
  # for strings of 20-150 characters.
  best_match <- NULL
  best_score <- threshold
  step       <- max(1L, ev_len %/% 20L)
  pt_len     <- nchar(plain_text)

  for (start in seq(1L, pt_len - ev_len + 1L, by = step)) {
    window       <- substr(plain_text, start, start + ev_len - 1L)
    clean_window <- clean_sentence_for_comparison(window)
    score        <- stringdist::stringsim(clean_evidence, clean_window,
                                          method = "cosine", q = 3L)
    if (score > best_score) {
      best_score <- score
      best_match <- window
      if (best_score > 0.95) break
    }
  }

  return(best_match)
}

#' Clean sentence for comparison by removing markdown and normalizing whitespace
#'
#' @param sentence Raw sentence text
#' @return Cleaned sentence text
#' @keywords internal
clean_sentence_for_comparison <- function(sentence) {
  cleaned <- stringr::str_replace_all(sentence, "\\$([^$]*)\\$", "\\1")
  cleaned <- stringr::str_remove_all(cleaned, "[*_]|<[^>]+>|^#{1,6}\\s*")
  cleaned <- stringr::str_squish(stringr::str_trim(cleaned))
  return(cleaned)
}

#' Get highlight match segments for client-side marking
#'
#' Runs server-side fuzzy matching and returns matched text segments with
#' colors for the browser to apply via mark.js. Avoids re-rendering the
#' OCR viewer HTML on every row selection.
#'
#' @param html Rendered HTML string to search within
#' @param evidence Character vector of evidence sentences to match
#' @param similarity_threshold Minimum similarity score (default 0.7)
#' @return List of lists, each with `text`, `bg_color`, `border_color`
#' @export
get_highlight_matches <- function(html, evidence, similarity_threshold = 0.7) {
  # Strip HTML once — this is the same plain text mark.js searches in the DOM
  plain_text <- html_to_plain_text(html)

  matches <- list()
  for (i in seq_along(evidence)) {
    ev <- evidence[[i]]
    if (is.null(ev) || is.na(ev) || nchar(ev) < 10) next
    clean_ev <- clean_sentence_for_comparison(ev)
    matched_text <- find_best_match_in_html(plain_text, clean_ev, similarity_threshold)
    if (!is.null(matched_text)) {
      idx <- (length(matches) %% length(HIGHLIGHT_COLORS)) + 1
      matches[[length(matches) + 1]] <- list(
        text         = matched_text,
        bg_color     = HIGHLIGHT_COLORS[[idx]],
        border_color = HIGHLIGHT_BORDERS[[idx]]
      )
    }
  }
  matches
}

#' Build a pre-injected evidence index for the OCR viewer
#'
#' Collects all unique supporting sentences from every row in the extracted
#' data frame, locates each in the rendered HTML via plain-text matching,
#' and injects \code{<span class="ecr-ev" data-ev-id="N">} wrappers in
#' place. Returns the modified HTML (rendered once per document) and a
#' row-to-ev-id mapping for the JS client so row switching is a pure
#' CSS-class toggle with no further server computation.
#'
#' @param html Rendered HTML string (from render_tensorlake_html)
#' @param extracted_df Data frame containing all_supporting_source_sentences
#' @param similarity_threshold Minimum cosine-trigram similarity (default 0.7)
#' @return Named list: \code{html} (modified HTML) and \code{row_map}
#'   (list keyed by 0-based row index, each element a list of ev_ids)
#' @export
build_evidence_index <- function(html, extracted_df,
                                 similarity_threshold = 0.7) {
  if (!"all_supporting_source_sentences" %in% names(extracted_df)) {
    return(list(html = html, row_map = list()))
  }

  plain_text    <- html_to_plain_text(html)
  modified_html <- html

  # sentence text -> ev_id  (first unique matched text wins)
  sentence_to_id <- list()
  # matched text  -> ev_id  (dedup: two sentences matching same span share id)
  matched_to_id  <- list()
  ev_id_counter  <- 0L

  # Parse sentences per row (keep NULLs for rows with no data)
  sentences_by_row <- lapply(seq_len(nrow(extracted_df)), function(i) {
    raw <- extracted_df$all_supporting_source_sentences[[i]]
    if (is.null(raw) || is.na(raw) || nchar(trimws(raw)) == 0) {
      return(character(0))
    }
    sents <- tryCatch(jsonlite::fromJSON(raw), error = function(e) as.character(raw))
    sents <- sents[!is.na(sents) & nchar(trimws(sents)) >= 10]
    sents
  })

  # Process unique sentences in document order
  seen <- character(0)
  for (row_sents in sentences_by_row) {
    for (sent in row_sents) {
      if (sent %in% seen) next
      seen <- c(seen, sent)

      clean_sent <- clean_sentence_for_comparison(sent)
      matched    <- find_best_match_in_html(plain_text, clean_sent,
                                            similarity_threshold)
      if (is.null(matched)) next

      # Reuse ev_id if this exact text was already injected
      if (!is.null(matched_to_id[[matched]])) {
        sentence_to_id[[sent]] <- matched_to_id[[matched]]
      } else {
        ev_id                   <- ev_id_counter
        ev_id_counter           <- ev_id_counter + 1L
        matched_to_id[[matched]] <- ev_id
        sentence_to_id[[sent]]  <- ev_id

        span          <- paste0('<span class="ecr-ev" data-ev-id="', ev_id,
                                '">', matched, "</span>")
        modified_html <- sub(matched, span, modified_html, fixed = TRUE)
      }
    }
  }

  # Build row_map: "0"-based string key -> list of ev_ids (preserving order)
  row_map <- stats::setNames(
    lapply(seq_len(nrow(extracted_df)), function(i) {
      sents  <- sentences_by_row[[i]]
      ids    <- unique(unlist(lapply(sents, function(s) sentence_to_id[[s]])))
      as.list(ids)
    }),
    as.character(seq_len(nrow(extracted_df)) - 1L)
  )

  list(html = modified_html, row_map = row_map)
}

#' Extract sentences from HTML or markdown text
#'
#' @param text HTML or markdown text to process
#' @return Vector of sentences
#' @export
extract_sentences_from_markdown <- function(text) {
  # First strip HTML tags to get plain text for sentence extraction
  # But preserve the structure by replacing block-level tags with newlines
  plain_text <- text

  # Replace block-level elements with newlines to preserve structure
  plain_text <- stringr::str_replace_all(plain_text, "</?(div|p|tr|li|h[1-6])[^>]*>", "\n")

  # Replace table cells with spaces
  plain_text <- stringr::str_replace_all(plain_text, "</?(td|th)[^>]*>", " ")

  # Remove all remaining HTML tags
  plain_text <- stringr::str_remove_all(plain_text, "<[^>]+>")

  # Decode common HTML entities
  plain_text <- stringr::str_replace_all(plain_text, "&nbsp;", " ")
  plain_text <- stringr::str_replace_all(plain_text, "&amp;", "&")
  plain_text <- stringr::str_replace_all(plain_text, "&lt;", "<")
  plain_text <- stringr::str_replace_all(plain_text, "&gt;", ">")
  plain_text <- stringr::str_replace_all(plain_text, "&quot;", "\"")

  lines <- unlist(strsplit(plain_text, "\n", fixed = TRUE))
  lines <- lines[nchar(trimws(lines)) > 5]

  all_sentences <- c()
  for (line in lines) {
    # Clean up whitespace before tokenizing
    line <- stringr::str_squish(line)
    if (nchar(line) < 5) next

    # Use tokenizers if available, otherwise fall back to simple split
    if (requireNamespace("tokenizers", quietly = TRUE)) {
      line_sentences <- tokenizers::tokenize_sentences(line, simplify = TRUE)
    } else {
      # Simple sentence splitting fallback
      line_sentences <- unlist(strsplit(line, "(?<=[.!?])\\s+", perl = TRUE))
    }
    line_sentences <- line_sentences[nchar(line_sentences) > 5]
    all_sentences <- c(all_sentences, line_sentences)
  }

  return(all_sentences)
}

#' Find sentence matches above threshold
#'
#' @param sentence_table Dataframe with sentence information
#' @param clean_evidence Vector of cleaned evidence sentences
#' @param similarity_threshold Minimum similarity threshold
#' @return Updated sentence_table with highlight column set
#' @keywords internal
find_sentence_matches <- function(sentence_table, clean_evidence, similarity_threshold) {
  for (i in seq_along(clean_evidence)) {
    clean_target <- clean_evidence[i]
    source_words <- unlist(strsplit(tolower(clean_target), "\\s+"))
    similarities <- calculate_sentence_similarities(sentence_table$clean_sentence, clean_target, source_words)

    max_similarity <- max(similarities, na.rm = TRUE)
    best_match_idx <- which.max(similarities)

    if (max_similarity >= similarity_threshold) {
      sentence_table$highlight[best_match_idx] <- TRUE
    }
  }

  return(sentence_table)
}

#' Calculate similarities between target sentence and candidate sentences
#'
#' @param candidate_sentences Vector of candidate sentences
#' @param clean_target Target sentence to match
#' @param source_words Words from the target sentence
#' @return Vector of similarity scores
#' @keywords internal
calculate_sentence_similarities <- function(candidate_sentences, clean_target, source_words) {
  sapply(seq_along(candidate_sentences), function(j) {
    target_sentence <- candidate_sentences[j]

    if (nchar(target_sentence) < nchar(clean_target) * 0.5) return(0)

    target_words <- unlist(strsplit(tolower(target_sentence), "\\s+"))
    word_overlap <- length(intersect(source_words, target_words)) / length(source_words)
    if (word_overlap < 0.3) return(0)

    best_similarity <- 0
    source_len <- nchar(clean_target)
    min_len <- max(30, source_len * 0.7)

    for (window_size in seq(source_len, min_len, by = -20)) {
      if (window_size > nchar(target_sentence)) next

      step_size <- max(10, window_size %/% 10)
      for (start_pos in seq(1, nchar(target_sentence) - window_size + 1, by = step_size)) {
        target_substr <- substr(target_sentence, start_pos, start_pos + window_size - 1)
        substr_similarity <- stringdist::stringsim(clean_target, target_substr, method = "jw")

        if (substr_similarity > best_similarity) {
          best_similarity <- substr_similarity
          if (best_similarity > 0.95) return(best_similarity)
        }
      }
    }

    return(best_similarity)
  })
}

#' Reconstruct text with highlights applied
#'
#' @param original_text Original markdown text
#' @param sentence_table Sentence table with highlight flags
#' @return Text with highlights applied
#' @keywords internal
reconstruct_text_with_highlights <- function(original_text, sentence_table) {
  working_text <- original_text

  for (i in seq_len(nrow(sentence_table))) {
    if (sentence_table$highlight[i]) {
      original_sentence <- sentence_table$raw_sentence[i]
      highlighted_sentence <- paste0('<mark class="text-highlight">', original_sentence, '</mark>')

      if (grepl(original_sentence, working_text, fixed = TRUE)) {
        working_text <- stringr::str_replace_all(working_text, stringr::fixed(original_sentence), highlighted_sentence)
      }
    }
  }

  return(working_text)
}
