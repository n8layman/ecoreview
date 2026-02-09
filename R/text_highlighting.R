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

#' Find best matching text segment in HTML
#'
#' @param html_text The HTML text to search
#' @param clean_evidence Cleaned evidence sentence
#' @param threshold Similarity threshold
#' @return Matched text segment or NULL
#' @keywords internal
find_best_match_in_html <- function(html_text, clean_evidence, threshold) {
  # Extract text segments from HTML (content between tags)
  # Match text that's not inside a tag
  text_segments <- stringr::str_extract_all(html_text, "(?<=>)[^<]+(?=<)")[[1]]

  # Also get text at start/end that might not be between tags
  text_segments <- c(text_segments, stringr::str_extract_all(html_text, "^[^<]+")[[1]])

  # Filter short segments
  text_segments <- text_segments[nchar(trimws(text_segments)) > 10]

  if (length(text_segments) == 0) return(NULL)

  best_match <- NULL
  best_score <- threshold

  for (segment in text_segments) {
    segment <- trimws(segment)
    clean_segment <- clean_sentence_for_comparison(segment)

    # Check word overlap first for efficiency
    evidence_words <- unlist(strsplit(tolower(clean_evidence), "\\s+"))
    segment_words <- unlist(strsplit(tolower(clean_segment), "\\s+"))
    word_overlap <- length(intersect(evidence_words, segment_words)) / max(length(evidence_words), 1)

    if (word_overlap < 0.3) next

    # Calculate similarity
    similarity <- stringdist::stringsim(clean_evidence, clean_segment, method = "jw")

    if (similarity > best_score) {
      best_score <- similarity
      best_match <- segment
    }

    # Also check substrings if segment is longer than evidence
    if (nchar(clean_segment) > nchar(clean_evidence) * 1.2) {
      # Sliding window approach
      window_size <- nchar(clean_evidence)
      for (start in seq(1, nchar(clean_segment) - window_size + 1, by = 20)) {
        substr_text <- substr(clean_segment, start, start + window_size - 1)
        sub_similarity <- stringdist::stringsim(clean_evidence, substr_text, method = "jw")
        if (sub_similarity > best_score) {
          # Find corresponding position in original segment
          original_substr <- substr(segment, start, start + window_size - 1)
          best_score <- sub_similarity
          best_match <- original_substr
        }
      }
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
