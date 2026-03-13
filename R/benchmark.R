# Benchmark analysis pipeline
#
# Functions for collecting, standardizing, and analyzing benchmark trial results.
# These handle steps 2-3 of the benchmark pipeline (step 1 = extraction, in ecoextract).

#' @importFrom rlang .data syms
#' @importFrom utils head
NULL

#' Collect all benchmark trial results into a single CSV
#'
#' Reads all `benchmark_trial_*.db` files from a directory, exports records
#' using [ecoextract::export_db()], adds a `replicate` column, and writes
#' the combined results to `benchmark_records.csv`.
#'
#' @param benchmark_dir Directory containing `benchmark_trial_*.db` files
#' @return Tibble of all records across all replicates (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' records <- collect_benchmark_results("data/test_variability")
#' }
collect_benchmark_results <- function(benchmark_dir) {
  if (!requireNamespace("ecoextract", quietly = TRUE)) {
    stop("Package 'ecoextract' is required for collect_benchmark_results(). ",
         "Install it with: remotes::install_github('n8layman/ecoextract')")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required for collect_benchmark_results(). ",
         "Install it with: install.packages('purrr')")
  }

  benchmark_dir <- normalizePath(benchmark_dir, mustWork = TRUE)

  # Always start fresh - delete previous CSV and name mappings
  for (f in c("benchmark_records.csv", "name_mappings_host.csv", "name_mappings_pathogen.csv")) {
    fp <- file.path(benchmark_dir, f)
    if (file.exists(fp)) {
      cat(sprintf("Removing %s\n", f))
      file.remove(fp)
    }
  }

  db_files <- list.files(
    benchmark_dir,
    pattern = "^benchmark_trial_.*\\.db$",
    full.names = TRUE
  ) |> sort()

  stopifnot("No benchmark_trial_*.db files found" = length(db_files) > 0)
  cat(sprintf("Collecting results from %d trial databases\n", length(db_files)))

  all_records <- purrr::map_dfr(seq_along(db_files), function(i) {
    ecoextract::export_db(db_conn = db_files[i]) |>
      dplyr::mutate(replicate = i)
  })

  output_csv <- file.path(benchmark_dir, "benchmark_records.csv")
  readr::write_csv(all_records, output_csv)
  cat(sprintf("Saved %s records to %s\n",
              format(nrow(all_records), big.mark = ","), output_csv))

  invisible(all_records)
}


#' Standardize names across benchmark replicates
#'
#' Reads `benchmark_records.csv`, pools all unique host/pathogen names, sends
#' each to an LLM for standardization, and adds standardized columns to the CSV.
#' Name mappings are cached to `name_mappings_host.csv` / `name_mappings_pathogen.csv`
#' to avoid re-running the LLM on subsequent calls.
#'
#' @param benchmark_dir Directory containing `benchmark_records.csv`
#' @param model LLM model to use for standardization (default: Claude via ellmer)
#' @param name_type One of `"host"`, `"pathogen"`, or `"both"`
#' @return The updated records tibble (also overwrites `benchmark_records.csv`)
#' @export
#'
#' @examples
#' \dontrun{
#' records <- standardize_benchmark_names("data/test_variability")
#' }
standardize_benchmark_names <- function(
    benchmark_dir,
    model = NULL,
    name_type = "both"
) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for standardize_benchmark_names(). ",
         "Install it with: install.packages('ellmer')")
  }

  benchmark_dir <- normalizePath(benchmark_dir, mustWork = TRUE)

  records_csv <- file.path(benchmark_dir, "benchmark_records.csv")
  stopifnot("benchmark_records.csv not found - run collect_benchmark_results() first" =
              file.exists(records_csv))

  all_records <- readr::read_csv(records_csv, show_col_types = FALSE)
  cat(sprintf("Loaded %s records from %s\n",
              format(nrow(all_records), big.mark = ","), basename(records_csv)))

  # Drop any existing standardized columns to avoid duplicates on re-run
  all_records <- all_records |>
    dplyr::select(-dplyr::any_of(c("Host_Name_std", "Pathogen_Name_std")))

  # Standardize host names (or load from existing CSV cache)
  host_csv <- file.path(benchmark_dir, "name_mappings_host.csv")
  if (name_type %in% c("host", "both")) {
    if (file.exists(host_csv)) {
      cat(sprintf("Loading existing host mappings from %s\n", host_csv))
      host_map <- readr::read_csv(host_csv, show_col_types = FALSE)
    } else {
      unique_hosts <- sort(unique(all_records$Host_Name))
      cat(sprintf("Unique host names to standardize: %d\n", length(unique_hosts)))
      host_map <- standardize_name_vector(unique_hosts, "host", model)
      readr::write_csv(host_map, host_csv)
      cat(sprintf("Saved host mappings to %s\n", host_csv))
    }

    all_records <- all_records |>
      dplyr::left_join(host_map |> dplyr::select("original", "canonical"),
                       by = c("Host_Name" = "original")) |>
      dplyr::rename(Host_Name_std = "canonical")
  }

  # Standardize pathogen names (or load from existing CSV cache)
  pathogen_csv <- file.path(benchmark_dir, "name_mappings_pathogen.csv")
  if (name_type %in% c("pathogen", "both")) {
    if (file.exists(pathogen_csv)) {
      cat(sprintf("Loading existing pathogen mappings from %s\n", pathogen_csv))
      pathogen_map <- readr::read_csv(pathogen_csv, show_col_types = FALSE)
    } else {
      unique_pathogens <- sort(unique(all_records$Pathogen_Name))
      cat(sprintf("Unique pathogen names to standardize: %d\n", length(unique_pathogens)))
      pathogen_map <- standardize_name_vector(unique_pathogens, "pathogen", model)
      readr::write_csv(pathogen_map, pathogen_csv)
      cat(sprintf("Saved pathogen mappings to %s\n", pathogen_csv))
    }

    all_records <- all_records |>
      dplyr::left_join(pathogen_map |> dplyr::select("original", "canonical"),
                       by = c("Pathogen_Name" = "original")) |>
      dplyr::rename(Pathogen_Name_std = "canonical")
  }

  # Overwrite CSV with standardized columns added
  readr::write_csv(all_records, records_csv)
  cat(sprintf("Updated %s with standardized name columns\n", basename(records_csv)))

  invisible(all_records)
}


#' Standardize a vector of names using an LLM
#'
#' Sends unique names to an LLM in batches for taxonomic standardization.
#' Resolves common names, misspellings, outdated nomenclature, and
#' parenthetical qualifiers to canonical scientific names.
#'
#' @param names Character vector of unique names to standardize
#' @param name_type `"host"` or `"pathogen"` -- determines the system prompt
#' @param model Optional model specification for [ellmer::chat_anthropic()]
#' @return Tibble with columns: `original`, `canonical`, `common_name`
#' @keywords internal
standardize_name_vector <- function(names, name_type = "host", model = NULL) {

  system_prompt <- if (name_type == "host") {
    paste(
      "You are a taxonomic name standardization expert.",
      "You will receive a JSON array of organism names extracted from scientific literature.",
      "These names may include scientific names, common names, abbreviations, or misspellings",
      "that refer to the same organism.",
      "",
      "Your task: resolve each name to a SINGLE standardized scientific name.",
      "Rules:",
      "- Resolve common names to scientific names (e.g., 'chicken' -> 'Gallus gallus domesticus', 'mouse' -> 'Mus musculus')",
      "- Preserve the level of taxonomic detail given - do NOT collapse subspecies to species",
      "  (e.g., 'Mustela sibirica coreana' stays as-is, do not reduce to 'Mustela sibirica')",
      "- STRIP all parenthetical qualifiers - output ONLY the scientific name",
      "  (e.g., 'Bos taurus (cattle)' -> 'Bos taurus', 'Aedes caspius (mosquito)' -> 'Aedes caspius')",
      "- STRIP common name suffixes after the scientific name",
      "  (e.g., 'Bos taurus cattle' -> 'Bos taurus')",
      "- Fix obvious misspellings and formatting inconsistencies",
      "- Names that clearly refer to the same organism MUST map to the EXACT same string",
      "- If a name is already a valid scientific name at any rank, keep it as-is",
      "- If a name cannot be resolved, return it unchanged but still strip parenthetical qualifiers",
      "- Be aware that older papers may use outdated nomenclature",
      "- The canonical form must be a clean scientific name with NO parenthetical annotations",
      sep = "\n"
    )
  } else {
    paste(
      "You are a pathogen taxonomy standardization expert.",
      "You will receive a JSON array of pathogen names extracted from scientific literature.",
      "These names may include full species names, abbreviations, serotypes, or variant spellings",
      "that refer to the same pathogen.",
      "",
      "Your task: resolve each name to a SINGLE standardized form.",
      "Rules:",
      "- Preserve the level of detail given - do NOT collapse serotypes/genotypes to species",
      "  (e.g., 'DENV-1' and 'DENV-2' stay separate, 'Dengue virus type 1' -> 'Dengue virus 1')",
      "- Strain-level variants should map to the species name only when the strain is just a lab label",
      "  (e.g., 'WEEV strain AS145' -> 'Western equine encephalitis virus')",
      "- STRIP all parenthetical qualifiers and annotations",
      "  (e.g., 'Foot-and-mouth disease virus (FMDV)' -> 'Foot-and-mouth disease virus')",
      "  (e.g., 'Clostridium perfringens (C. welchii)' -> 'Clostridium perfringens')",
      "- Fix obvious misspellings and resolve abbreviations to full names",
      "- Names referring to the same pathogen MUST map to the EXACT same string",
      "- If a name is already standard, keep it as-is",
      "- If a name cannot be resolved, return it unchanged but still strip parenthetical qualifiers",
      "- Be aware that older papers may use outdated nomenclature (e.g., pre-ICTV reclassification)",
      "- The canonical form must be a clean name with NO parenthetical annotations",
      sep = "\n"
    )
  }

  output_type <- ellmer::type_array(
    ellmer::type_object(
      original = ellmer::type_string("The original name exactly as provided"),
      canonical = ellmer::type_string("The standardized scientific name only, no parenthetical qualifiers"),
      common_name = ellmer::type_string("Common name or parenthetical qualifier if any, otherwise empty string")
    )
  )

  # Batch names to avoid token limits
  batch_size <- 50
  batches <- split(names, ceiling(seq_along(names) / batch_size))

  cat(sprintf("Sending %d %s names to LLM for standardization (%d batches of up to %d)...\n",
              length(names), name_type, length(batches), batch_size))

  result_df <- purrr::map_dfr(seq_along(batches), function(b) {
    batch <- batches[[b]]
    cat(sprintf("  Batch %d/%d (%d names)...\n", b, length(batches), length(batch)))

    chat <- ellmer::chat_anthropic(
      system_prompt = system_prompt,
      model = model
    )

    names_json <- jsonlite::toJSON(batch, auto_unbox = TRUE)
    prompt <- sprintf(
      "Standardize the following %s names. Return one mapping for each input name.\n\n%s",
      name_type,
      names_json
    )

    result <- chat$chat_structured(prompt, type = output_type)
    batch_df <- dplyr::bind_rows(result)

    # Overwrite original with input names by position - LLM may mangle the
    # original field (e.g., stripping parentheticals it was told to remove)
    if (nrow(batch_df) == length(batch)) {
      batch_df$original <- batch
    }

    batch_df
  })

  # Validate: every input name should have a mapping
  missing <- setdiff(names, result_df$original)
  if (length(missing) > 0) {
    warning(sprintf("%d names missing from LLM response: %s",
                    length(missing), paste(head(missing, 5), collapse = ", ")))
    result_df <- dplyr::bind_rows(
      result_df,
      tibble::tibble(original = missing, canonical = missing, common_name = "")
    )
  }

  n_changed <- sum(result_df$original != result_df$canonical)
  n_groups <- dplyr::n_distinct(result_df$canonical)
  cat(sprintf("  %d names -> %d canonical forms (%d names changed)\n",
              nrow(result_df), n_groups, n_changed))

  changed <- result_df |> dplyr::filter(.data$original != .data$canonical)
  if (nrow(changed) > 0) {
    cat("  Mappings applied:\n")
    for (i in seq_len(min(nrow(changed), 20))) {
      cat(sprintf("    %s -> %s\n", changed$original[i], changed$canonical[i]))
    }
    if (nrow(changed) > 20) cat(sprintf("    ... and %d more\n", nrow(changed) - 20))
  }

  result_df
}


#' Compute duplicate rate across benchmark trials
#'
#' Using harmonized names, counts within-paper-trial records sharing the same
#' unique field values. Measures how often the model produces duplicate records
#' that should have been merged.
#'
#' @param benchmark_dir Directory containing `benchmark_records.csv` (with
#'   standardized name columns from [standardize_benchmark_names()])
#' @param unique_fields Character vector of column names that define record
#'   uniqueness. Defaults to `c("Host_Name_std", "Pathogen_Name_std")`,
#'   matching the `x-unique-fields` schema for host-pathogen pair data.
#' @return A list with three components:
#'   \describe{
#'     \item{per_paper}{Tibble with duplicate rate per paper-trial}
#'     \item{per_trial}{Tibble with duplicate rate per trial (aggregate across papers)}
#'     \item{aggregate}{List with overall duplicate rate, total records, and total duplicates}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' dup_rates <- compute_duplicate_rate("data/test_variability")
#' dup_rates$aggregate
#' dup_rates$per_paper
#' }
compute_duplicate_rate <- function(
    benchmark_dir,
    unique_fields = c("Host_Name_std", "Pathogen_Name_std")
) {
  benchmark_dir <- normalizePath(benchmark_dir, mustWork = TRUE)

  records_csv <- file.path(benchmark_dir, "benchmark_records.csv")
  stopifnot("benchmark_records.csv not found - run collect_benchmark_results() first" =
              file.exists(records_csv))

  all_records <- readr::read_csv(records_csv, show_col_types = FALSE)

  # Check that unique_fields exist in data

  missing_fields <- setdiff(unique_fields, names(all_records))
  if (length(missing_fields) > 0) {
    stop(sprintf(
      "Fields not found in benchmark_records.csv: %s. Run standardize_benchmark_names() first?",
      paste(missing_fields, collapse = ", ")
    ))
  }

  # Per paper-trial: count duplicates
  per_paper <- all_records |>
    dplyr::group_by(.data$document_id, .data$file_name, .data$replicate) |>
    dplyr::mutate(
      dup_group = paste(!!!rlang::syms(unique_fields), sep = "|||")
    ) |>
    dplyr::summarise(
      total_records = dplyr::n(),
      unique_records = dplyr::n_distinct(.data$dup_group),
      duplicate_records = .data$total_records - .data$unique_records,
      duplicate_rate = .data$duplicate_records / .data$total_records,
      .groups = "drop"
    )

  # Per trial: aggregate across papers
  per_trial <- per_paper |>
    dplyr::group_by(.data$replicate) |>
    dplyr::summarise(
      total_records = sum(.data$total_records),
      unique_records = sum(.data$unique_records),
      duplicate_records = sum(.data$duplicate_records),
      duplicate_rate = .data$duplicate_records / .data$total_records,
      .groups = "drop"
    )

  # Overall aggregate
  total_records <- sum(per_paper$total_records)
  total_duplicates <- sum(per_paper$duplicate_records)
  aggregate <- list(
    total_records = total_records,
    total_duplicates = total_duplicates,
    duplicate_rate = if (total_records > 0) total_duplicates / total_records else 0
  )

  cat(sprintf("Duplicate rate: %.1f%% (%d duplicates out of %d records)\n",
              aggregate$duplicate_rate * 100,
              aggregate$total_duplicates,
              aggregate$total_records))

  list(
    per_paper = per_paper,
    per_trial = per_trial,
    aggregate = aggregate
  )
}
