#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Purpose: Summarize all non-linked IPUMS census .dta files (those starting
#          with "census_") in data_raw/census_ipums into ONE markdown file.
# I/O:     Input  = data_raw/census_ipums/census_*.dta
#          Output = analysis_output/census_files_sum_stats.md
# Notes:
#   - Excludes: census_full_count_1850_1950_mh_poor_institutions.dta
#   - High-cardinality is defined as 250+ distinct non-missing values.
#   - If distinct < 250, the output prints the full value distribution
#     (counts and percents) for any variable type.
#   - For numeric variables, prints: min, p10, p25, median, p75, p90, max,
#     and a mode (exact when feasible, otherwise binned midpoint).
#   - Numeric summaries are also printed for labelled numeric variables when
#     they are high-cardinality (250+ distinct).
#   - For 250+ distinct values, prints 5 example non-missing values.
#   - For Stata-labelled variables, value distributions include BOTH the
#     numeric code and the value label where available.
#   - Numeric formatting is vectorized (prevents accidental recycling).
#   - Does not create any output directory.
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(haven)

#=#=#=#=#=#=#=#=#=#=#=#=
# Main function ####
#=#=#=#=#=#=#=#=#=#=#=#=

main <- function() {
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Paths
  #=#=#=#=#=#=#=#=#=#=#=#
  
  census_ipums_dir <- file.path(data_raw, "census_ipums")
  output_md_path <- file.path(analysis_output, "census_files_sum_stats.md")
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Identify input files
  #=#=#=#=#=#=#=#=#=#=#=#
  
  census_files <- list.files(
    path = census_ipums_dir,
    pattern = "^census_.*\\.dta$",
    full.names = TRUE
  ) %>%
    sort()
  
  excluded_file_name <- "census_full_count_1850_1950_mh_poor_institutions.dta"
  
  census_files <- census_files %>%
    keep(~ basename(.x) != excluded_file_name)
  
  message(stringr::str_glue("Found {length(census_files)} census_*.dta files in: {census_ipums_dir} (after exclusions)"))
  message(stringr::str_glue("Writing single markdown output to: {output_md_path}"))
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Open markdown connection and stream output
  #=#=#=#=#=#=#=#=#=#=#=#
  
  con <- base::file(output_md_path, open = "wt")
  on.exit(close(con), add = TRUE)
  
  write_md_lines(
    con = con,
    lines = c(
      "# Census Files Summary Statistics",
      "",
      stringr::str_glue("- Generated: {as.character(Sys.time())}"),
      stringr::str_glue("- Input directory: `{census_ipums_dir}`"),
      "- Scope: All `.dta` files beginning with `census_` (non-linked files).",
      stringr::str_glue("- Excluded file: `{excluded_file_name}`"),
      "- High-cardinality threshold: 250+ distinct non-missing values.",
      "- Numeric rounding: Integer detection; otherwise rounded to 3 decimals and trimmed.",
      ""
    )
  )
  
  purrr::walk(
    census_files,
    ~ {
      message(stringr::str_glue("Starting file: {basename(.x)}"))
      file_lines <- summarize_census_file_to_md_lines(
        input_file_path = .x,
        distinct_threshold = 250
      )
      write_md_lines(con = con, lines = file_lines)
      gc()
      message(stringr::str_glue("Finished file: {basename(.x)}"))
    }
  )
  
  message("All done.")
}

#=#=#=#=#=#=#=#=#=#=#=#
# Helper functions ####
#=#=#=#=#=#=#=#=#=#=#=#

summarize_census_file_to_md_lines <- function(input_file_path,
                                              distinct_threshold) {
  
  file_name <- basename(input_file_path)
  
  message(stringr::str_glue("Loading .dta: {input_file_path}"))
  census_df <- haven::read_dta(input_file_path)
  
  n_rows <- nrow(census_df)
  n_cols <- ncol(census_df)
  
  md_lines <- c(
    stringr::str_glue("## {file_name}"),
    "",
    stringr::str_glue("- Observations: {format(n_rows, big.mark = ',')}"),
    stringr::str_glue("- Variables: {format(n_cols, big.mark = ',')}"),
    ""
  )
  
  var_names <- names(census_df)
  
  message(stringr::str_glue("Summarizing {length(var_names)} variables for: {file_name}"))
  
  for (i in seq_along(var_names)) {
    
    var_name <- var_names[[i]]
    
    if (i %% 25 == 0) {
      message(stringr::str_glue("Progress: {i} of {length(var_names)} variables in {file_name}"))
    }
    
    x <- census_df[[var_name]]
    
    var_lines <- summarize_one_variable_to_md_lines(
      x = x,
      var_name = var_name,
      file_name = file_name,
      distinct_threshold = distinct_threshold
    )
    
    md_lines <- c(md_lines, var_lines)
  }
  
  md_lines <- c(md_lines, "")
  
  rm(census_df)
  
  md_lines
}

summarize_one_variable_to_md_lines <- function(x,
                                               var_name,
                                               file_name,
                                               distinct_threshold,
                                               sample_n_for_cardinality = 200000,
                                               mode_unique_limit_exact = 2000,
                                               mode_bins_for_numeric = 50) {
  
  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_non_missing <- n_total - n_missing
  pct_missing <- ifelse(n_total > 0, n_missing / n_total, NA_real_)
  
  var_label <- attr(x, "label") %||% ""
  var_label <- as_label_string(var_label, max_len = 80)
  
  class_chr <- paste(class(x), collapse = "|")
  
  label_lookup_df <- extract_label_lookup_df(x)
  is_labelled <- !is.null(label_lookup_df)
  
  is_numeric_like <- is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")
  
  # Compute sample-based distinctness
  sample_idx <- seq_len(min(n_total, sample_n_for_cardinality))
  sample_vals <- x[sample_idx]
  sample_vals_non_missing <- sample_vals[!is.na(sample_vals)]
  sample_unique_n <- compute_sample_unique_n(sample_vals_non_missing)
  
  # Compute full distinct if likely below threshold
  full_unique_n <- NA_integer_
  if (sample_unique_n < distinct_threshold) {
    full_unique_n <- compute_full_unique_n(x)
  }
  
  is_high_cardinality <- ifelse(
    !is.na(full_unique_n),
    full_unique_n >= distinct_threshold,
    sample_unique_n >= distinct_threshold
  )
  
  distinct_display <- ifelse(
    !is.na(full_unique_n),
    as.character(full_unique_n),
    stringr::str_glue(">= {distinct_threshold} (sample distinct = {sample_unique_n})")
  )
  
  md_lines <- c(
    stringr::str_glue("### {var_name}"),
    "",
    stringr::str_glue("- Label: {sanitize_md_inline(var_label)}"),
    stringr::str_glue("- Class: `{class_chr}`"),
    stringr::str_glue("- Missing: {format(n_missing, big.mark = ',')} / {format(n_total, big.mark = ',')} ({format_pct(pct_missing)})"),
    stringr::str_glue("- Distinct (non-missing): {distinct_display}"),
    stringr::str_glue("- Has Stata value labels: {ifelse(is_labelled, 'Yes', 'No')}"),
    ""
  )
  
  # Numeric summary
  print_numeric_summary <- is_numeric_like && (!is_labelled || is_high_cardinality)
  
  if (print_numeric_summary) {
    
    numeric_stats_df <- compute_numeric_summary_df(
      x = x,
      mode_unique_limit_exact = mode_unique_limit_exact,
      mode_bins_for_numeric = mode_bins_for_numeric
    )
    
    md_lines <- c(
      md_lines,
      "**Numeric summary**",
      "",
      format_md_table(numeric_stats_df),
      ""
    )
  }
  
  # Value distribution if distinct < threshold (for any variable type)
  if (!is_high_cardinality) {
    
    value_dist_df <- compute_value_distribution_df(
      x = x,
      n_total = n_total,
      label_lookup_df = label_lookup_df
    )
    
    md_lines <- c(
      md_lines,
      "**Value distribution (all distinct values)**",
      "",
      format_md_table(value_dist_df),
      ""
    )
    
  } else {
    
    examples_chr <- compute_examples_chr(
      x = x,
      label_lookup_df = label_lookup_df,
      n_examples = 5
    )
    
    md_lines <- c(
      md_lines,
      "**Examples (5)**",
      "",
      stringr::str_glue("- {examples_chr}"),
      ""
    )
  }
  
  md_lines
}

compute_numeric_summary_df <- function(x,
                                       mode_unique_limit_exact,
                                       mode_bins_for_numeric) {
  
  is_date <- inherits(x, "Date")
  is_posix <- inherits(x, "POSIXct") || inherits(x, "POSIXt")
  
  x_num <- x
  if (is_date) x_num <- as.numeric(x)
  if (is_posix) x_num <- as.numeric(x)
  
  x_num_non_missing <- x_num[!is.na(x_num)]
  
  if (length(x_num_non_missing) == 0) {
    numeric_stats_df <- tibble(
      stat = c("min", "p10", "p25", "median", "p75", "p90", "max", "mode", "mode_type"),
      value = rep(NA_character_, 9)
    )
    numeric_stats_df
  } else {
    
    q <- stats::quantile(
      x_num_non_missing,
      probs = c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1),
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
    
    mode_out <- compute_numeric_mode(
      x_num_non_missing = x_num_non_missing,
      is_date = is_date,
      is_posix = is_posix,
      mode_unique_limit_exact = mode_unique_limit_exact,
      mode_bins_for_numeric = mode_bins_for_numeric
    )
    
    numeric_stats_df <- tibble(
      stat = c("min", "p10", "p25", "median", "p75", "p90", "max", "mode", "mode_type"),
      value = c(
        format_numeric_for_md(q[1], is_date = is_date, is_posix = is_posix),
        format_numeric_for_md(q[2], is_date = is_date, is_posix = is_posix),
        format_numeric_for_md(q[3], is_date = is_date, is_posix = is_posix),
        format_numeric_for_md(q[4], is_date = is_date, is_posix = is_posix),
        format_numeric_for_md(q[5], is_date = is_date, is_posix = is_posix),
        format_numeric_for_md(q[6], is_date = is_date, is_posix = is_posix),
        format_numeric_for_md(q[7], is_date = is_date, is_posix = is_posix),
        mode_out$mode_value,
        mode_out$mode_type
      )
    )
    
    numeric_stats_df
  }
}

compute_numeric_mode <- function(x_num_non_missing,
                                 is_date,
                                 is_posix,
                                 mode_unique_limit_exact,
                                 mode_bins_for_numeric) {
  
  sample_unique_n <- length(unique(x_num_non_missing[seq_len(min(length(x_num_non_missing), 200000))]))
  
  if (sample_unique_n <= mode_unique_limit_exact) {
    
    freq_tab <- sort(table(x_num_non_missing), decreasing = TRUE)
    if (length(freq_tab) == 0) {
      mode_value <- NA_character_
      mode_type <- NA_character_
    } else {
      mode_raw <- as.numeric(names(freq_tab)[1])
      mode_value <- format_numeric_for_md(mode_raw, is_date = is_date, is_posix = is_posix)
      mode_type <- "Exact"
    }
    
  } else {
    
    rng <- range(x_num_non_missing)
    if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] < rng[2]) {
      breaks <- seq(from = rng[1], to = rng[2], length.out = mode_bins_for_numeric + 1)
      h <- hist(x_num_non_missing, breaks = breaks, plot = FALSE)
      idx <- which.max(h$counts)
      mode_mid <- (h$breaks[idx] + h$breaks[idx + 1]) / 2
      mode_value <- format_numeric_for_md(mode_mid, is_date = is_date, is_posix = is_posix)
      mode_type <- stringr::str_glue("Binned_midpoint (bins = {mode_bins_for_numeric})")
    } else {
      mode_value <- NA_character_
      mode_type <- NA_character_
    }
  }
  
  list(
    mode_value = mode_value,
    mode_type = mode_type
  )
}

compute_value_distribution_df <- function(x,
                                          n_total,
                                          label_lookup_df) {
  
  if (!is.null(label_lookup_df)) {
    
    x_num <- suppressWarnings(as.numeric(x))
    
    counts_df <- tibble(value_num = x_num) %>%
      mutate(is_missing = is.na(value_num)) %>%
      filter(!is_missing) %>%
      count(value_num, name = "n") %>%
      arrange(desc(n), value_num) %>%
      left_join(label_lookup_df, by = "value_num") %>%
      mutate(
        label = dplyr::coalesce(label, NA_character_),
        pct_total = n / n_total
      ) %>%
      select(value_num, label, n, pct_total)
    
    if (any(is.na(x_num))) {
      counts_df <- counts_df %>%
        bind_rows(
          tibble(
            value_num = NA_real_,
            label = "<NA>",
            n = sum(is.na(x_num)),
            pct_total = sum(is.na(x_num)) / n_total
          )
        )
    }
    
    counts_df %>%
      mutate(
        pct_total = format_pct(pct_total),
        value_num = if_else(is.na(value_num), NA_character_, format_numeric_plain(value_num)),
        label = dplyr::if_else(is.na(label), "", label),
        label = sanitize_md_inline(label)
      )
    
  } else {
    
    if (is.factor(x)) {
      x_val <- as.character(x)
    } else if (is.logical(x)) {
      x_val <- as.character(x)
    } else if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
      x_val <- as.character(x)
    } else {
      x_val <- as.character(x)
    }
    
    counts_df <- tibble(value = x_val) %>%
      count(value, name = "n", sort = TRUE) %>%
      mutate(
        pct_total = n / n_total
      ) %>%
      arrange(desc(n), value)
    
    counts_df %>%
      mutate(
        value = dplyr::if_else(is.na(value), "<NA>", value),
        pct_total = format_pct(pct_total),
        value = sanitize_md_inline(value)
      )
  }
}

compute_examples_chr <- function(x,
                                 label_lookup_df,
                                 n_examples) {
  
  x_non_missing <- x[!is.na(x)]
  
  if (length(x_non_missing) == 0) {
    "No non-missing values."
  } else if (!is.null(label_lookup_df)) {
    
    x_num <- suppressWarnings(as.numeric(x_non_missing))
    unique_vals <- unique(x_num)
    example_vals <- unique_vals[seq_len(min(n_examples, length(unique_vals)))]
    
    example_df <- tibble(value_num = example_vals) %>%
      left_join(label_lookup_df, by = "value_num") %>%
      mutate(
        value_num_chr = format_numeric_plain(value_num),
        example = ifelse(
          is.na(label),
          value_num_chr,
          stringr::str_glue("{label} ({value_num_chr})")
        )
      )
    
    example_df$example %>%
      sanitize_md_inline() %>%
      paste(collapse = " | ")
    
  } else {
    
    x_chr <- if (is.factor(x_non_missing)) as.character(x_non_missing) else as.character(x_non_missing)
    unique_vals <- unique(x_chr)
    unique_vals[seq_len(min(n_examples, length(unique_vals)))] %>%
      sanitize_md_inline() %>%
      paste(collapse = " | ")
  }
}

extract_label_lookup_df <- function(x) {
  
  if (haven::is.labelled(x) && !is.null(attr(x, "labels"))) {
    labels_vec <- attr(x, "labels")
    label_lookup_df <- tibble(
      value_num = as.numeric(unname(labels_vec)),
      label = names(labels_vec)
    ) %>%
      distinct(value_num, label)
    label_lookup_df
  } else if (!is.null(attr(x, "labels"))) {
    
    labels_vec <- attr(x, "labels")
    suppressWarnings({
      value_num <- as.numeric(unname(labels_vec))
    })
    
    if (all(is.na(value_num))) {
      NULL
    } else {
      label_lookup_df <- tibble(
        value_num = value_num,
        label = names(labels_vec)
      ) %>%
        distinct(value_num, label)
      
      label_lookup_df
    }
    
  } else {
    NULL
  }
}

compute_sample_unique_n <- function(sample_vals_non_missing) {
  
  if (length(sample_vals_non_missing) == 0) {
    0L
  } else if (is.factor(sample_vals_non_missing)) {
    length(unique(as.character(sample_vals_non_missing)))
  } else {
    length(unique(sample_vals_non_missing))
  }
}

compute_full_unique_n <- function(x) {
  
  if (length(x) == 0) {
    0L
  } else if (is.factor(x)) {
    nlevels(x)
  } else {
    x_non_missing <- x[!is.na(x)]
    if (length(x_non_missing) == 0) 0L else length(unique(x_non_missing))
  }
}

format_md_table <- function(df) {
  
  if (nrow(df) == 0) {
    return("*No rows to display.*")
  }
  
  df_chr <- df %>%
    mutate(across(everything(), ~ sanitize_md_inline(as.character(.x))))
  
  header <- paste0("| ", paste(names(df_chr), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(names(df_chr))), collapse = " | "), " |")
  
  body <- purrr::pmap_chr(
    df_chr,
    ~ {
      row_vals <- c(...)
      paste0("| ", paste(row_vals, collapse = " | "), " |")
    }
  )
  
  c(header, sep, body)
}

sanitize_md_inline <- function(x) {
  
  x %>%
    stringr::str_replace_all("\\|", "\\\\|") %>%
    stringr::str_replace_all("\n", " ") %>%
    stringr::str_squish()
}

format_pct <- function(x) {
  
  ifelse(
    is.na(x),
    NA_character_,
    stringr::str_glue("{round(100 * x, 2)}%")
  )
}

write_md_lines <- function(con,
                           lines) {
  
  writeLines(text = lines, con = con, sep = "\n")
}

format_numeric_for_md <- function(v,
                                  is_date,
                                  is_posix,
                                  decimals = 3) {
  
  if (length(v) == 0 || is.na(v)) return(NA_character_)
  
  if (is_date) {
    return(as.character(as.Date(v, origin = "1970-01-01")))
  }
  
  if (is_posix) {
    return(as.character(as.POSIXct(v, origin = "1970-01-01", tz = "UTC")))
  }
  
  format_numeric_plain(v, decimals = decimals)
}

format_numeric_plain <- function(v,
                                 decimals = 3) {
  
  # Vectorized numeric formatting:
  # - Integer-like -> no decimals
  # - Otherwise -> round to `decimals`, trim trailing zeros
  
  v_num <- suppressWarnings(as.numeric(v))
  
  out <- rep(NA_character_, length(v_num))
  
  ok <- !is.na(v_num)
  
  if (!any(ok)) return(out)
  
  int_like <- rep(FALSE, length(v_num))
  int_like[ok] <- abs(v_num[ok] - round(v_num[ok])) < 1e-9
  
  out[ok & int_like] <- as.character(as.integer(round(v_num[ok & int_like])))
  
  if (any(ok & !int_like)) {
    
    v_round <- round(v_num[ok & !int_like], decimals)
    s <- format(v_round, scientific = FALSE, trim = TRUE)
    s <- stringr::str_replace_all(s, "\\s+", "")
    s <- stringr::str_replace(s, "(\\.[0-9]*?)0+$", "\\1")
    s <- stringr::str_replace(s, "\\.$", "")
    
    out[ok & !int_like] <- s
  }
  
  out
}

as_label_string <- function(x,
                            max_len = 80) {
  
  if (is.null(x) || length(x) == 0) return("")
  
  if (is.list(x)) x <- x[[1]]
  if (is.factor(x)) x <- as.character(x)
  
  if (!is.character(x)) x <- as.character(x)
  if (length(x) == 0) return("")
  
  x1 <- x[[1]]
  if (is.na(x1)) return("")
  
  x1 <- stringr::str_squish(x1)
  x1 <- substr(x1, 1, max_len)
  
  if (!nzchar(x1)) "" else x1
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#=#=#=#=#=#=#=#
# EXECUTE ####
#=#=#=#=#=#=#=#

main()
