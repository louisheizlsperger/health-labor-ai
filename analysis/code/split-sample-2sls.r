#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Purpose: First stage & reduced form using “new-hospital distance” IVs
#          with county-cluster SEs, first-stage F-stats, and RAW (endogenous)
#          second stage (OLS): outcomes ~ mh_inst_1870.
#          + Includes county controls:
#              (i) geometric population growth 1870→1880 (annualized),
#              (ii) log population in 1870.
# I/O:     Input  = build_output/censuslink_1870_1880_linked_wide_with_distance.dta
#                   file.path(data_raw_census_ipums,"census_full_count_1880_outcome_variables_reduced_linked.dta")
#                   file.path(build_output,"mh_inst_counts_by_county_1850_1950_panel.dta")
#          Output = analysis_output/*.md (stargazer tables)
# Notes:
#   - Endogenous (1880) regressor alias: mh_inst == mh_inst_1880 (from linked file)
#   - Instrument set (switchable list):
#       * dist_new_hospital_1871_1879_100km
#       * dist_new_hospital_1871_1879_100km_capped
#       * log_dist_new_hospital_1871_1879
#   - Instruments are already in 100 km units (except the log spec).
#   - Outcomes: famsize_tc (<=15), married_spouse_present, labforce_in (LPMs).
#   - Only county-clustered SEs (clusters = statefip_1870 × countyicp_1870).
#   - Tables show coefficient estimates for instrument and ALL controls.
#   - Stats shown: N, R^2, and F (cluster) for the main regressor in each column.
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# Main function ####
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

main <- function() {
  
  #=#=#=#=#=#=#=#=#=#=#=#=#
  # Paths and input files ####
  #=#=#=#=#=#=#=#=#=#=#=#=#
  linked_with_dist_path <- file.path(build_output, "censuslink_1870_1880_linked_wide_with_distance.dta")
  merged_df_path        <- file.path(data_raw_census_ipums, "censuslink_1870_1880_linked_wide_with_distance_and_outcomes.dta")
  outcomes_path         <- file.path(data_raw_census_ipums, "census_full_count_1880_outcome_variables_reduced_linked.dta")
  county_panel_path     <- file.path(build_output, "mh_inst_counts_by_county_1850_1950_panel.dta")
  
  # Instruments to run (edit this list in one place)
  instruments <- c(
    "dist_new_hospital_1871_1879_100km",
    "dist_new_hospital_1871_1879_100km_capped",
    "log_dist_new_hospital_1871_1879"
  )
  
  # Outcomes to run
  outcomes <- c("famsize_tc", "married_spouse_present", "labforce_in")
  
  # Controls (always included)
  controls <- c("county_geo_pop_grow_1870_1880", "county_log_pop_1870")
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Load and prepare data ####
  #=#=#=#=#=#=#=#=#=#=#=#
  message("Loading merged linked microdata with outcomes ...")
  if (file.exists(merged_df_path)) {
    df <- haven::read_dta(merged_df_path)
  } else {
    linked <- haven::read_dta(linked_with_dist_path) %>%
      mutate(across(where(haven::is.labelled), ~ haven::as_factor(., levels = "labels"))) %>%
      mutate(histid_1880_chr = normalize_histid(histid_1880))
    outcomes_raw <- haven::read_dta(outcomes_path) %>%
      mutate(histid_chr = normalize_histid(histid))
    df <- linked %>%
      left_join(outcomes_raw %>% select(-histid), by = c("histid_1880_chr" = "histid_chr"))
    rm(linked, outcomes_raw)
  }
  
  # Outcomes & endogenous regressor definitions + IVs
  df <- df %>%
    mutate(
      mh_inst = as.integer(mh_inst_1880),
      famsize_tc = pmin(as.integer(famsize), 15L),
      marst_lbl    = haven::as_factor(marst, levels = "labels"),
      labforce_lbl = haven::as_factor(labforce, levels = "labels"),
      married_spouse_present = as.integer(marst_lbl == "married, spouse present"),
      labforce_in = case_when(
        labforce_lbl == "yes, in the labor force" ~ 1L,
        labforce_lbl == "no, not in the labor force" ~ 0L,
        TRUE ~ NA_integer_
      ),
      mh_inst_1870 = case_when(
        !is.na(gqtyped_1870) & gqtyped_1870 %in% c(300L, 461L, 462L) ~ 1L,
        !is.na(gqtyped_1870)                                         ~ 0L,
        TRUE                                                         ~ NA_integer_
      ),
      dist_new_hospital_1871_1879_100km        = dist_new_hospital_1871_1879 / 100,
      dist_new_hospital_1871_1879_100km_capped = if_else(dist_new_hospital_1871_1879_100km > 2, 2, dist_new_hospital_1871_1879_100km),
      log_dist_new_hospital_1871_1879          = if_else(dist_new_hospital_1871_1879 > 0, log(dist_new_hospital_1871_1879), NA_real_),
      
      county_id = paste0(statefip_1870, "_", countyicp_1870)
    )
  
  # County controls from county panel (1870→1880 growth; log 1870 pop)
  message("Loading county panel and constructing population controls ...")
  county_controls <- haven::read_dta(county_panel_path) %>%
    mutate(across(where(haven::is.labelled), ~ haven::as_factor(., levels = "labels"))) %>%
    mutate(year = as.integer(as.character(year))) %>%
    select(statefip, countyicp, year, n) %>%
    group_by(statefip, countyicp, year) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
    filter(year %in% c(1870, 1880)) %>%
    pivot_wider(names_from = year, values_from = n, names_prefix = "n_") %>%
    mutate(
      county_geo_pop_grow_1870_1880 = if_else(!is.na(n_1870) & !is.na(n_1880) & n_1870 > 0,
                                              (n_1880 / n_1870)^(1/10) - 1, NA_real_),
      county_log_pop_1870           = if_else(!is.na(n_1870) & n_1870 > 0, log(n_1870), NA_real_)
    ) %>%
    select(statefip, countyicp, county_geo_pop_grow_1870_1880, county_log_pop_1870)
  
  df <- df %>%
    left_join(county_controls,
              by = c("statefip_1870" = "statefip",
                     "countyicp_1870" = "countyicp"))
  
  # Report control availability
  control_missing_share <- mean(!stats::complete.cases(df[, controls]))
  message(sprintf("[CHECK] Share with missing county controls: %0.2f%%", 100 * control_missing_share))
  
  #=#=#=#=#=#=#=#=#=#=#=#
  # Variable labels ####
  #=#=#=#=#=#=#=#=#=#=#=#
  
  # Use labelstar to apply labels
  df <- labelstar::assign_labels(df, variable_labels)
  
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
  # FIRST STAGE (per instrument) ####
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
  
  # Build first stage specifications
  formulas_fs <- purrr::map(
    instruments,
    ~ build_formula(y = "mh_inst", 
                    x_vec = c(controls, .x),
                    cluster_var = "county_id")
  )
  
  labels_fs <- labelstar::get_labels(formulas_fs, df)
  
  models_fs <- purrr::map(
    .x = formulas_fs,
    .f = ~ lfe::felm(.x, data = df)
  )
  
  # Cluster-robust F on the instrument only (single excluded IV ⇒ t^2)
  summs  <- lapply(models_fs, summary)
  F_instr <- mapply(function(s, iv) {
    tval <- s$coefficients[iv, "t value"]
    as.numeric(tval)^2
  }, summs, instruments)
  
  # Cluster-robust overall F for all non-intercept slopes (instrument + controls)
  F_overall <- vapply(models_fs, function(m) {
    cf <- stats::coef(m)
    keep <- setdiff(names(cf), "(Intercept)")
    keep <- keep[!is.na(cf[keep])]
    V <- stats::vcov(m)[keep, keep, drop = FALSE]
    b <- cf[keep]
    q <- length(keep)
    if (q == 0) return(NA_real_)
    as.numeric(t(b) %*% solve(V, b) / q)
  }, numeric(1))
  
  # Append both rows to your existing add.lines
  labels_fs$add_lines <- c(
    labels_fs$add_lines,
    list(c("F (cluster) on instrument", sprintf("%.3f", F_instr))),
    list(c("F (cluster) overall",   sprintf("%.3f", F_overall)))
  )
  
  fs_table <- capture.output(
    stargazer(models_fs, 
            type = 'text',
            # Variable labels
            dep.var.labels = labels_fs$dep_var_label,
            covariate.labels = labels_fs$covariate_labels,
            # Additional table details
            add.lines = labels_fs$add_lines,
            notes.append = FALSE,
            notes = labels_fs$table_notes,
            digits = 6,
            keep.stat = c("n","rsq","f")
            )
  )
  print(fs_table)
  
  # Write a .md file to drag into a GitHub issue or copy from
  out_md <- file.path(analysis_output, "first_stage_regression_table.md")
  cat(c("### First stage (county-clustered SEs)", "", "```", fs_table, "```", ""),
      sep = "\n", file = out_md)
  
  #=#=#=#=#=#=#=#=#=#=#=#=#=#
  # REDUCED FORM (per IV) ####
  #=#=#=#=#=#=#=#=#=#=#=#=#=#
  
  purrr::walk(instruments, function(iv) {
    
    # outcome ~ controls + iv | 0 | 0 | county_id  (clustered at county)
    formulas_rf <- purrr::map(
      outcomes,
      ~ build_formula(
        y = .x,
        x_vec = c(controls, iv),
        cluster_var = "county_id"
      )
    )
    
    # Labels (show raw DV means)
    labels_rf <- labelstar::get_labels(formulas_rf, df, dep_var_means = "raw")
    
    # Fit models
    models_rf <- purrr::map(formulas_rf, ~ lfe::felm(.x, data = df))
    
    # Capture a GitHub-friendly text table
    rf_table <- capture.output(
      stargazer(
        models_rf,
        type             = "text",
        dep.var.labels   = labels_rf$dep_var_label,
        covariate.labels = labels_rf$covariate_labels,
        add.lines        = labels_rf$add_lines,
        notes.append     = FALSE,
        notes            = labels_rf$table_notes,
        digits           = 6,
        keep.stat        = c("n","rsq")    # no F, no adj.R2, no residual SE
      )
    )
    print(rf_table)
    
    # Write one .md file per IV
    safe_iv <- gsub("[^A-Za-z0-9_-]", "", iv)
    out_md  <- file.path(analysis_output, paste0("reduced_form_", safe_iv, ".md"))
    header  <- sprintf("### Reduced form (county-clustered SEs) — instrument: `%s`", iv)
    
    cat(c(header, "", "```", rf_table, "```", ""),
        sep = "\n", file = out_md)
  })
  
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
  # RAW (ENDOGENOUS) SECOND STAGE — OLS ####
  #=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
  
  formulas_ols <- purrr::map(
      outcomes,
      ~ build_formula(
        y = .x,
        x_vec = c(controls, "mh_inst_1870"),
        cluster_var = "county_id"
      )
    )
    
  # Labels (show raw DV means)
  labels_ols <- labelstar::get_labels(formulas_ols, df, dep_var_means = "raw")
    
  # Fit models (county-clustered SEs)
  models_ols <- purrr::map(formulas_ols, ~ lfe::felm(.x, data = df))
  
  # Capture a text table (no adj.R2, no residual SE, no F-stat)
  lines <- capture.output(
    stargazer(
      models_ols,
      type             = "text",
      dep.var.labels   = labels_ols$dep_var_label,
      covariate.labels = labels_ols$covariate_labels,
      add.lines        = labels_ols$add_lines,
      notes.append     = FALSE,
      notes            = labels_ols$table_notes,
      digits           = 6,
      keep.stat        = c("n","rsq")
    )
  )
  
  # Print to console
  cat(paste(lines, collapse = "\n"), "\n")
  
  # Write a .md file for GitHub
  out_md <- file.path(analysis_output, "raw_second_stage_endogenous.md")
  header <- "### Raw (endogenous) second stage — OLS (county-clustered SEs)"
  cat(c(header, "", "```", lines, "```", ""), sep = "\n", file = out_md)

  message("Done. All tables printed to console and written to analysis_output/*.md")
  
}

#=#=#=#=#=#=#=#=#=#=#=#
# Helper functions ####
#=#=#=#=#=#=#=#=#=#=#=#

# Build a formula: y ~ x1 + x2 + ...
build_formula <- function(y, x_vec, cluster_var) {
  rhs <- paste(x_vec, collapse = " + ")
  stats::as.formula(paste0(y, " ~ ", rhs, " | 0 | 0 | ", cluster_var))
}

# Compute county-clustered SEs aligned to the estimation sample
clustered_se <- function(fit, data) {
  mf <- stats::model.frame(fit)
  rn <- rownames(mf)
  if (suppressWarnings(all(!is.na(as.integer(rn))))) {
    ridx <- as.integer(rn)
  } else {
    ridx <- match(rn, rownames(data))
  }
  if (any(is.na(ridx))) {
    stop("Could not align cluster IDs to the fitted sample. Check row names.")
  }
  cl <- interaction(data$statefip_1870[ridx], data$countyicp_1870[ridx], drop = TRUE)
  vc <- sandwich::vcovCL(fit, cluster = cl)
  se <- sqrt(diag(vc))
  out <- unname(se[names(stats::coef(fit))])
  names(out) <- names(stats::coef(fit))
  out
}

# Wald F-stat on a single term using cluster SEs (t^2)
wald_f_for_term <- function(fit, se_vec, term) {
  cf <- stats::coef(fit)
  if (!term %in% names(cf)) return(NA_real_)
  est <- unname(cf[term]); se <- unname(se_vec[term])
  if (is.na(est) || is.na(se) || se == 0) return(NA_real_)
  (est / se)^2
}

# Normalize histid (as in previous scripts)
normalize_histid <- function(x) {
  if (is.double(x)) {
    out <- format(x, scientific = FALSE, trim = TRUE, digits = 22)
    out <- sub("\\.0+$", "", out)
  } else {
    out <- as.character(x)
  }
  tolower(trimws(out))
}

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

#== Encapsulated table builders =================================================

# First stage for a single instrument: prints and writes a .md file
run_first_stage_for_iv <- function(df, iv, controls, var_labels, out_dir) {
  
  
  fst <- stats::lm(build_formula("mh_inst", c(iv, controls)), data = df)
  se_fst <- clustered_se(fst, df)
  f_iv   <- wald_f_for_term(fst, se_fst, iv)
  
  coef_order <- names(stats::coef(fst))
  coef_order_no_int <- setdiff(coef_order, "(Intercept)")
  cov_labels <- purrr::map_chr(coef_order_no_int, ~ var_labels[[.x]] %||% .x)
  
  out_title <- stringr::str_glue("First Stage — mh_inst ~ {iv_lbl} + controls (county-clustered SEs)")
  add_lines <- list(c("F (cluster) on main regressor", formatC(f_iv, digits = 3, format = "f")))
  
  lines <- capture.output(
    stargazer::stargazer(
      fst,
      se = list(se_fst),
      type = "text",
      title = out_title,
      dep.var.caption = "Dependent variable:",
      dep.var.labels  = var_labels[["mh_inst"]],
      covariate.labels = cov_labels,
      omit = "(Intercept)",
      omit.stat = c("ser"),
      add.lines = add_lines,
      column.labels = NULL,
      header = FALSE
    )
  )
  cat(paste(lines, collapse = "\n"), "\n")
  writeLines(lines, file.path(out_dir, stringr::str_glue("first_stage__{iv}.md")))
  invisible(NULL)
}

# Reduced form for a single instrument across outcomes
run_reduced_form_for_iv <- function(df, iv, outcomes, controls, var_labels, outcome_labels, out_dir) {
  iv_lbl <- var_labels[[iv]] %||% iv
  
  rf_models <- purrr::map(outcomes, ~ stats::lm(build_formula(.x, c(iv, controls)), data = df))
  rf_ses    <- purrr::map(rf_models, ~ clustered_se(.x, df))
  rf_Fs     <- purrr::map_dbl(rf_models, ~ wald_f_for_term(.x, clustered_se(.x, df), iv))
  
  coef_order <- names(stats::coef(rf_models[[1]]))
  coef_order_no_int <- setdiff(coef_order, "(Intercept)")
  cov_labels <- purrr::map_chr(coef_order_no_int, ~ var_labels[[.x]] %||% .x)
  
  out_title <- stringr::str_glue("Reduced Form — outcomes ~ {iv_lbl} + controls (county-clustered SEs)")
  add_lines <- list(c("F (cluster) on main regressor",
                      paste(formatC(rf_Fs, digits = 3, format = "f"), collapse = "  |  ")))
  
  lines <- capture.output(
    stargazer::stargazer(
      rf_models,
      se = rf_ses,
      type = "text",
      title = out_title,
      dep.var.caption = "Outcome:",
      column.labels = outcome_labels,
      covariate.labels = cov_labels,
      omit = "(Intercept)",
      omit.stat = c("ser"),
      add.lines = add_lines,
      header = FALSE
    )
  )
  cat(paste(lines, collapse = "\n"), "\n")
  writeLines(lines, file.path(out_dir, stringr::str_glue("reduced_form__{iv}.md")))
  invisible(NULL)
}

# Raw (endogenous) second stage across outcomes
run_raw_second_stage <- function(df, outcomes, controls, var_labels, outcome_labels, out_dir) {
  ss_models <- purrr::map(outcomes, ~ stats::lm(build_formula(.x, c("mh_inst_1870", controls)), data = df))
  ss_ses    <- purrr::map(ss_models, ~ clustered_se(.x, df))
  ss_Fs     <- purrr::map_dbl(ss_models, ~ wald_f_for_term(.x, clustered_se(.x, df), "mh_inst_1870"))
  
  coef_order <- names(stats::coef(ss_models[[1]]))
  coef_order_no_int <- setdiff(coef_order, "(Intercept)")
  cov_labels <- purrr::map_chr(coef_order_no_int, ~ var_labels[[.x]] %||% .x)
  
  out_title <- "Raw (Endogenous) Second Stage — OLS: outcomes ~ Institutionalized in 1870 + controls (county-clustered SEs)"
  add_lines <- list(c("F (cluster) on main regressor",
                      paste(formatC(ss_Fs, digits = 3, format = "f"), collapse = "  |  ")))
  
  lines <- capture.output(
    stargazer::stargazer(
      ss_models,
      se = ss_ses,
      type = "text",
      title = out_title,
      dep.var.caption = "Outcome:",
      column.labels = outcome_labels,
      covariate.labels = cov_labels,
      omit = "(Intercept)",
      omit.stat = c("ser"),
      add.lines = add_lines,
      header = FALSE
    )
  )
  cat(paste(lines, collapse = "\n"), "\n")
  writeLines(lines, file.path(out_dir, "raw_second_stage_endogenous.md"))
  invisible(NULL)
}

#=#=#=#=#=#=#=#
# Call external scripts ####
#=#=#=#=#=#=#=#

source(file.path(analysis_code, "variable-labels.r"))

#=#=#=#=#=#=#=#
# EXECUTE ####
#=#=#=#=#=#=#=#

# main()