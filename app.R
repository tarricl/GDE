# =============================================================================
# GDE Candidate Method Equivalence Assessment - Shiny dashboard prototype v0.23
# =============================================================================
#
# App folder name for this release:
#   GDE_v023
#
# Workflow engine:
#   equivalence_workflow_v1_61.R
#
# Main changes in this app version:
#   - Short app-folder name to reduce Windows path-length problems.
#   - Run outputs are written inside GDE_v023/runs rather than in a deeply
#     nested bundle folder or tempdir().
#   - Candidate-method diagnostic plots use side-by-side raw vs corrected views.
#   - Candidate-method side-by-side plot selection uses plot_manifest.csv.
#   - Candidate-method side-by-side plots use coherent axes across stages.
#  - v0.23: side-by-side difference-vs-reference and time-series plots use
#    the same outlier-category colours as the single diagnostic plots:
#    OK = darkgreen, Low (>2 µg/m³) = orange, High (>25% RM) = red.
#   - Informative colours are restored for diagnostic categories and stages.
#   - The non-existing correction-coefficients plot box is removed.
#   - Daily and annual primary plots are selected using their actual filenames.
#   - Tables are compact, rounded, centred and status-coloured.
#
# Notes:
#   - Extract this folder directly to a short path such as:
#       C:/Users/claud/Downloads/GDE_v023
#   - Launch with:
#       shiny::runApp("C:/Users/claud/Downloads/GDE_v023")
#
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# ---- Locate and source workflow engine ---------------------------------------

find_app_dir <- function() {
  possible_dirs <- unique(c(
    getwd(),
    file.path(getwd(), "GDE_v023"),
    file.path(getwd(), "GDE_v023"),
    file.path(getwd(), "gde_equivalence_shiny_v0_4")
  ))

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    active_path <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(active_path)) possible_dirs <- unique(c(dirname(active_path), possible_dirs))
  }

  frames <- sys.frames()
  ofiles <- unlist(lapply(frames, function(fr) {
    if (!is.null(fr$ofile)) fr$ofile else character(0)
  }))
  if (length(ofiles) > 0) {
    possible_dirs <- unique(c(dirname(normalizePath(ofiles, winslash = "/", mustWork = FALSE)), possible_dirs))
  }

  for (d in possible_dirs) {
    wf <- file.path(d, "equivalence_workflow_v1_61.R")
    if (file.exists(wf)) return(normalizePath(d, winslash = "/", mustWork = TRUE))
  }

  stop(
    "Workflow file not found. Please make sure app.R, equivalence_workflow_v1_61.R, ",
    "equivalence_report_template_v1_61.qmd and equivalence_config_v1_61.yml are in the same folder, ",
    "then run shiny::runApp('.') from that folder."
  )
}

app_dir <- find_app_dir()
workflow_file <- file.path(app_dir, "equivalence_workflow_v1_61.R")
source(workflow_file)

# ---- Helpers -----------------------------------------------------------------

safe_file_name <- function(x) {
  x <- basename(x)
  x <- gsub("[^A-Za-z0-9_.-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

filter_values <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  out <- trimws(as.character(x))
  out <- out[nzchar(out)]
  if (length(out) == 0) NULL else out
}

filter_uploaded_dataset_for_run <- function(input_path, output_path,
                                            country_filter = NULL,
                                            instrument_filter = NULL,
                                            size_filter = NULL,
                                            campaign_filter = NULL) {
  raw <- read_equivalence_input(input_path)
  std <- standardise_cm_data(raw)

  n_before <- nrow(std)

  if (!is.null(country_filter) && "Country" %in% names(std)) {
    std <- std %>% dplyr::filter(.data$Country %in% country_filter)
  }
  if (!is.null(instrument_filter) && "Instrument" %in% names(std)) {
    std <- std %>% dplyr::filter(.data$Instrument %in% instrument_filter)
  }
  if (!is.null(size_filter) && "Size" %in% names(std)) {
    std <- std %>% dplyr::filter(.data$Size %in% size_filter)
  }
  if (!is.null(campaign_filter) && "Campaign" %in% names(std)) {
    std <- std %>% dplyr::filter(.data$Campaign %in% campaign_filter)
  }

  n_after <- nrow(std)

  if (n_after == 0) {
    stop(
      "The selected filters return zero rows. ",
      "Check Country, Instrument, Pollutant and Site/Campaign selections."
    )
  }

  readr::write_csv(std, output_path)

  list(
    filtered_input = output_path,
    n_before = n_before,
    n_after = n_after,
    country_filter = country_filter %||% "ALL",
    instrument_filter = instrument_filter %||% "ALL",
    size_filter = size_filter %||% "ALL",
    campaign_filter = campaign_filter %||% "ALL"
  )
}

read_csv_safe_app <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) data.frame(error = conditionMessage(e))
  )
}

friendly_error <- function(msg) {
  msg <- as.character(msg)
  dplyr::case_when(
    grepl("Workflow file not found", msg, ignore.case = TRUE) ~
      paste0("The workflow engine file was not found. Check that app.R and equivalence_workflow_v1_61.R are in the same folder. Details: ", msg),
    grepl("Report template not found", msg, ignore.case = TRUE) ~
      paste0("The Quarto report template was not found. Check that equivalence_report_template_v1_61.qmd is in the app folder. Details: ", msg),
    grepl("Quarto completed but the HTML report was not created", msg, ignore.case = TRUE) ~
      paste0("The workflow completed, but Quarto failed while rendering the HTML report. Check the Run log for the Quarto error, or rerun with 'Render HTML report' unchecked. Details: ", msg),
    grepl("Missing required column", msg, ignore.case = TRUE) ~
      paste0("The input file is missing one or more required columns. Check the expected column names for the selected scenario. Details: ", msg),
    grepl("YAML|yaml", msg, ignore.case = TRUE) ~
      paste0("The YAML configuration could not be read. Check indentation and parameter names. Details: ", msg),
    TRUE ~ msg
  )
}

clean_status_text <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("WARNING_LOW_N|LOW_N|LOW N", x, ignore.case = TRUE) ~ "LOW N",
    grepl("NOT_ASSESSABLE|NOT ASSESSABLE", x, ignore.case = TRUE) ~ "NOT ASSESSABLE",
    grepl("NO_DATA|NO DATA", x, ignore.case = TRUE) ~ "NO DATA",
    grepl("PASS", x, ignore.case = TRUE) ~ "PASS",
    grepl("FAIL", x, ignore.case = TRUE) ~ "FAIL",
    TRUE ~ gsub("_", " ", x)
  )
}

make_run_zip <- function(output_dir) {
  zip_path <- file.path(output_dir, "assessment_output_bundle.zip")
  if (file.exists(zip_path)) unlink(zip_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)

  files <- list.files(".", recursive = TRUE, all.files = FALSE, include.dirs = FALSE)
  files <- files[files != basename(zip_path)]
  utils::zip(zipfile = zip_path, files = files)
  zip_path
}

build_config_text <- function(input, auxiliary_file_name = NULL) {
  aux_value <- if (is.null(auxiliary_file_name)) "null" else paste0('"', auxiliary_file_name, '"')

  lines <- c(
    "# Configuration generated by the Shiny dashboard prototype",
    "",
    "correction:",
    paste0("  enabled: ", ifelse(isTRUE(input$correction_enabled), "true", "false")),
    paste0("  model: \"", input$correction_model, "\""),
    "  fit_range:",
    paste0("    lower_rm: ", input$fit_rm_min),
    paste0("    upper_factor_daily_lv: ", input$fit_upper_factor),
    "",
    "daily_assessment:",
    paste0("  decision_metric: \"", input$daily_metric, "\""),
    paste0("  lv_window_lower_factor: ", input$daily_lv_lower),
    paste0("  lv_window_upper_factor: ", input$daily_lv_upper),
    paste0("  min_n_ok: ", input$daily_min_ok),
    paste0("  min_n_low_n: ", input$daily_min_low_n),
    "",
    "annual_assessment:",
    paste0("  min_consecutive_pairs_for_V: ", input$min_pairs_v),
    "  use_effective_sample_size: true",
    paste0("  auxiliary_autocorrelation_allowed: ", ifelse(isTRUE(input$aux_allowed), "true", "false")),
    paste0("  auxiliary_autocorrelation_file: ", aux_value),
    paste0("  auxiliary_min_consecutive_pairs_for_V: ", input$aux_min_pairs_v),
    "",
    "diagnostics:",
    paste0("  mbe_threshold: ", input$diag_mbe),
    paste0("  sd_diff_threshold: ", input$diag_sd),
    paste0("  skewness_threshold: ", input$diag_skew),
    paste0("  kurtosis_threshold: ", input$diag_kurt),
    paste0("  ubscm_threshold: ", input$diag_ubscm),
    "",
    "linearity:",
    paste0("  r2_threshold_type_testing: ", input$r2_type_testing),
    paste0("  r2_threshold_ongoing: ", input$r2_ongoing),
    "",
    "rm_screening:",
    paste0("  duplicate_abs_threshold: ", input$rm_duplicate_threshold),
    paste0("  fail_pct_threshold: ", input$rm_warning_threshold),
    "",
    "limit_values:",
    "  PM10:",
    paste0("    daily: ", input$lv_pm10_daily),
    paste0("    annual: ", input$lv_pm10_annual),
    "  PM2.5:",
    paste0("    daily: ", input$lv_pm25_daily),
    paste0("    annual: ", input$lv_pm25_annual),
    "",
    "dqo:",
    "  PM10:",
    paste0("    daily: ", input$dqo_pm10_daily),
    paste0("    annual: ", input$dqo_pm10_annual),
    "  PM2.5:",
    paste0("    daily: ", input$dqo_pm25_daily),
    paste0("    annual: ", input$dqo_pm25_annual),
    "",
    "vrep:",
    paste0("  PM10: ", input$vrep_pm10),
    paste0("  PM2.5: ", input$vrep_pm25),
    ""
  )

  paste(lines, collapse = "\n")
}

write_generated_config <- function(path, input, auxiliary_file_name = NULL) {
  writeLines(build_config_text(input, auxiliary_file_name), path, useBytes = TRUE)
  invisible(path)
}

default_editable_config_text <- function() {
  paste(c(
    "# Editable YAML configuration",
    "",
    "correction:",
    "  enabled: true",
    "  model: \"TLS\"",
    "  fit_range:",
    "    lower_rm: 0",
    "    upper_factor_daily_lv: 1.5",
    "",
    "daily_assessment:",
    "  decision_metric: \"CI95\"",
    "  lv_window_lower_factor: 0.7",
    "  lv_window_upper_factor: 1.3",
    "  min_n_ok: 15",
    "  min_n_low_n: 10",
    "",
    "annual_assessment:",
    "  min_consecutive_pairs_for_V: 40",
    "  use_effective_sample_size: true",
    "  auxiliary_autocorrelation_allowed: true",
    "  auxiliary_autocorrelation_file: null",
    "  auxiliary_min_consecutive_pairs_for_V: 40",
    "",
    "diagnostics:",
    "  mbe_threshold: 1.0",
    "  sd_diff_threshold: 2.5",
    "  skewness_threshold: 0.5",
    "  kurtosis_threshold: 4.0",
    "  ubscm_threshold: 2.5",
    "",
    "linearity:",
    "  r2_threshold_type_testing: 0.95",
    "  r2_threshold_ongoing: 0.90",
    "",
    "rm_screening:",
    "  duplicate_abs_threshold: 2.0",
    "  fail_pct_threshold: 5.0",
    "",
    "limit_values:",
    "  PM10:",
    "    daily: 45",
    "    annual: 20",
    "  PM2.5:",
    "    daily: 25",
    "    annual: 10",
    "",
    "dqo:",
    "  PM10:",
    "    daily: 25",
    "    annual: 20",
    "  PM2.5:",
    "    daily: 25",
    "    annual: 30",
    "",
    "vrep:",
    "  PM10: 4.04",
    "  PM2.5: 5.46",
    ""
  ), collapse = "\n")
}

status_color <- function(x) {
  x <- paste(clean_status_text(x), collapse = " ")
  if (grepl("FAIL", x, ignore.case = TRUE)) return("red")
  if (grepl("WARNING|LOW|NOT|NO DATA", x, ignore.case = TRUE)) return("yellow")
  if (grepl("PASS", x, ignore.case = TRUE)) return("green")
  "blue"
}

rename_for_app <- function(df) {
  rename_map <- c(
    "Size" = "Pollutant",
    "CM_type" = "CM",
    "Campaign" = "Site",
    "daily_method_status" = "Result d",
    "annual_method_status" = "Result y",
    "final_method_status" = "Final",
    "main_issue" = "Main issue",
    "n_site_fails" = "n site fails",
    "daily_status" = "Result d",
    "daily_site_status" = "Result d",
    "annual_status" = "Result y",
    "annual_site_status" = "Result y",
    "daily_assessment_status" = "Result d",
    "annual_assessment_status" = "Result y",
    "u_daily_bias" = "Bias d",
    "u_annual_bias" = "Bias y",
    "u_annual_random" = "Random y",
    "scorr" = "Random",
    "SD_diff" = "SD(diff)",
    "SD_res" = "SD res",
    "R2" = "R²",
    "R2_threshold" = "R² min",
    "n_LV" = "n LV",
    "n_annual" = "n annual",
    "daily_metric_rel_LV_pct" = "Metric d (%LVd)",
    "daily_decision_metric" = "Metric d",
    "u_daily_rel_LV_pct" = "Udaily (%LVd)",
    "u_annual_rel_LV_pct" = "Uann (%LVy)",
    "n_RM_duplicates" = "n RM pairs",
    "n_rm_excluded" = "n above RM threshold",
    "n_RM_excluded" = "n above RM threshold",
    "pct_RM_excluded" = "% above RM threshold",
    "pct_excluded_duplicate_denominator" = "% above RM threshold",
    "RM_abs_diff_mean" = "Mean |RM1−RM2|",
    "RM_diff_SD" = "SD(RM1−RM2)",
    "n_observations" = "n observations",
    "n_dates" = "n days",
    "n_pairs" = "n pairs",
    "V_used" = "V used",
    "Neff_input" = "Neff contribution"
  )

  for (old in names(rename_map)) {
    if (old %in% names(df) && !(rename_map[[old]] %in% names(df))) {
      names(df)[names(df) == old] <- rename_map[[old]]
    }
  }
  df
}

drop_constant_id_cols <- function(df) {
  for (col in c("Instrument", "Pollutant", "CM")) {
    if (col %in% names(df) && length(unique(stats::na.omit(df[[col]]))) <= 1) {
      df[[col]] <- NULL
    }
  }
  df
}

select_compact_cols <- function(df, table_type = "generic") {
  keep <- switch(
    table_type,
    summary = c("Instrument", "Pollutant", "Stage", "Result d", "Result y", "n site fails", "Final", "Main issue"),
    final_site = c("Instrument", "Pollutant", "CM", "Site", "Result d", "Result y", "Final", "Main issue",
                   "n LV", "Metric d (%LVd)", "n annual", "Uann (%LVy)"),
    daily_unit = c("Instrument", "Pollutant", "CM", "Stage", "n LV", "Metric d", "Metric d (%LVd)",
                   "Bias d", "Random", "SD(diff)", "Result d"),
    daily_site = c("Instrument", "Pollutant", "CM", "Site", "Stage", "n LV", "Metric d", "Metric d (%LVd)",
                   "Bias d", "Random", "SD(diff)", "Result d"),
    annual_unit = c("Instrument", "Pollutant", "CM", "Stage", "n annual", "Random y", "Bias y", "Uann (%LVy)", "Result y"),
    annual_site = c("Instrument", "Pollutant", "CM", "Site", "Stage", "n annual", "Random y", "Bias y", "Uann (%LVy)", "Result y"),
    rm_screen = c("n_rows_total", "n_rows_with_rm_duplicates", "n above RM threshold", "% above RM threshold", "rm_screen_flag"),
    rm_site = c("Instrument", "Pollutant", "Site", "n RM pairs", "Mean |RM1−RM2|", "SD(RM1−RM2)",
                "n above RM threshold", "% above RM threshold"),
    linearity = c("Instrument", "Pollutant", "CM", "Stage", "correction_model", "n_regression", "R²", "R² min",
                  "slope", "intercept", "linearity_status"),
    corrections = c("Instrument", "Pollutant", "CM", "Stage", "correction_model", "slope", "intercept", "n_fit"),
    neff = c("Instrument", "Pollutant", "Site", "V_source", "n observations", "n days", "n pairs", "rho1", "Vrep", "V used", "Neff contribution"),
    NULL
  )

  if (!is.null(keep)) {
    hit <- intersect(keep, names(df))
    if (length(hit) > 0) df <- df[, hit, drop = FALSE]
  }
  df
}

format_table_for_app <- function(df, digits = 2, table_type = "generic") {
  if (is.null(df) || nrow(df) == 0) return(df)

  df <- rename_for_app(df)
  df <- select_compact_cols(df, table_type = table_type)
  df <- drop_constant_id_cols(df)

  status_cols <- grep("status|result|final|flag|pass|Result|Final", names(df), ignore.case = TRUE, value = TRUE)
  if (length(status_cols) > 0) {
    df <- df %>% mutate(across(all_of(status_cols), clean_status_text))
  }

  count_pattern <- paste(
    c("^n$", "^n ", "^n_", "_n$", "count", "pairs", "rows", "dates", "days",
      "observations", "units", "G$", "df$", "site fails", "fit$", "annual$", "LV$"),
    collapse = "|"
  )

  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      if (grepl(count_pattern, nm, ignore.case = TRUE)) {
        df[[nm]] <- ifelse(is.na(df[[nm]]), NA, as.integer(round(df[[nm]], 0)))
      } else {
        df[[nm]] <- round(df[[nm]], digits)
      }
    }
  }

  df
}

small_table <- function(df, table_type = "generic") {
  df <- format_table_for_app(df, digits = 2, table_type = table_type)

  status_cols <- grep("Result|Final|status|flag", names(df), ignore.case = TRUE, value = TRUE)

  dt <- DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    escape = FALSE,
    options = list(
      pageLength = 12,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = "tip",
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      ),
      headerCallback = DT::JS(
        "function(thead, data, start, end, display){",
        "  $(thead).find('th').css({'text-align':'center', 'vertical-align':'middle'});",
        "}"
      ),
      initComplete = DT::JS(
        "function(settings, json){",
        "  var api = this.api();",
        "  setTimeout(function(){",
        "    api.columns.adjust();",
        "    $(api.table().header()).find('th').css({'text-align':'center', 'vertical-align':'middle'});",
        "    $(api.table().body()).find('td').css({'text-align':'center', 'vertical-align':'middle'});",
        "  }, 150);",
        "}"
      ),
      drawCallback = DT::JS(
        "function(settings){",
        "  var api = this.api();",
        "  api.columns.adjust();",
        "  $(api.table().header()).find('th').css({'text-align':'center', 'vertical-align':'middle'});",
        "  $(api.table().body()).find('td').css({'text-align':'center', 'vertical-align':'middle'});",
        "}"
      )
    ),
    class = "cell-border stripe compact nowrap"
  )

  if (length(status_cols) > 0) {
    for (sc in status_cols) {
      dt <- DT::formatStyle(
        dt, sc,
        color = DT::styleEqual(
          c("PASS", "FAIL", "LOW N", "WARNING", "NOT ASSESSABLE", "NO DATA"),
          c("#008000", "#C00000", "#C08000", "#C08000", "#C08000", "#C08000")
        ),
        fontWeight = "bold"
      )
    }
  }

  dt
}

plot_slot <- function(output_id, title, width = 6) {
  box(
    title = title,
    status = "info",
    solidHeader = TRUE,
    width = width,
    imageOutput(output_id, height = "auto")
  )
}


# ---- App-generated side-by-side diagnostic plots ------------------------------

safe_range <- function(x, default = c(0, 1), symmetric = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(default)

  if (symmetric) {
    m <- max(abs(x), na.rm = TRUE)
    if (!is.finite(m) || m <= 0) m <- 1
    return(c(-m, m))
  }

  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) == 0) {
    m <- mean(x, na.rm = TRUE)
    if (!is.finite(m)) m <- 0
    return(m + c(-1, 1))
  }
  r
}

safe_date_from_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(suppressWarnings(as.Date(x, origin = "1899-12-30")))

  out <- tryCatch(suppressWarnings(as.Date(x)), error = function(e) rep(as.Date(NA), length(x)))
  if (!all(is.na(out))) return(out)

  out <- tryCatch(suppressWarnings(as.Date(x, format = "%d/%m/%Y")), error = function(e) rep(as.Date(NA), length(x)))
  out
}

save_side_by_side_plot <- function(plot_obj, path, width = 14, height = 8) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename = path, plot = plot_obj, width = width, height = height, dpi = 150)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

manifest_row <- function(output_dir, plot_id, plot_type, title, path, axes) {
  output_norm <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  data.frame(
    plot_id = plot_id,
    section = "candidate_diagnostics",
    plot_type = plot_type,
    display_mode = "side_by_side",
    stage = "No correction vs After correction",
    title = title,
    path = path_norm,
    relative_path = gsub(paste0("^", output_norm, "/?"), "", path_norm),
    axes = axes,
    source = "long_stage_data.csv",
    stringsAsFactors = FALSE
  )
}

candidate_plot_colours <- c(
  "OK" = "darkgreen",
  "Low (>2 µg/m³)" = "orange",
  "High (>25% RM)" = "red",
  "Outside fit range" = "grey40",
  "No correction" = "blue",
  "After correction" = "forestgreen"
)

create_candidate_side_by_side_manifest <- function(output_dir) {
  stage_file <- file.path(output_dir, "01_clean_data", "long_stage_data.csv")
  plot_dir <- file.path(output_dir, "04_plots", "candidate_side_by_side")
  manifest_file <- file.path(output_dir, "04_plots", "plot_manifest.csv")
  log_file <- file.path(output_dir, "04_plots", "plot_manifest_generation_log.txt")

  dir.create(dirname(manifest_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  log_lines <- c(
    paste("Starting candidate side-by-side manifest generation at", Sys.time()),
    paste("Output directory:", normalizePath(output_dir, winslash = "/", mustWork = FALSE)),
    paste("Stage file:", normalizePath(stage_file, winslash = "/", mustWork = FALSE))
  )

  append_log <- function(...) {
    log_lines <<- c(log_lines, paste(..., collapse = " "))
    tryCatch(
      writeLines(log_lines, log_file, useBytes = TRUE),
      error = function(e) {
        message("Could not write plot manifest log: ", conditionMessage(e))
      }
    )
  }

  if (!file.exists(stage_file)) {
    append_log("Missing long_stage_data.csv.")
    return(data.frame())
  }

  d <- tryCatch(
    readr::read_csv(stage_file, show_col_types = FALSE),
    error = function(e) {
      append_log("Could not read long_stage_data.csv:", conditionMessage(e))
      data.frame()
    }
  )

  if (!is.data.frame(d) || nrow(d) == 0) {
    append_log("Stage data are empty.")
    return(data.frame())
  }

  append_log("Columns available:", paste(names(d), collapse = ", "))

  # Tolerate minor naming variations.
  if (!"CM_value" %in% names(d) && "CM_raw" %in% names(d)) d$CM_value <- d$CM_raw
  if (!"CM_type" %in% names(d) && "CM" %in% names(d)) d$CM_type <- d$CM
  if (!"RM_AVG" %in% names(d) && "RM1" %in% names(d)) d$RM_AVG <- d$RM1
  if (!"Size" %in% names(d) && "Pollutant" %in% names(d)) d$Size <- d$Pollutant
  if (!"Stage" %in% names(d)) d$Stage <- "No correction"
  if (!"Site" %in% names(d)) {
    if ("Campaign" %in% names(d)) d$Site <- d$Campaign else d$Site <- "Site"
  }
  if (!"date" %in% names(d)) d$date <- NA

  required_cols <- c("Stage", "Size", "CM_type", "RM_AVG", "CM_value")
  missing_cols <- setdiff(required_cols, names(d))
  if (length(missing_cols) > 0) {
    append_log("Missing required columns:", paste(missing_cols, collapse = ", "))
    return(data.frame())
  }

  d <- d %>%
    dplyr::mutate(
      Stage = as.character(Stage),
      Stage = dplyr::case_when(
        grepl("after", Stage, ignore.case = TRUE) ~ "After correction",
        grepl("corrected", Stage, ignore.case = TRUE) ~ "After correction",
        TRUE ~ "No correction"
      ),
      RM_AVG = suppressWarnings(as.numeric(RM_AVG)),
      CM_value = suppressWarnings(as.numeric(CM_value)),
      diff = CM_value - RM_AVG,
      abs_diff = abs(diff),
      date = safe_date_from_any(date),
      Site = as.character(Site),
      Size = as.character(Size),
      CM_type = as.character(CM_type),
      facet_row = paste(Size, CM_type, sep = " | "),
      outside_fit_range = FALSE,
      outlier_cm = dplyr::case_when(
        abs_diff > 2 & abs_diff > 0.25 * abs(RM_AVG) ~ "High (>25% RM)",
        abs_diff > 2 ~ "Low (>2 µg/m³)",
        TRUE ~ "OK"
      )
    ) %>%
    dplyr::filter(is.finite(RM_AVG), is.finite(CM_value), is.finite(diff))

  append_log("Rows after finite CM/RM filtering:", nrow(d))
  append_log("Stages available:", paste(unique(as.character(d$Stage)), collapse = ", "))

  if (nrow(d) == 0) {
    append_log("No finite paired data available.")
    return(data.frame())
  }

  d$Stage <- factor(d$Stage, levels = intersect(c("No correction", "After correction"), unique(as.character(d$Stage))))

  x_rng <- safe_range(d$RM_AVG)
  diff_rng <- safe_range(d$diff, symmetric = TRUE)
  date_rng <- if (any(!is.na(d$date))) range(d$date, na.rm = TRUE) else as.Date(c(NA, NA))

  n_facet_rows <- length(unique(d$facet_row))
  n_sites <- length(unique(d$Site))
  base_height <- max(6, min(14, 3.2 + 1.2 * n_facet_rows))

  d <- d %>%
    dplyr::arrange(RM_AVG) %>%
    dplyr::mutate(
      threshold_pos = ifelse(RM_AVG <= 8, 2, 0.25 * RM_AVG),
      threshold_neg = -threshold_pos
    )

  manifest <- list()

  make_plot <- function(plot_obj, plot_id, plot_type, title, filename, axes, width = 14, height = base_height) {
    tryCatch({
      f <- file.path(plot_dir, filename)
      save_side_by_side_plot(plot_obj, f, width = width, height = height)
      manifest[[length(manifest) + 1]] <<- manifest_row(output_dir, plot_id, plot_type, title, f, axes)
      append_log("Generated:", filename)
    }, error = function(e) {
      append_log("Failed:", filename, "-", conditionMessage(e))
    })
  }

  p1 <- ggplot2::ggplot(d, ggplot2::aes(x = RM_AVG, y = diff, color = outlier_cm, shape = Stage)) +
    ggplot2::geom_point(alpha = 0.70, size = 1.6) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = threshold_pos), color = "grey40", linetype = "dotted") +
    ggplot2::geom_line(ggplot2::aes(y = threshold_neg), color = "grey40", linetype = "dotted") +
    ggplot2::coord_cartesian(xlim = x_rng, ylim = diff_rng) +
    ggplot2::facet_grid(facet_row ~ Stage) +
    ggplot2::scale_color_manual(values = candidate_plot_colours, drop = FALSE) +
    ggplot2::labs(
      title = "Candidate method difference vs reference concentration",
      subtitle = "Side-by-side no correction versus after correction; identical axes are used across stages.",
      x = "Reference method average (µg/m³)",
      y = "Candidate method - reference method (µg/m³)",
      color = "Point category",
      shape = "Stage"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10), plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
  make_plot(p1, "cm_diff_vs_reference_sbs", "difference_vs_reference", "Candidate method difference vs reference concentration", "cm_diff_vs_reference_side_by_side.png", "Same x and y limits across stages")

  p2 <- ggplot2::ggplot(d, ggplot2::aes(x = diff, fill = Stage)) +
    ggplot2::geom_histogram(bins = 30, color = "black", alpha = 0.70) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::coord_cartesian(xlim = diff_rng) +
    ggplot2::facet_grid(facet_row ~ Stage, scales = "free_y") +
    ggplot2::scale_fill_manual(values = candidate_plot_colours, drop = FALSE) +
    ggplot2::labs(
      title = "Distribution of candidate method differences",
      subtitle = "Side-by-side no correction versus after correction; identical x-axis is used across stages.",
      x = "Candidate method - reference method (µg/m³)",
      y = "Count",
      fill = "Stage"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10), plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
  make_plot(p2, "cm_difference_histogram_sbs", "difference_distribution", "Distribution of candidate method differences", "cm_difference_histogram_side_by_side.png", "Same x-axis across stages; y-axis free by facet row")

  d_time <- d %>% dplyr::filter(!is.na(date))
  if (nrow(d_time) > 0 && all(!is.na(date_rng))) {
    p3 <- ggplot2::ggplot(d_time, ggplot2::aes(x = date, y = diff, color = outlier_cm, shape = Stage)) +
      ggplot2::geom_point(alpha = 0.70, size = 1.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = c(-2, 2), color = "grey40", linetype = "dotted") +
      ggplot2::coord_cartesian(xlim = date_rng, ylim = diff_rng) +
      ggplot2::facet_grid(facet_row ~ Stage) +
      ggplot2::scale_color_manual(values = candidate_plot_colours, drop = FALSE) +
      ggplot2::labs(
        title = "Candidate method differences over time",
        subtitle = "Side-by-side no correction versus after correction; identical axes are used across stages.",
        x = "Date",
        y = "Candidate method - reference method (µg/m³)",
        color = "Point category",
        shape = "Stage"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 10), plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
    make_plot(p3, "cm_difference_time_series_sbs", "difference_time_series", "Candidate method differences over time", "cm_difference_time_series_side_by_side.png", "Same x and y limits across stages")
  } else {
    append_log("Time-series side-by-side plot skipped: no valid date column.")
  }

  d_site <- d %>%
    dplyr::group_by(Stage, Size, CM_type, Site) %>%
    dplyr::summarise(
      n = dplyr::n(),
      MBE = mean(diff, na.rm = TRUE),
      SD_diff = stats::sd(diff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      facet_row = paste(Size, CM_type, sep = " | "),
      Site_short = stringr::str_trunc(as.character(Site), width = 35)
    )

  if (nrow(d_site) > 0) {
    mbe_rng <- safe_range(d_site$MBE, symmetric = TRUE)
    site_height <- max(6, min(16, 3 + 0.35 * n_sites + 1.0 * n_facet_rows))
    p4 <- ggplot2::ggplot(d_site, ggplot2::aes(x = MBE, y = stats::reorder(Site_short, MBE), color = Stage)) +
      ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.90) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::coord_cartesian(xlim = mbe_rng) +
      ggplot2::facet_grid(facet_row ~ Stage, scales = "free_y", space = "free_y") +
      ggplot2::scale_color_manual(values = candidate_plot_colours, drop = FALSE) +
      ggplot2::labs(
        title = "Candidate method site/campaign diagnostic",
        subtitle = "Mean bias by site/campaign; side-by-side no correction versus after correction with identical x-axis.",
        x = "Mean candidate method - reference method (µg/m³)",
        y = "Site/Campaign",
        color = "Stage",
        size = "n"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 10), plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
    make_plot(p4, "cm_site_campaign_bias_sbs", "site_campaign_bias", "Candidate method site/campaign diagnostic", "cm_site_campaign_bias_side_by_side.png", "Same x-axis across stages; site/campaign ordering free by facet row", width = 14, height = site_height)
  }

  manifest_df <- dplyr::bind_rows(manifest)
  if (nrow(manifest_df) > 0) {
    readr::write_csv(manifest_df, manifest_file)
    append_log("Manifest written:", manifest_file)
  } else {
    append_log("No side-by-side plots could be generated.")
  }

  manifest_df
}

# ---- UI ----------------------------------------------------------------------

header <- dashboardHeader(
  title = "GDE equivalence assessment",
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "sidebar",
    menuItem("Data & run", tabName = "data_run", icon = icon("play-circle")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
    menuItem("Reference screening", tabName = "reference", icon = icon("balance-scale")),
    menuItem("Linearity & correction", tabName = "linearity", icon = icon("chart-line")),
    menuItem("Candidate method diagnostics", tabName = "diagnostics", icon = icon("chart-area")),
    menuItem("Daily LV", tabName = "daily", icon = icon("calendar-day")),
    menuItem("Annual", tabName = "annual", icon = icon("calendar")),
    menuItem("Plots", tabName = "plots", icon = icon("images")),
    menuItem("Report & downloads", tabName = "downloads", icon = icon("download")),
    menuItem("Run log", tabName = "log", icon = icon("terminal"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f8; }
      .box { border-radius: 6px; }
      .small-note { color: #555; font-size: 0.92em; }
      .run-ok { color: #007a3d; font-weight: 700; }
      .run-error { color: #b00020; font-weight: 700; }
      .main-header .logo { font-weight: 700; }
      .dataTables_wrapper { font-size: 0.92em; }
      .dataTables_wrapper table.dataTable,
      .dataTables_scrollHead table.dataTable,
      .dataTables_scrollBody table.dataTable {
        width: 100% !important;
        table-layout: auto !important;
      }
      table.dataTable thead th,
      table.dataTable thead td {
        text-align: center !important;
        vertical-align: middle !important;
        white-space: nowrap;
      }
      table.dataTable tbody td {
        text-align: center !important;
        vertical-align: middle !important;
        white-space: nowrap;
      }
      .dataTables_scrollHeadInner,
      .dataTables_scrollHeadInner table {
        width: 100% !important;
      }
      table.dataTable tbody td.dt-left,
      table.dataTable thead th.dt-left {
        text-align: left !important;
      }
      .shiny-table table,
      table.shiny-table,
      .table {
        text-align: center;
      }
      .shiny-table th,
      .shiny-table td,
      table.shiny-table th,
      table.shiny-table td,
      .table th,
      .table td {
        text-align: center !important;
        vertical-align: middle !important;
      }
      .dataTables_scrollHead table.dataTable thead th {
        text-align: center !important;
      }
      pre { white-space: pre-wrap; max-height: 480px; overflow-y: auto; }
    "))
  ),

  tabItems(
    tabItem(
      tabName = "data_run",
      fluidRow(
        box(
          title = "Input files and run controls",
          status = "primary",
          solidHeader = TRUE,
          width = 5,
          fileInput("dataset", "Main dataset", accept = c(".csv", ".txt", ".xlsx", ".xls", ".rds")),
          radioButtons(
            "config_mode", "Configuration source",
            choices = c(
              "Use UI settings" = "ui",
              "Edit YAML in app" = "edit",
              "Use uploaded YAML config" = "upload"
            ),
            selected = "ui"
          ),
          conditionalPanel(
            condition = "input.config_mode == 'upload'",
            fileInput("config_upload", "YAML config file", accept = c(".yml", ".yaml"))
          ),
          fileInput("aux_file", "Auxiliary autocorrelation file", accept = c(".csv", ".txt", ".xlsx", ".xls")),
          selectInput("scenario", "Scenario", choices = c("ongoing_verification", "type_testing"), selected = "ongoing_verification"),
          selectInput("run_mode", "Run mode", choices = c("full_workflow", "diagnostic_only", "uncertainty_only"), selected = "full_workflow"),
          checkboxInput("render_html", "Render HTML report", value = TRUE),
          checkboxInput("stop_on_rm_fail", "Stop if RM screening warning threshold is exceeded", value = FALSE),
          actionButton("run", "Run assessment", icon = icon("play"), class = "btn-primary")
        ),
        box(
          title = "Main settings",
          status = "info",
          solidHeader = TRUE,
          width = 4,
          selectInput("daily_metric", "Daily LV decision metric", choices = c("CI95", "Udaily", "Utot", "Udiff"), selected = "CI95"),
          checkboxInput("correction_enabled", "Enable correction stage", value = TRUE),
          selectInput("correction_model", "Correction model", choices = c("TLS", "OLS", "MEAN_RATIO", "NONE"), selected = "TLS"),
          selectizeInput(
            "country_filter", "Country",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "All countries")
          ),
          selectizeInput(
            "instrument_filter", "Instrument",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "All instruments")
          ),
          selectizeInput(
            "pollutant_filter", "Pollutant",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "All pollutants")
          ),
          selectizeInput(
            "site_filter", "Site/Campaign",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "All sites/campaigns")
          ),
          p(class = "small-note", "Dropdowns are populated after dataset upload. These selections define the subset used for the whole run. Empty selection means all values."),
          uiOutput("filter_choices_status")
        ),
        box(
          title = "Run status",
          status = "success",
          solidHeader = TRUE,
          width = 3,
          uiOutput("run_status")
        )
      ),
      fluidRow(
        box(
          title = "Subset selected for the next run",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          tableOutput("subset_preview")
        )
      ),
      fluidRow(
        box(
          title = "Advanced settings",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          fluidRow(
            column(
              3,
              h4("RM screening"),
              numericInput("rm_duplicate_threshold", "Duplicate-RM threshold, µg/m³", value = 2.0, min = 0, step = 0.1),
              numericInput("rm_warning_threshold", "Warning threshold, %", value = 5.0, min = 0, max = 100, step = 0.5),
              h4("Linearity"),
              numericInput("r2_type_testing", "R² threshold, type testing", value = 0.95, min = 0, max = 1, step = 0.01),
              numericInput("r2_ongoing", "R² threshold, ongoing verification", value = 0.90, min = 0, max = 1, step = 0.01)
            ),
            column(
              3,
              h4("Daily LV"),
              numericInput("daily_lv_lower", "Daily LV window lower factor", value = 0.7, min = 0, step = 0.05),
              numericInput("daily_lv_upper", "Daily LV window upper factor", value = 1.3, min = 0, step = 0.05),
              numericInput("daily_min_ok", "Daily min n OK", value = 15, min = 1, step = 1),
              numericInput("daily_min_low_n", "Daily min n LOW N", value = 10, min = 1, step = 1)
            ),
            column(
              3,
              h4("Correction / diagnostics"),
              numericInput("fit_rm_min", "Fit RM minimum", value = 0, step = 1),
              numericInput("fit_upper_factor", "Fit RM maximum, factor × daily LV", value = 1.5, min = 0.1, step = 0.1),
              numericInput("diag_mbe", "MBE threshold", value = 1.0, min = 0, step = 0.1),
              numericInput("diag_sd", "SD(diff) threshold", value = 2.5, min = 0, step = 0.1),
              numericInput("diag_skew", "Skewness threshold", value = 0.5, min = 0, step = 0.1),
              numericInput("diag_kurt", "Kurtosis threshold", value = 4.0, min = 0, step = 0.1),
              numericInput("diag_ubscm", "UbsCM threshold", value = 2.5, min = 0, step = 0.1)
            ),
            column(
              3,
              h4("Annual / LV / DQO"),
              numericInput("min_pairs_v", "Minimum consecutive pairs for V", value = 40, min = 1, step = 1),
              checkboxInput("aux_allowed", "Allow auxiliary autocorrelation file", value = TRUE),
              numericInput("aux_min_pairs_v", "Auxiliary min pairs for V", value = 40, min = 1, step = 1),
              numericInput("vrep_pm10", "Vrep PM10", value = 4.04, min = 1, step = 0.01),
              numericInput("vrep_pm25", "Vrep PM2.5", value = 5.46, min = 1, step = 0.01),
              numericInput("lv_pm10_daily", "PM10 daily LV", value = 45, min = 0, step = 1),
              numericInput("lv_pm10_annual", "PM10 annual LV", value = 20, min = 0, step = 1),
              numericInput("lv_pm25_daily", "PM2.5 daily LV", value = 25, min = 0, step = 1),
              numericInput("lv_pm25_annual", "PM2.5 annual LV", value = 10, min = 0, step = 1),
              numericInput("dqo_pm10_daily", "PM10 daily DQO, %", value = 25, min = 0, step = 1),
              numericInput("dqo_pm10_annual", "PM10 annual DQO, %", value = 20, min = 0, step = 1),
              numericInput("dqo_pm25_daily", "PM2.5 daily DQO, %", value = 25, min = 0, step = 1),
              numericInput("dqo_pm25_annual", "PM2.5 annual DQO, %", value = 30, min = 0, step = 1)
            )
          )
        )
      ),
      fluidRow(
        box(
          title = "Editable YAML configuration",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          conditionalPanel(
            condition = "input.config_mode == 'edit'",
            textAreaInput(
              "config_text",
              "Editable YAML configuration",
              value = default_editable_config_text(),
              rows = 18,
              width = "100%",
              resize = "vertical"
            ),
            actionButton("refresh_config_from_ui", "Refresh YAML from UI settings"),
            br(), br(),
            downloadButton("download_current_config", "Download current YAML")
          ),
          conditionalPanel(
            condition = "input.config_mode != 'edit'",
            verbatimTextOutput("config_preview")
          )
        )
      )
    ),

    tabItem(
      tabName = "dashboard",
      fluidRow(
        valueBoxOutput("run_box", width = 3),
        valueBoxOutput("daily_box", width = 3),
        valueBoxOutput("annual_box", width = 3),
        valueBoxOutput("final_box", width = 3)
      ),
      fluidRow(
        box(title = "Executive summary", status = "primary", solidHeader = TRUE, width = 12, DTOutput("summary_table"))
      ),
      fluidRow(
        box(title = "Final combined site assessment", status = "primary", solidHeader = TRUE, width = 12, DTOutput("final_site_table"))
      )
    ),

    tabItem(
      tabName = "reference",
      fluidRow(
        box(title = "RM duplicate screening summary", status = "primary", solidHeader = TRUE, width = 6, DTOutput("rm_screen_table")),
        box(title = "RM duplicate agreement by site", status = "primary", solidHeader = TRUE, width = 6, DTOutput("rm_site_table"))
      ),
      fluidRow(
        plot_slot("plot_rm_scatter", "RM duplicate agreement: RM1 vs RM2"),
        plot_slot("plot_rm_bland", "RM duplicate difference vs RM average")
      ),
      fluidRow(
        plot_slot("plot_rm_histogram", "Distribution of RM duplicate differences"),
        plot_slot("plot_rm_timeseries", "RM duplicate differences over time")
      )
    ),

    tabItem(
      tabName = "linearity",
      fluidRow(
        box(title = "Linearity diagnostics", status = "primary", solidHeader = TRUE, width = 7, DTOutput("linearity_table")),
        box(title = "Correction coefficients", status = "primary", solidHeader = TRUE, width = 5, DTOutput("correction_table"))
      ),
      fluidRow(
        plot_slot("plot_linearity", "Linearity and correction fit", width = 12)
      )
    ),

    tabItem(
      tabName = "diagnostics",
      fluidRow(
        box(title = "Candidate method difference diagnostics", status = "primary", solidHeader = TRUE, width = 6, DTOutput("diff_table")),
        box(title = "Candidate method diagnostics by site", status = "primary", solidHeader = TRUE, width = 6, DTOutput("diff_site_table"))
      ),
      fluidRow(
        plot_slot("plot_cm_bland", "Candidate method difference vs reference concentration: raw vs corrected"),
        plot_slot("plot_cm_histogram", "Distribution of candidate method differences: raw vs corrected")
      ),
      fluidRow(
        plot_slot("plot_cm_timeseries", "Candidate method differences over time: raw vs corrected"),
        plot_slot("plot_cm_site_diagnostics", "Candidate method site/campaign diagnostics: raw vs corrected")
      )
    ),

    tabItem(
      tabName = "daily",
      fluidRow(
        box(title = "Daily LV assessment", status = "primary", solidHeader = TRUE, width = 6, DTOutput("daily_unit_table")),
        box(title = "Daily site/campaign LV assessment", status = "primary", solidHeader = TRUE, width = 6, DTOutput("daily_site_table"))
      ),
      fluidRow(
        plot_slot("plot_daily_unit", "Daily LV assessment by candidate unit"),
        plot_slot("plot_daily_site", "Daily LV assessment by site/campaign")
      )
    ),

    tabItem(
      tabName = "annual",
      fluidRow(
        box(title = "Annual LV assessment", status = "primary", solidHeader = TRUE, width = 6, DTOutput("annual_unit_table")),
        box(title = "Annual site/campaign assessment", status = "primary", solidHeader = TRUE, width = 6, DTOutput("annual_site_table"))
      ),
      fluidRow(
        box(title = "Annual autocorrelation and Neff inputs", status = "primary", solidHeader = TRUE, width = 12, DTOutput("neff_table"))
      ),
      fluidRow(
        plot_slot("plot_annual_unit", "Annual uncertainty assessment by candidate unit"),
        plot_slot("plot_annual_site", "Annual site/campaign uncertainty assessment")
      )
    ),

    tabItem(
      tabName = "plots",
      fluidRow(
        box(title = "Plot manifest", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("plot_manifest_table"))
      ),
      fluidRow(
        box(title = "All generated plots", status = "primary", solidHeader = TRUE, width = 12,
            uiOutput("plot_selector"), imageOutput("selected_plot", height = "auto"))
      )
    ),

    tabItem(
      tabName = "downloads",
      fluidRow(
        box(
          title = "Download outputs",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          p("Download the full workflow output bundle or the rendered HTML report."),
          downloadButton("download_zip", "Download full output ZIP"),
          br(), br(),
          downloadButton("download_html", "Download HTML report"),
          br(), br(),
          downloadButton("download_excel", "Download Excel workbook"),
          br(), br(),
          downloadButton("download_current_config_2", "Download current YAML config")
        ),
        box(
          title = "Configuration used",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          DTOutput("config_table")
        )
      )
    ),

    tabItem(
      tabName = "log",
      fluidRow(
        box(title = "Run log", status = "primary", solidHeader = TRUE, width = 12, verbatimTextOutput("run_log"))
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    run_complete = FALSE,
    run_dir = NULL,
    output_dir = NULL,
    zip_path = NULL,
    html_path = NULL,
    xlsx_path = NULL,
    subset_info = NULL,
    log = character(),
    error = NULL
  )

  observeEvent(input$refresh_config_from_ui, {
    updateTextAreaInput(session, "config_text", value = build_config_text(input, auxiliary_file_name = NULL))
  })

  config_download_handler <- function(file) {
    if (identical(input$config_mode, "edit")) {
      writeLines(input$config_text %||% default_editable_config_text(), file, useBytes = TRUE)
    } else {
      write_generated_config(file, input, auxiliary_file_name = NULL)
    }
  }

  output$download_current_config <- downloadHandler(
    filename = function() paste0("gde_equivalence_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yml"),
    content = config_download_handler
  )

  output$download_current_config_2 <- downloadHandler(
    filename = function() paste0("gde_equivalence_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yml"),
    content = config_download_handler
  )

  output$config_preview <- renderText({
    if (identical(input$config_mode, "edit")) return(input$config_text %||% default_editable_config_text())
    if (identical(input$config_mode, "upload")) return("Uploaded YAML config will be used. The applied configuration is shown after the run.")
    build_config_text(input, auxiliary_file_name = if (!is.null(input$aux_file)) safe_file_name(input$aux_file$name) else NULL)
  })

  dataset_filter_choices <- reactive({
    req(input$dataset)

    tryCatch({
      raw <- read_equivalence_input(input$dataset$datapath)
      std <- standardise_cm_data(raw)

      country_choices <- if ("Country" %in% names(std)) sort(unique(stats::na.omit(as.character(std$Country)))) else character(0)
      instrument_choices <- if ("Instrument" %in% names(std)) sort(unique(stats::na.omit(as.character(std$Instrument)))) else character(0)
      pollutant_choices <- if ("Size" %in% names(std)) sort(unique(stats::na.omit(as.character(std$Size)))) else character(0)
      site_choices <- if ("Campaign" %in% names(std)) sort(unique(stats::na.omit(as.character(std$Campaign)))) else character(0)

      list(
        country = country_choices,
        instrument = instrument_choices,
        pollutant = pollutant_choices,
        site = site_choices,
        error = NULL
      )
    }, error = function(e) {
      list(
        country = character(0),
        instrument = character(0),
        pollutant = character(0),
        site = character(0),
        error = conditionMessage(e)
      )
    })
  })

  observeEvent(dataset_filter_choices(), {
    ch <- dataset_filter_choices()

    updateSelectizeInput(session, "country_filter", choices = ch$country, selected = character(0), server = TRUE)
    updateSelectizeInput(session, "instrument_filter", choices = ch$instrument, selected = character(0), server = TRUE)
    updateSelectizeInput(session, "pollutant_filter", choices = ch$pollutant, selected = character(0), server = TRUE)
    updateSelectizeInput(session, "site_filter", choices = ch$site, selected = character(0), server = TRUE)
  })

  output$filter_choices_status <- renderUI({
    if (is.null(input$dataset)) {
      return(tags$p(class = "small-note", "Upload a dataset to populate the filter dropdowns."))
    }
    ch <- dataset_filter_choices()
    if (!is.null(ch$error)) {
      return(tags$p(class = "run-error", paste("Could not read filter values:", ch$error)))
    }
    tags$p(
      class = "small-note",
      paste0(
        "Available filters loaded: ",
        length(ch$country), " countries, ",
        length(ch$instrument), " instruments, ",
        length(ch$pollutant), " pollutants, ",
        length(ch$site), " sites/campaigns."
      )
    )
  })

  output$subset_preview <- renderTable({
    data.frame(
      Filter = c("Country", "Instrument", "Pollutant", "Site/Campaign"),
      Selected = c(
        paste(filter_values(input$country_filter) %||% "ALL", collapse = ", "),
        paste(filter_values(input$instrument_filter) %||% "ALL", collapse = ", "),
        paste(filter_values(input$pollutant_filter) %||% "ALL", collapse = ", "),
        paste(filter_values(input$site_filter) %||% "ALL", collapse = ", ")
      )
    )
  }, bordered = TRUE, striped = TRUE, spacing = "s")

  observeEvent(input$run, {
    req(input$dataset)

    rv$run_complete <- FALSE
    rv$error <- NULL
    rv$log <- character()
    rv$run_dir <- NULL
    rv$output_dir <- NULL
    rv$zip_path <- NULL
    rv$html_path <- NULL
    rv$xlsx_path <- NULL
    rv$subset_info <- NULL

    run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
    # Keep run paths short by using the short app folder name GDE_v023.
    # Extract the app directly to a short path, e.g. C:/Users/claud/Downloads/GDE_v023.
    run_root <- file.path(app_dir, "runs")
    run_dir <- file.path(run_root, paste0("run_", run_id))
    input_dir <- file.path(run_dir, "input")
    output_dir <- file.path(run_dir, "output_equivalence")

    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    uploaded_name <- safe_file_name(input$dataset$name)
    input_path <- file.path(input_dir, uploaded_name)
    file.copy(input$dataset$datapath, input_path, overwrite = TRUE)

    selected_country <- filter_values(input$country_filter)
    selected_instrument <- filter_values(input$instrument_filter)
    selected_pollutant <- filter_values(input$pollutant_filter)
    selected_site <- filter_values(input$site_filter)

    filtered_input_path <- file.path(input_dir, "filtered_input_for_run.csv")
    subset_info <- filter_uploaded_dataset_for_run(
      input_path = input_path,
      output_path = filtered_input_path,
      country_filter = selected_country,
      instrument_filter = selected_instrument,
      size_filter = selected_pollutant,
      campaign_filter = selected_site
    )
    rv$subset_info <- subset_info

    aux_file_name <- NULL
    if (!is.null(input$aux_file) && !is.null(input$aux_file$datapath)) {
      aux_file_name <- safe_file_name(input$aux_file$name)
      file.copy(input$aux_file$datapath, file.path(input_dir, aux_file_name), overwrite = TRUE)
    }

    config_path <- file.path(input_dir, "generated_config.yml")
    if (identical(input$config_mode, "upload")) {
      req(input$config_upload)
      config_path <- file.path(input_dir, safe_file_name(input$config_upload$name))
      file.copy(input$config_upload$datapath, config_path, overwrite = TRUE)
    } else if (identical(input$config_mode, "edit")) {
      writeLines(input$config_text %||% default_editable_config_text(), config_path, useBytes = TRUE)
    } else {
      write_generated_config(config_path, input, auxiliary_file_name = aux_file_name)
    }

    withProgress(message = "Running assessment", value = 0, {
      incProgress(0.10, detail = "Preparing input and configuration")

      result <- tryCatch({
        log_output <- capture.output({
          run_equivalence_workflow(
            input_file = filtered_input_path,
            output_dir = output_dir,
            scenario = input$scenario,
            run_mode = input$run_mode,
            stop_on_rm_fail = isTRUE(input$stop_on_rm_fail),
            render_html = isTRUE(input$render_html),
            country_filter = NULL,
            instrument_filter = NULL,
            size_filter = NULL,
            campaign_filter = NULL,
            config_file = config_path
          )
        })
        list(ok = TRUE, log = log_output)
      }, error = function(e) {
        list(ok = FALSE, error = conditionMessage(e), log = character())
      })

      incProgress(0.80, detail = "Collecting outputs")

      rv$run_dir <- run_dir
      rv$output_dir <- output_dir
      rv$log <- result$log

      if (!isTRUE(result$ok)) {
        rv$error <- friendly_error(result$error)
      } else {
        html_candidate <- file.path(output_dir, "05_report", "equivalence_report.html")
        if (file.exists(html_candidate)) rv$html_path <- html_candidate

        xlsx_candidate <- file.path(output_dir, "equivalence_results_daily_first.xlsx")
        if (file.exists(xlsx_candidate)) rv$xlsx_path <- xlsx_candidate

        side_by_side_manifest <- tryCatch(
          create_candidate_side_by_side_manifest(output_dir),
          error = function(e) {
            rv$log <- c(rv$log, paste("Candidate side-by-side plot generation failed:", conditionMessage(e)))
            data.frame()
          }
        )
        if (is.data.frame(side_by_side_manifest) && nrow(side_by_side_manifest) > 0) {
          rv$log <- c(rv$log, paste("Candidate side-by-side plot manifest written with", nrow(side_by_side_manifest), "plots."))
        }

        rv$zip_path <- tryCatch(
          make_run_zip(output_dir),
          error = function(e) {
            rv$log <- c(rv$log, paste("ZIP creation failed:", conditionMessage(e)))
            NULL
          }
        )

        rv$run_complete <- TRUE
        updateTabItems(session, "sidebar", "dashboard")
      }

      incProgress(1, detail = "Done")
    })
  })

  output$run_status <- renderUI({
    if (!is.null(rv$error)) return(tags$p(class = "run-error", paste("Run failed:", rv$error)))
    if (isTRUE(rv$run_complete)) {
      subset_line <- ""
      if (!is.null(rv$subset_info)) {
        subset_line <- paste0(
          "Subset used: ", rv$subset_info$n_after, " / ", rv$subset_info$n_before, " rows. ",
          "Country: ", paste(rv$subset_info$country_filter, collapse = ", "), "; ",
          "Instrument: ", paste(rv$subset_info$instrument_filter, collapse = ", "), "; ",
          "Pollutant: ", paste(rv$subset_info$size_filter, collapse = ", "), "; ",
          "Site/Campaign: ", paste(rv$subset_info$campaign_filter, collapse = ", "), "."
        )
      }
      tags$div(
        tags$p(class = "run-ok", "Run completed."),
        tags$p(class = "small-note", paste("Output directory:", rv$output_dir)),
        tags$p(class = "small-note", paste("Run folder:", rv$run_dir)),
        tags$p(class = "small-note", subset_line)
      )
    } else {
      tags$p(class = "small-note", "Upload a dataset, choose the configuration and click Run assessment.")
    }
  })

  table_from <- function(...) {
    req(rv$output_dir)
    read_csv_safe_app(file.path(rv$output_dir, ...))
  }

  output$summary_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "method_summary.csv"), "summary"))
  output$final_site_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "final_site_combined_assessment.csv"), "final_site"))
  output$rm_screen_table <- DT::renderDT(small_table(table_from("00_run_log", "rm_screening_summary.csv"), "rm_screen"))
  output$rm_site_table <- DT::renderDT(small_table(table_from("02_diagnostics", "rm_diagnostics.csv"), "rm_site"))
  output$linearity_table <- DT::renderDT(small_table(table_from("02_diagnostics", "linearity_diagnostics.csv"), "linearity"))
  output$correction_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "correction_coefficients.csv"), "corrections"))
  output$diff_table <- DT::renderDT(small_table(table_from("02_diagnostics", "difference_diagnostics.csv")))
  output$diff_site_table <- DT::renderDT(small_table(table_from("02_diagnostics", "difference_site_campaign_diagnostics.csv")))
  output$daily_unit_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "daily_unit_results.csv"), "daily_unit"))
  output$daily_site_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "daily_site_campaign_results.csv"), "daily_site"))
  output$annual_unit_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "annual_unit_results.csv"), "annual_unit"))
  output$annual_site_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "annual_site_results.csv"), "annual_site"))
  output$neff_table <- DT::renderDT(small_table(table_from("03_uncertainty_tables", "V_Neff_inputs.csv"), "neff"))
  output$config_table <- DT::renderDT(small_table(table_from("00_run_log", "configuration_used.csv")))
  output$plot_manifest_table <- DT::renderDT(small_table(read_csv_safe_app(file.path(rv$output_dir, "04_plots", "plot_manifest.csv"))))

  method_summary <- reactive({
    req(rv$output_dir)
    read_csv_safe_app(file.path(rv$output_dir, "03_uncertainty_tables", "method_summary.csv"))
  })

  output$run_box <- renderValueBox({
    if (!isTRUE(rv$run_complete)) return(valueBox("Waiting", "Run status", icon = icon("hourglass-half"), color = "blue"))
    valueBox("Done", "Run status", icon = icon("check"), color = "green")
  })

  output$daily_box <- renderValueBox({
    df <- method_summary()
    if (nrow(df) == 0 || !"daily_method_status" %in% names(df)) return(valueBox("n/a", "Daily result", icon = icon("calendar-day"), color = "blue"))
    vals <- clean_status_text(unique(df$daily_method_status))
    valueBox(paste(vals, collapse = " / "), "Daily result", icon = icon("calendar-day"), color = status_color(vals))
  })

  output$annual_box <- renderValueBox({
    df <- method_summary()
    if (nrow(df) == 0 || !"annual_method_status" %in% names(df)) return(valueBox("n/a", "Annual result", icon = icon("calendar"), color = "blue"))
    vals <- clean_status_text(unique(df$annual_method_status))
    valueBox(paste(vals, collapse = " / "), "Annual result", icon = icon("calendar"), color = status_color(vals))
  })

  output$final_box <- renderValueBox({
    df <- method_summary()
    if (nrow(df) == 0 || !"final_method_status" %in% names(df)) return(valueBox("n/a", "Final result", icon = icon("flag-checkered"), color = "blue"))
    vals <- clean_status_text(unique(df$final_method_status))
    valueBox(paste(vals, collapse = " / "), "Final result", icon = icon("flag-checkered"), color = status_color(vals))
  })

  plot_files_all <- reactive({
    req(rv$output_dir)
    files <- list.files(rv$output_dir, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
    files[order(files)]
  })

  plot_files_matching <- function(pattern, exclude = NULL) {
    files <- plot_files_all()
    hit <- files[grepl(pattern, basename(files), ignore.case = TRUE) | grepl(pattern, files, ignore.case = TRUE)]
    if (!is.null(exclude) && length(hit) > 0) {
      hit <- hit[!grepl(exclude, basename(hit), ignore.case = TRUE) & !grepl(exclude, hit, ignore.case = TRUE)]
    }
    hit
  }

  first_plot <- function(pattern, exclude = NULL) {
    files <- plot_files_matching(pattern, exclude = exclude)
    if (length(files) == 0) return(NA_character_)
    files[1]
  }

  first_plot_prefer <- function(patterns, exclude = NULL, exclude_path = NULL) {
    for (pat in patterns) {
      files <- plot_files_matching(pat, exclude = exclude)
      if (!is.null(exclude_path) && length(files) > 0 && !is.na(exclude_path)) {
        files <- files[
          normalizePath(files, winslash = "/", mustWork = FALSE) !=
            normalizePath(exclude_path, winslash = "/", mustWork = FALSE)
        ]
      }
      if (length(files) > 0) return(files[1])
    }
    NA_character_
  }

  render_plot_file <- function(path_fun) {
    renderImage({
      path <- path_fun()
      validate(need(!is.na(path) && file.exists(path), "Plot not available for this run. Check 04_plots/plot_manifest_generation_log.txt in the output ZIP."))
      list(src = path, contentType = "image/png", width = "100%", alt = basename(path))
    }, deleteFile = FALSE)
  }

  plot_selector_ui <- function(id, files) {
    if (length(files) == 0) return(tags$p("No plots available."))
    labels <- sub(paste0("^", gsub("\\\\", "/", rv$output_dir), "/?"), "", gsub("\\\\", "/", files))
    selectInput(id, "Plot", choices = stats::setNames(files, labels), selected = files[1])
  }

  output$plot_selector <- renderUI(plot_selector_ui("selected_plot_file", plot_files_all()))

  output$selected_plot <- renderImage({
    req(input$selected_plot_file)
    list(src = input$selected_plot_file, contentType = "image/png", width = "100%", alt = basename(input$selected_plot_file))
  }, deleteFile = FALSE)

  # Reference-method plots. These are intentionally confined to the Reference screening section.
  output$plot_rm_scatter <- render_plot_file(function() first_plot("rm_duplicate_scatter"))
  output$plot_rm_bland <- render_plot_file(function() first_plot("rm_duplicate_bland"))
  output$plot_rm_histogram <- render_plot_file(function() first_plot("rm_duplicate_histogram"))
  output$plot_rm_timeseries <- render_plot_file(function() first_plot("rm_duplicate_time"))

  # Linearity and correction plots.
  output$plot_linearity <- render_plot_file(function() first_plot("linearity|correction_fit|regression", exclude = "rm_duplicate"))

  plot_manifest <- reactive({
    req(rv$output_dir)
    mf <- file.path(rv$output_dir, "04_plots", "plot_manifest.csv")
    if (!file.exists(mf)) {
      tryCatch(
        create_candidate_side_by_side_manifest(rv$output_dir),
        error = function(e) {
          rv$log <- c(rv$log, paste("On-demand candidate side-by-side plot generation failed:", conditionMessage(e)))
          data.frame()
        }
      )
    }
    if (!file.exists(mf)) return(data.frame())
    read_csv_safe_app(mf)
  })

  manifest_plot_path <- function(plot_id, plot_type = NULL) {
    mf <- plot_manifest()
    if (nrow(mf) == 0) return(NA_character_)

    row <- data.frame()
    if ("plot_id" %in% names(mf)) {
      row <- mf %>% dplyr::filter(.data$plot_id == !!plot_id)
    }
    if (nrow(row) == 0 && !is.null(plot_type) && "plot_type" %in% names(mf)) {
      row <- mf %>% dplyr::filter(.data$plot_type == !!plot_type)
    }
    if (nrow(row) == 0) return(NA_character_)

    candidates <- character(0)
    if ("path" %in% names(row)) candidates <- c(candidates, as.character(row$path[1]))
    if ("relative_path" %in% names(row)) candidates <- c(candidates, file.path(rv$output_dir, as.character(row$relative_path[1])))

    candidates <- candidates[nzchar(candidates)]
    candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0) return(NA_character_)
    hit[1]
  }

  # Candidate-method diagnostics are selected from plot_manifest.csv.
  # These plots are generated from long_stage_data.csv with coherent axes for
  # the no-correction and after-correction stages.
  output$plot_cm_bland <- render_plot_file(function() manifest_plot_path("cm_diff_vs_reference_sbs", "difference_vs_reference"))
  output$plot_cm_histogram <- render_plot_file(function() manifest_plot_path("cm_difference_histogram_sbs", "difference_distribution"))
  output$plot_cm_timeseries <- render_plot_file(function() manifest_plot_path("cm_difference_time_series_sbs", "difference_time_series"))
  output$plot_cm_site_diagnostics <- render_plot_file(function() manifest_plot_path("cm_site_campaign_bias_sbs", "site_campaign_bias"))

  # Daily and annual plots.
  output$plot_daily_unit <- render_plot_file(function() first_plot_prefer(c("daily_primary_uncertainty", "daily.*primary", "decomposed_daily", "daily.*uncertainty")))
  output$plot_daily_site <- render_plot_file(function() first_plot_prefer(c("daily_site_campaign_guardrail", "daily.*site", "site_campaign.*daily", "daily_site_campaign")))
  output$plot_annual_unit <- render_plot_file(function() first_plot_prefer(c("annual_primary_uncertainty", "annual.*primary", "annual.*uncertainty")))
  output$plot_annual_site <- render_plot_file(function() first_plot_prefer(c("annual_site_campaign_uncertainty", "annual.*site", "site_campaign.*annual", "annual_site_campaign")))

  output$run_log <- renderText({
    if (!is.null(rv$error)) return(rv$error)
    if (length(rv$log) == 0) return("No run log available yet.")
    paste(rv$log, collapse = "\n")
  })

  output$download_zip <- downloadHandler(
    filename = function() paste0("gde_equivalence_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      req(rv$zip_path)
      file.copy(rv$zip_path, file, overwrite = TRUE)
    }
  )

  output$download_html <- downloadHandler(
    filename = function() "equivalence_report.html",
    content = function(file) {
      req(rv$html_path)
      file.copy(rv$html_path, file, overwrite = TRUE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() "equivalence_results_daily_first.xlsx",
    content = function(file) {
      req(rv$xlsx_path)
      file.copy(rv$xlsx_path, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
