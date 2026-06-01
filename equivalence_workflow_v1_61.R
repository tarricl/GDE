# =============================================================================
# Equivalence workflow for Candidate Method (CM) assessment
# Daily-first + Annual assessment + optional correction/rechecking
# Version: 0.1.60
# =============================================================================
#
# Command-line usage:
#   Rscript equivalence_workflow_v1_21.R input_file output_dir scenario run_mode stop_on_rm_fail
#
# Example type testing:
#   Rscript equivalence_workflow_v1_21.R "input.xlsx" output_equivalence type_testing full_workflow TRUE
#
# Example ongoing verification / corrected assessment:
#   Rscript equivalence_workflow_v1_21.R input.xlsx output_ongoing ongoing_verification full_workflow FALSE
#
# Main design decisions implemented:
#   - RM duplicate screening is mandatory where RM1 and RM2 exist.
#   - RM_SCREEN_FAIL_REDO if >5% of duplicate RM rows have |RM1 - RM2| > 2 µg/m3.
#   - Two limit values are used: LV_daily and LV_annual.
#   - Output and report are daily-first, then annual.
#   - Primary uncertainty metric is 2 * random component + systematic component.
#   - Annual is always calculated on all screened valid paired data.
#   - Daily is calculated in the LV_daily ±30% window.
#   - Type testing official decision uses Raw stage only.
#   - Ongoing verification includes Raw plus TLS correction stage.
#   - CM1 and CM2 are assessed separately; method-level PASS requires all required units to pass.
#   - Site/campaign daily failures are guardrails for type testing.
#
# Required input columns after standardisation:
#   Instrument, Size, RM1, CM1
# Optional columns:
#   date, Campaign, RM2, CM2, SN1, SN2
#
# =============================================================================

# ---- Packages ----------------------------------------------------------------
required_packages <- c(
  "dplyr", "tidyr", "purrr", "ggplot2", "readxl", "readr",
  "openxlsx", "tibble", "stringr", "rlang", "yaml"
)

load_required_packages <- function(packages = required_packages, install_missing = FALSE) {
  missing_pkgs <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_pkgs) > 0) {
    if (isTRUE(install_missing)) {
      install.packages(missing_pkgs)
    } else {
      stop(
        "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
        "\nInstall them first, or call load_required_packages(install_missing = TRUE)."
      )
    }
  }
  invisible(lapply(packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))))
}
load_required_packages()

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

safe_name <- function(x) {
  x <- paste(x, collapse = "_")
  x <- gsub("[^A-Za-z0-9_.-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nchar(x) == 0, "unnamed", x)
}

has_col <- function(df, nm) nm %in% names(df)

parse_date_flexible <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  # Excel serial numbers can occur in xlsx/csv exports.
  if (is.numeric(x)) {
    out <- suppressWarnings(as.Date(x, origin = "1899-12-30"))
    return(out)
  }

  x_chr <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x_chr))

  formats <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
    "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y",
    "%d/%m/%Y %H:%M", "%d/%m/%Y %H:%M:%S",
    "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"
  )

  for (fmt in formats) {
    idx <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)
    if (!any(idx)) break
    parsed <- suppressWarnings(as.Date(x_chr[idx], format = fmt))
    out[idx][!is.na(parsed)] <- parsed[!is.na(parsed)]
  }

  # Last attempt for numeric-looking Excel serials stored as text.
  idx <- is.na(out) & grepl("^[0-9]+(\\.[0-9]+)?$", x_chr)
  if (any(idx)) {
    out[idx] <- suppressWarnings(as.Date(as.numeric(x_chr[idx]), origin = "1899-12-30"))
  }

  out
}


safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    return(NA_real_)
  }
  stats::sd(x)
}

safe_var <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    return(NA_real_)
  }
  stats::var(x)
}

left_join_allow_many <- function(x, y, by) {
  # dplyr >= 1.1 supports relationship = "many-to-many"; older versions do not.
  # This helper makes the intended duplication explicit for correction stages
  # while remaining compatible with older dplyr releases.
  tryCatch(
    dplyr::left_join(x, y, by = by, relationship = "many-to-many"),
    error = function(e) dplyr::left_join(x, y, by = by)
  )
}

safe_quantile <- function(x, p = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
}


safe_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) {
    return(NA_real_)
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) {
    return(NA_real_)
  }
  mean(((x - m) / s)^3)
}

safe_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 4) {
    return(NA_real_)
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) {
    return(NA_real_)
  }
  mean(((x - m) / s)^4)
}

# ---- Config ------------------------------------------------------------------

message("Loaded equivalence workflow v1.33: VERIFIED daily bias logic (global signed bias; local site/campaign absolute bias).")


message("Loaded equivalence workflow v1.36: executive Main issue consistency fix; daily CI95 low-N not converted to FAIL.")

message("Loaded equivalence workflow v1.61: CM-level daily site PASS + LOW N logic fix.")

default_config <- list(
  workflow_version = "0.1.61",

  # Correction / calibration configuration.
  correction_enabled = TRUE,
  correction_model = "TLS", # TLS, OLS, MEAN_RATIO, NONE

  # Daily LV decision metric. Other metrics are still retained in the Annex.
  daily_decision_metric = "CI95", # Udaily, CI95, Utot, Udiff

  rm_duplicate_abs_threshold = 2.0,
  rm_fail_pct_threshold = 5.0,
  lv_table = tibble::tribble(
    ~Size, ~LV_daily, ~LV_annual,
    "PM10", 45, 20,
    "PM2.5", 25, 10
  ),
  # Configurable DQO. Edit here if final legal values differ.
  dqo_table = tibble::tribble(
    ~Size, ~LV_type, ~DQO_percent,
    "PM10", "daily", 25,
    "PM10", "annual", 20,
    "PM2.5", "daily", 25,
    "PM2.5", "annual", 30
  ),
  # Fallback V from current working methodology; can be replaced by external V table later.
  vrep_table = tibble::tribble(
    ~Size,   ~Vrep,
    "PM10",   4.04,
    "PM2.5",  5.46
  ),
  min_consecutive_pairs_for_V = 40,
  auxiliary_autocorrelation_allowed = TRUE,
  auxiliary_autocorrelation_file = NULL,
  auxiliary_min_consecutive_pairs_for_V = 40,
  daily_n_ok = 15,
  daily_n_warning = 10,
  min_n_campaign_diagnostic = 20,
  type_testing_r2_threshold = 0.95,
  ongoing_r2_threshold = 0.90,

  # Linearity/correction fitting range. This affects only the regression used for
  # linearity and correction coefficients, not daily/annual uncertainty datasets.
  tls_fit_rm_min = 0,
  tls_fit_rm_max_factor_daily_LV = 1.5,

  # Daily LV window for daily assessment.
  daily_lv_window_lower_factor = 0.7,
  daily_lv_window_upper_factor = 1.3,

  # Diagnostic thresholds from the methodology report, Table 1.
  diagnostic_mbe_threshold = 1.0,
  diagnostic_sd_threshold = 2.5,
  diagnostic_ubscm_threshold = 2.5,
  diagnostic_skewness_threshold = 0.5,
  diagnostic_kurtosis_threshold = 4.0
)


# ---- YAML configuration -------------------------------------------------------
flatten_named_values <- function(x, prefix = NULL) {
  if (is.null(x)) return(tibble::tibble(parameter = character(), value = character()))
  out <- list()
  walk_list <- function(obj, pfx = NULL) {
    if (is.list(obj) && !is.data.frame(obj)) {
      for (nm in names(obj)) {
        walk_list(obj[[nm]], c(pfx, nm))
      }
    } else {
      out[[length(out) + 1]] <<- tibble::tibble(
        parameter = paste(pfx, collapse = "."),
        value = paste(as.character(obj), collapse = ", ")
      )
    }
  }
  walk_list(x, prefix)
  dplyr::bind_rows(out)
}

yaml_limit_table <- function(x) {
  if (is.null(x)) return(NULL)
  tibble::tibble(
    Size = names(x),
    LV_daily = purrr::map_dbl(x, ~ as.numeric(.x$daily %||% NA_real_)),
    LV_annual = purrr::map_dbl(x, ~ as.numeric(.x$annual %||% NA_real_))
  )
}

yaml_dqo_table <- function(x) {
  if (is.null(x)) return(NULL)
  purrr::map_dfr(names(x), function(sz) {
    tibble::tibble(
      Size = sz,
      LV_type = c("daily", "annual"),
      DQO_percent = c(as.numeric(x[[sz]]$daily %||% NA_real_), as.numeric(x[[sz]]$annual %||% NA_real_))
    )
  })
}

load_workflow_config <- function(config_file = NULL, base_config = default_config) {
  config <- base_config

  if (is.null(config_file) || !nzchar(config_file)) {
    config$config_file <- NA_character_
    return(config)
  }

  if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file)
  }

  y <- yaml::read_yaml(config_file)

  config$config_file <- normalizePath(config_file, winslash = "/", mustWork = FALSE)

  # Correction model
  if (!is.null(y$correction)) {
    config$correction_enabled <- isTRUE(y$correction$enabled %||% config$correction_enabled)
    config$correction_model <- toupper(as.character(y$correction$model %||% config$correction_model))
    if (!is.null(y$correction$fit_range)) {
      config$tls_fit_rm_min <- as.numeric(y$correction$fit_range$lower_rm %||% config$tls_fit_rm_min)
      config$tls_fit_rm_max_factor_daily_LV <- as.numeric(y$correction$fit_range$upper_factor_daily_lv %||% config$tls_fit_rm_max_factor_daily_LV)
    }
  }

  # Daily assessment
  if (!is.null(y$daily_assessment)) {
    config$daily_decision_metric <- toupper(as.character(y$daily_assessment$decision_metric %||% config$daily_decision_metric))
    config$daily_lv_window_lower_factor <- as.numeric(y$daily_assessment$lv_window_lower_factor %||% config$daily_lv_window_lower_factor)
    config$daily_lv_window_upper_factor <- as.numeric(y$daily_assessment$lv_window_upper_factor %||% config$daily_lv_window_upper_factor)
    config$daily_n_ok <- as.integer(y$daily_assessment$min_n_ok %||% config$daily_n_ok)
    config$daily_n_warning <- as.integer(y$daily_assessment$min_n_low_n %||% config$daily_n_warning)
  }

  # Annual assessment
  if (!is.null(y$annual_assessment)) {
    config$min_consecutive_pairs_for_V <- as.integer(y$annual_assessment$min_consecutive_pairs_for_V %||% config$min_consecutive_pairs_for_V)
    config$auxiliary_autocorrelation_allowed <- isTRUE(y$annual_assessment$auxiliary_autocorrelation_allowed %||% config$auxiliary_autocorrelation_allowed)
    config$auxiliary_autocorrelation_file <- y$annual_assessment$auxiliary_autocorrelation_file %||% config$auxiliary_autocorrelation_file
    config$auxiliary_min_consecutive_pairs_for_V <- as.integer(y$annual_assessment$auxiliary_min_consecutive_pairs_for_V %||% config$auxiliary_min_consecutive_pairs_for_V)

    if (!is.null(config$auxiliary_autocorrelation_file) && !is.na(config$auxiliary_autocorrelation_file) && nzchar(config$auxiliary_autocorrelation_file)) {
      aux_path <- as.character(config$auxiliary_autocorrelation_file)
      if (!file.exists(aux_path) && !is.null(config$config_file) && !is.na(config$config_file)) {
        candidate <- file.path(dirname(config$config_file), aux_path)
        if (file.exists(candidate)) aux_path <- candidate
      }
      config$auxiliary_autocorrelation_file <- aux_path
    }
  }

  # Diagnostics
  if (!is.null(y$diagnostics)) {
    config$diagnostic_mbe_threshold <- as.numeric(y$diagnostics$mbe_threshold %||% config$diagnostic_mbe_threshold)
    config$diagnostic_sd_threshold <- as.numeric(y$diagnostics$sd_diff_threshold %||% config$diagnostic_sd_threshold)
    config$diagnostic_skewness_threshold <- as.numeric(y$diagnostics$skewness_threshold %||% config$diagnostic_skewness_threshold)
    config$diagnostic_kurtosis_threshold <- as.numeric(y$diagnostics$kurtosis_threshold %||% config$diagnostic_kurtosis_threshold)
    config$diagnostic_ubscm_threshold <- as.numeric(y$diagnostics$ubscm_threshold %||% config$diagnostic_ubscm_threshold)
  }

  # Linearity thresholds
  if (!is.null(y$linearity)) {
    config$type_testing_r2_threshold <- as.numeric(y$linearity$r2_threshold_type_testing %||% config$type_testing_r2_threshold)
    config$ongoing_r2_threshold <- as.numeric(y$linearity$r2_threshold_ongoing %||% config$ongoing_r2_threshold)
  }

  # RM screening
  if (!is.null(y$rm_screening)) {
    config$rm_duplicate_abs_threshold <- as.numeric(y$rm_screening$duplicate_abs_threshold %||% config$rm_duplicate_abs_threshold)
    config$rm_fail_pct_threshold <- as.numeric(y$rm_screening$fail_pct_threshold %||% config$rm_fail_pct_threshold)
  }

  # Limit values and DQO
  lv_tbl <- yaml_limit_table(y$limit_values)
  if (!is.null(lv_tbl) && nrow(lv_tbl) > 0) config$lv_table <- lv_tbl

  dqo_tbl <- yaml_dqo_table(y$dqo)
  if (!is.null(dqo_tbl) && nrow(dqo_tbl) > 0) config$dqo_table <- dqo_tbl

  # V fallback
  if (!is.null(y$vrep)) {
    config$vrep_table <- tibble::tibble(
      Size = names(y$vrep),
      Vrep = purrr::map_dbl(y$vrep, as.numeric)
    )
  }

  valid_corrections <- c("TLS", "OLS", "MEAN_RATIO", "NONE")
  if (!toupper(config$correction_model) %in% valid_corrections) {
    stop("Unsupported correction model: ", config$correction_model,
         ". Use one of: ", paste(valid_corrections, collapse = ", "))
  }

  valid_daily <- c("UDAILY", "CI95", "UTOT", "UDIFF")
  if (!toupper(config$daily_decision_metric) %in% valid_daily) {
    stop("Unsupported daily decision metric: ", config$daily_decision_metric,
         ". Use one of: ", paste(valid_daily, collapse = ", "))
  }

  config
}

config_summary_table <- function(config) {
  tibble::tibble(
    Parameter = c(
      "Workflow version",
      "Config file",
      "Correction enabled",
      "Correction model",
      "Correction/linearity fitting range",
      "Daily decision metric",
      "Daily LV window",
      "Daily min n OK",
      "Daily min n LOW N",
      "Type-testing R2 threshold",
      "Ongoing R2 threshold",
      "RM duplicate threshold",
      "RM warning threshold",
      "MBE diagnostic threshold",
      "SD(diff) diagnostic threshold",
      "Skewness diagnostic threshold",
      "Kurtosis diagnostic threshold",
      "UbsCM diagnostic threshold",
      "Minimum consecutive pairs for V",
      "Auxiliary autocorrelation allowed",
      "Auxiliary autocorrelation file",
      "Auxiliary minimum consecutive pairs for V"
    ),
    Value = c(
      as.character(config$workflow_version),
      as.character(config$config_file %||% "internal defaults"),
      as.character(config$correction_enabled),
      as.character(config$correction_model),
      paste0(config$tls_fit_rm_min, " to ", config$tls_fit_rm_max_factor_daily_LV, " LVd"),
      as.character(toupper(config$daily_decision_metric)),
      paste0(config$daily_lv_window_lower_factor, " to ", config$daily_lv_window_upper_factor, " LVd"),
      as.character(config$daily_n_ok),
      as.character(config$daily_n_warning),
      as.character(config$type_testing_r2_threshold),
      as.character(config$ongoing_r2_threshold),
      as.character(config$rm_duplicate_abs_threshold),
      paste0(config$rm_fail_pct_threshold, "%"),
      as.character(config$diagnostic_mbe_threshold),
      as.character(config$diagnostic_sd_threshold),
      as.character(config$diagnostic_skewness_threshold),
      as.character(config$diagnostic_kurtosis_threshold),
      as.character(config$diagnostic_ubscm_threshold),
      as.character(config$min_consecutive_pairs_for_V),
      as.character(config$auxiliary_autocorrelation_allowed),
      as.character(config$auxiliary_autocorrelation_file %||% NA_character_),
      as.character(config$auxiliary_min_consecutive_pairs_for_V)
    )
  )
}



# ---- Script location ----------------------------------------------------------
.workflow_script_dir <- local({
  p <- NA_character_
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (!is.null(frames[[i]]$ofile)) {
      p <- frames[[i]]$ofile
      break
    }
  }
  if (is.na(p) || !nzchar(p)) {
    ca <- commandArgs(trailingOnly = FALSE)
    ff <- grep("^--file=", ca, value = TRUE)
    if (length(ff) > 0) p <- sub("^--file=", "", ff[[1]])
  }
  if (!is.na(p) && nzchar(p)) dirname(normalizePath(p, winslash = "/", mustWork = FALSE)) else getwd()
})

# ---- Country/site helper ------------------------------------------------------
derive_country_from_campaign <- function(campaign) {
  y <- stringr::str_to_lower(as.character(campaign))
  dplyr::case_when(
    grepl("teddington|birmingham|bristol|east kilbride|manchester", y) ~ "United Kingdom",
    grepl("cologne|köln|bornheim|bonn|niederzier|bulk handling|hambach|duisburg|bruehl|rodenkirchen|wieselfeld|berlin", y) ~ "Germany",
    grepl("ispra|rome", y) ~ "Italy",
    grepl("vienna|graz|steyregg", y) ~ "Austria",
    grepl("madrid", y) ~ "Spain",
    grepl("athens", y) ~ "Greece",
    grepl("vredepeel", y) ~ "Netherlands",
    grepl("aspvreten|furulund", y) ~ "Sweden",
    grepl("tusimice", y) ~ "Czechia",
    TRUE ~ "Unknown"
  )
}

filter_focus <- function(df, country_filter = NULL, instrument_filter = NULL, size_filter = NULL, campaign_filter = NULL) {
  out <- df
  if (!is.null(country_filter) && !identical(country_filter, "")) out <- out %>% filter(Country %in% country_filter)
  if (!is.null(instrument_filter) && !identical(instrument_filter, "")) out <- out %>% filter(Instrument %in% instrument_filter)
  if (!is.null(size_filter) && !identical(size_filter, "")) out <- out %>% filter(Size %in% size_filter)
  if (!is.null(campaign_filter) && !identical(campaign_filter, "")) out <- out %>% filter(Campaign %in% campaign_filter)
  if (nrow(out) == 0) stop("No rows remain after applying focus filters. Check available_filters.csv in the output folder, or relax country/instrument/size/campaign filters.")
  out
}

make_available_filters <- function(df) {
  df %>%
    group_by(Country, Instrument, Size, Campaign) %>%
    summarise(n_rows = n(), .groups = "drop") %>%
    arrange(Country, Instrument, Size, Campaign)
}

# ---- Data ingestion and standardisation --------------------------------------
read_equivalence_input <- function(input_file, sheet = NULL) {
  if (!file.exists(input_file)) stop("Input file not found: ", input_file)
  ext <- tolower(tools::file_ext(input_file))

  if (ext %in% c("xlsx", "xls")) {
    sheets <- sheet %||% readxl::excel_sheets(input_file)
    out <- purrr::map_dfr(sheets, function(sh) {
      readxl::read_excel(input_file, sheet = sh) %>% mutate(source_sheet = sh)
    })
  } else if (ext %in% c("csv", "txt")) {
    first_line <- readLines(input_file, n = 1, warn = FALSE)
    delim <- if (length(first_line) > 0 && grepl(";", first_line) && !grepl(",", first_line)) ";" else ","
    out <- readr::read_delim(input_file, delim = delim, show_col_types = FALSE)
  } else if (ext == "rds") {
    out <- readRDS(input_file)
  } else {
    stop("Unsupported input format: .", ext)
  }
  as.data.frame(out)
}

standardise_cm_data <- function(df) {
  names(df) <- trimws(names(df))

  date_candidates <- c("date", "Date", "Start Date", "Start Date and Time", "Start_Date", "start_date")
  size_candidates <- c("Size", "Size Fraction", "Size_Fraction", "size")
  campaign_candidates <- c("Campaign", "Site", "Location", "campaign", "site")

  date_col <- intersect(date_candidates, names(df))[1]
  size_col <- intersect(size_candidates, names(df))[1]
  campaign_col <- intersect(campaign_candidates, names(df))[1]

  if (!is.na(date_col) && date_col != "date") df <- df %>% rename(date = !!date_col)
  if (!is.na(size_col) && size_col != "Size") df <- df %>% rename(Size = !!size_col)
  if (!is.na(campaign_col) && campaign_col != "Campaign") df <- df %>% rename(Campaign = !!campaign_col)

  if (!has_col(df, "date")) df$date <- seq_len(nrow(df))
  if (!has_col(df, "Campaign")) df$Campaign <- "Campaign_1"
  if (!has_col(df, "Instrument")) stop("Missing required column: Instrument")
  if (!has_col(df, "Size")) stop("Missing required column: Size")
  if (!has_col(df, "RM1")) stop("Missing required column: RM1")
  if (!has_col(df, "CM1")) stop("Missing required column: CM1")

  numeric_cols <- intersect(c("RM1", "RM2", "CM1", "CM2"), names(df))
  df <- df %>% mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.x))))

  df <- df %>%
    mutate(
      Size = case_when(
        grepl("PM\\s*2[.,]?5|PM25", as.character(Size), ignore.case = TRUE) ~ "PM2.5",
        grepl("PM\\s*10", as.character(Size), ignore.case = TRUE) ~ "PM10",
        TRUE ~ as.character(Size)
      ),
      Instrument = as.character(Instrument),
      Campaign = as.character(Campaign)
    )

  if (!has_col(df, "Country")) {
    df <- df %>% mutate(Country = derive_country_from_campaign(Campaign))
  } else {
    df <- df %>% mutate(Country = as.character(Country))
  }

  # Best-effort date parsing. The autocorrelation calculation requires true calendar dates.
  parsed_date <- parse_date_flexible(df$date)
  if (!all(is.na(parsed_date))) df$date <- parsed_date

  rm_cols <- intersect(c("RM1", "RM2"), names(df))
  df <- df %>%
    mutate(
      RM_AVG_raw = if (length(rm_cols) > 1) rowMeans(across(all_of(rm_cols)), na.rm = TRUE) else .data[[rm_cols[1]]],
      RM_AVG_raw = ifelse(is.nan(RM_AVG_raw), NA_real_, RM_AVG_raw),
      row_id = dplyr::row_number()
    )
  df
}

add_limit_values <- function(df, config = default_config) {
  df %>% left_join(config$lv_table, by = "Size")
}

# ---- RM screening -------------------------------------------------------------
rm_screening <- function(df, config = default_config, stop_on_fail = TRUE) {
  if (!all(c("RM1", "RM2") %in% names(df))) {
    summary <- tibble::tibble(
      n_rows_total = nrow(df),
      n_rows_with_rm_duplicates = 0L,
      n_rm_excluded = 0L,
      pct_excluded_duplicate_denominator = NA_real_,
      pct_excluded_total_denominator = NA_real_,
      rm_screen_flag = "RM_SCREEN_NOT_APPLICABLE"
    )
    df$rm_screen_excluded <- FALSE
    df$RM_AVG <- df$RM_AVG_raw
    return(list(data = df, summary = summary))
  }

  out <- df %>%
    mutate(
      has_rm_duplicates = !is.na(RM1) & !is.na(RM2),
      rm_duplicate_abs_diff = abs(RM1 - RM2),
      rm_screen_excluded = has_rm_duplicates & rm_duplicate_abs_diff > config$rm_duplicate_abs_threshold
    )

  n_dup <- sum(out$has_rm_duplicates, na.rm = TRUE)
  n_excl <- sum(out$rm_screen_excluded, na.rm = TRUE)
  pct_dup <- if (n_dup > 0) 100 * n_excl / n_dup else NA_real_
  pct_total <- if (nrow(out) > 0) 100 * n_excl / nrow(out) else NA_real_

  flag <- case_when(
    is.na(pct_dup) ~ "RM_SCREEN_NOT_APPLICABLE",
    pct_dup <= config$rm_fail_pct_threshold ~ "RM_SCREEN_PASS",
    pct_dup > config$rm_fail_pct_threshold ~ "RM_SCREEN_FAIL_REDO",
    TRUE ~ "RM_SCREEN_UNKNOWN"
  )

  summary <- tibble::tibble(
    n_rows_total = nrow(out),
    n_rows_with_rm_duplicates = n_dup,
    n_rm_excluded = n_excl,
    pct_excluded_duplicate_denominator = pct_dup,
    pct_excluded_total_denominator = pct_total,
    rm_screen_flag = flag
  )

  if (identical(flag, "RM_SCREEN_FAIL_REDO") && isTRUE(stop_on_fail)) {
    dir.create(".", showWarnings = FALSE)
    stop(
      "RM screening failed: ", round(pct_dup, 2),
      "% of duplicate RM rows have |RM1 - RM2| > ", config$rm_duplicate_abs_threshold,
      " µg/m3. According to the configured rule, the workflow stops."
    )
  }

  out <- out %>%
    filter(!rm_screen_excluded) %>%
    mutate(
      RM_AVG = rowMeans(across(any_of(c("RM1", "RM2"))), na.rm = TRUE),
      RM_AVG = ifelse(is.nan(RM_AVG), NA_real_, RM_AVG)
    )

  list(data = out, summary = summary)
}

estimate_ubsRM <- function(df) {
  if (!all(c("RM1", "RM2") %in% names(df))) {
    return(df %>% distinct(Instrument, Size) %>% mutate(ubsRM = 0, n_rm_duplicate_for_ubs = 0L))
  }
  df %>%
    filter(!is.na(RM1), !is.na(RM2)) %>%
    group_by(Instrument, Size) %>%
    summarise(
      n_rm_duplicate_for_ubs = n(),
      ubsRM = sqrt(sum((RM1 - RM2)^2, na.rm = TRUE) / (2 * n())),
      .groups = "drop"
    ) %>%
    right_join(df %>% distinct(Instrument, Size), by = c("Instrument", "Size")) %>%
    mutate(
      n_rm_duplicate_for_ubs = replace_na(n_rm_duplicate_for_ubs, 0L),
      ubsRM = replace_na(ubsRM, 0)
    )
}

# ---- Eligibility --------------------------------------------------------------
input_data_check <- function(df, scenario = c("type_testing", "ongoing_verification")) {
  scenario <- match.arg(scenario)
  cols <- c("Country", "Instrument", "Size", "date", "Campaign", "RM1", "RM2", "CM1", "CM2", "SN1", "SN2")
  tibble::tibble(
    column = cols,
    status = ifelse(cols %in% names(df), "found", "missing"),
    mandatory = case_when(
      column %in% c("Instrument", "Size", "date", "Campaign", "RM1", "CM1") ~ TRUE,
      scenario == "type_testing" & column %in% c("RM2", "CM2") ~ TRUE,
      TRUE ~ FALSE
    ),
    notes = case_when(
      column == "RM2" ~ "Mandatory for type testing; optional for ongoing verification.",
      column == "CM2" ~ "Mandatory for type testing; optional for ongoing verification.",
      column %in% c("SN1", "SN2") ~ "Optional serial number metadata.",
      TRUE ~ ""
    )
  )
}

prepare_long_cm_data <- function(df, scenario = c("type_testing", "ongoing_verification"), config = default_config) {
  scenario <- match.arg(scenario)
  cm_cols <- intersect(c("CM1", "CM2"), names(df))
  if (scenario == "type_testing" && !all(c("CM1", "CM2") %in% cm_cols)) {
    stop("Type testing requires CM1 and CM2.")
  }
  if (length(cm_cols) == 0) stop("No CM columns found.")

  id_cols <- intersect(c("row_id", "date", "Country", "Campaign", "Instrument", "Size", "RM1", "RM2", "RM_AVG", "LV_daily", "LV_annual"), names(df))
  sn_cols <- intersect(c("SN1", "SN2"), names(df))

  long <- df %>%
    select(all_of(id_cols), all_of(sn_cols), all_of(cm_cols)) %>%
    pivot_longer(cols = all_of(cm_cols), names_to = "CM_type", values_to = "CM_raw") %>%
    filter(!is.na(RM_AVG), !is.na(CM_raw)) %>%
    mutate(
      SN = case_when(
        CM_type == "CM1" & "SN1" %in% names(.) ~ as.character(SN1),
        CM_type == "CM2" & "SN2" %in% names(.) ~ as.character(SN2),
        TRUE ~ NA_character_
      ),
      Site = derive_site(Campaign)
    )

  long
}

derive_site <- function(campaign) {
  x <- as.character(campaign)
  x <- stringr::str_replace_all(x, regex("\\b(summer|winter|spring|autumn|fall)\\b", ignore_case = TRUE), "")
  x <- stringr::str_replace_all(x, regex("\\b(campaign|camp)\\b", ignore_case = TRUE), "")
  x <- stringr::str_replace_all(x, "[_-]+", " ")
  x <- stringr::str_squish(x)
  ifelse(is.na(x) | x == "", as.character(campaign), x)
}

# ---- Regression and corrections ----------------------------------------------
tls_fit <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2) {
    return(list(intercept = NA_real_, slope = NA_real_, n = length(x)))
  }
  Z <- cbind(x, y)
  meanZ <- matrix(rep(colMeans(Z), nrow(Z)), ncol = 2, byrow = TRUE)
  sv <- svd(Z - meanZ)
  v <- sv$v
  slope <- -v[1, 2] / v[2, 2]
  intercept <- mean(Z %*% v[, 2]) / v[2, 2]
  list(intercept = as.numeric(intercept), slope = as.numeric(slope), n = length(x))
}

ols_fit_internal <- function(x, y) {
  # Kept for internal diagnostics/checks only. OLS coefficients are deliberately
  # not exported to CSV/Excel/HTML and are not displayed in the report.
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2) {
    return(list(intercept = NA_real_, slope = NA_real_, n = length(x)))
  }
  fit <- stats::lm(y ~ x)
  list(intercept = unname(stats::coef(fit)[1]), slope = unname(stats::coef(fit)[2]), n = length(x))
}


filter_tls_fit_range <- function(df, config = default_config, value_col = "CM_raw") {
  # Restrict only the regression/linearity fitting dataset.
  # Uncertainty calculations are not filtered by this function.
  if (!("LV_daily" %in% names(df))) {
    return(df %>% filter(is.finite(RM_AVG), is.finite(.data[[value_col]])))
  }
  df %>%
    filter(
      is.finite(RM_AVG),
      is.finite(.data[[value_col]]),
      RM_AVG >= config$tls_fit_rm_min,
      RM_AVG <= config$tls_fit_rm_max_factor_daily_LV * LV_daily
    )
}

tls_fit_range_label <- function(config = default_config) {
  paste0(config$tls_fit_rm_min, "-", config$tls_fit_rm_max_factor_daily_LV, " LVd")
}


estimate_corrections <- function(long_df, config = default_config) {
  model <- toupper(config$correction_model %||% "TLS")
  enabled <- isTRUE(config$correction_enabled) && model != "NONE"

  if (!enabled) {
    return(tibble::tibble(
      Instrument = character(), Size = character(), correction_model = character(),
      tls_fit_range = character(), n_regression_total = integer(),
      n_regression = integer(), n_regression_excluded = integer(),
      intercept = numeric(), slope = numeric(), ratio_factor = numeric()
    ))
  }

  long_df %>%
    group_by(Instrument, Size) %>%
    group_modify(~ {
      total_valid <- .x %>%
        filter(is.finite(RM_AVG), is.finite(CM_raw)) %>%
        nrow()

      fit_df <- filter_tls_fit_range(.x, config, value_col = "CM_raw")
      x <- fit_df$RM_AVG
      y <- fit_df$CM_raw

      if (model == "TLS") {
        fit <- tls_fit(x, y)
        intercept <- fit$intercept
        slope <- fit$slope
        ratio_factor <- NA_real_
        n_fit <- fit$n
      } else if (model == "OLS") {
        fit <- ols_fit_internal(x, y)
        intercept <- fit$intercept
        slope <- fit$slope
        ratio_factor <- NA_real_
        n_fit <- fit$n
      } else if (model == "MEAN_RATIO") {
        ok <- is.finite(x) & is.finite(y) & mean(x[is.finite(x)], na.rm = TRUE) != 0
        x_ok <- x[ok]
        y_ok <- y[ok]
        ratio_factor <- if (length(x_ok) > 0) mean(y_ok, na.rm = TRUE) / mean(x_ok, na.rm = TRUE) else NA_real_
        intercept <- 0
        slope <- ratio_factor
        n_fit <- length(x_ok)
      } else {
        stop("Unsupported correction model: ", model)
      }

      tibble::tibble(
        correction_model = model,
        tls_fit_range = tls_fit_range_label(config),
        n_regression_total = total_valid,
        n_regression = n_fit,
        n_regression_excluded = max(total_valid - n_fit, 0),
        intercept = intercept,
        slope = slope,
        ratio_factor = ratio_factor
      )
    }, .keep = TRUE) %>%
    ungroup()
}

apply_correction_stages <- function(long_df, corrections, scenario = c("type_testing", "ongoing_verification")) {
  scenario <- match.arg(scenario)

  raw <- long_df %>%
    mutate(Stage = "No correction", correction_model = "RAW", CM_value = CM_raw)

  if (is.null(corrections) || nrow(corrections) == 0) {
    return(raw)
  }

  # Both type testing and ongoing verification are reported with the no-correction
  # stage and, where correction coefficients can be estimated, the after-correction stage.
  corrected <- long_df %>%
    left_join_allow_many(corrections, by = c("Instrument", "Size")) %>%
    mutate(
      CM_value = dplyr::case_when(
        correction_model %in% c("TLS", "OLS") & is.finite(slope) & slope != 0 ~ (CM_raw - intercept) / slope,
        correction_model == "MEAN_RATIO" & is.finite(ratio_factor) & ratio_factor != 0 ~ CM_raw / ratio_factor,
        TRUE ~ NA_real_
      ),
      Stage = "After correction"
    ) %>%
    filter(!is.na(CM_value)) %>%
    select(names(raw))

  bind_rows(raw, corrected) %>%
    mutate(
      Stage = factor(Stage, levels = c("No correction", "After correction")),
      Stage = as.character(Stage)
    )
}

# ---- Neff and autocorrelation -------------------------------------------------
lag1_consecutive <- function(df, value_col = "RM_AVG") {
  n_observations <- nrow(df)

  if (!("date" %in% names(df))) {
    return(tibble::tibble(
      n_observations = n_observations,
      n_dates = 0L,
      rho1 = NA_real_,
      n_pairs = 0L,
      V = NA_real_,
      date_status = "date_missing"
    ))
  }

  if (!value_col %in% names(df)) {
    return(tibble::tibble(
      n_observations = n_observations,
      n_dates = 0L,
      rho1 = NA_real_,
      n_pairs = 0L,
      V = NA_real_,
      date_status = paste0("value_column_missing_", value_col)
    ))
  }

  parsed_date <- parse_date_flexible(df$date)
  if (all(is.na(parsed_date))) {
    return(tibble::tibble(
      n_observations = n_observations,
      n_dates = 0L,
      rho1 = NA_real_,
      n_pairs = 0L,
      V = NA_real_,
      date_status = "date_not_parsed"
    ))
  }
  df <- df %>% mutate(date = parsed_date)

  d0 <- df %>%
    transmute(date = date, y = suppressWarnings(as.numeric(.data[[value_col]]))) %>%
    filter(!is.na(date), is.finite(y)) %>%
    group_by(date) %>%
    summarise(y = mean(y, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)

  n_dates <- nrow(d0)

  d <- d0 %>%
    mutate(prev_date = lag(date), prev_y = lag(y), delta_days = as.numeric(date - prev_date)) %>%
    filter(delta_days == 1, is.finite(prev_y), is.finite(y))

  if (nrow(d) < 2) {
    return(tibble::tibble(
      n_observations = n_observations,
      n_dates = n_dates,
      rho1 = NA_real_,
      n_pairs = nrow(d),
      V = NA_real_,
      date_status = "no_or_too_few_consecutive_pairs"
    ))
  }

  rho <- suppressWarnings(cor(d$prev_y, d$y, use = "complete.obs"))
  rho <- ifelse(is.na(rho), NA_real_, max(min(rho, 0.95), -0.95))
  V <- ifelse(is.na(rho), NA_real_, (1 + rho) / (1 - rho))

  tibble::tibble(
    n_observations = n_observations,
    n_dates = n_dates,
    rho1 = rho,
    n_pairs = nrow(d),
    V = V,
    date_status = "ok"
  )
}

standardise_auxiliary_autocorrelation_data <- function(aux_file) {
  if (is.null(aux_file) || is.na(aux_file) || !nzchar(aux_file) || !file.exists(aux_file)) {
    return(tibble::tibble(Size = character(), Campaign = character(), date = as.Date(character()), aux_value = numeric()))
  }

  raw <- read_equivalence_input(aux_file)
  names(raw) <- trimws(names(raw))

  size_candidates <- c("Size", "Pollutant", "pollutant", "Size Fraction", "Size_Fraction", "size")
  campaign_candidates <- c("Campaign", "Site", "site", "Location", "location", "campaign")
  date_candidates <- c("date", "Date", "Start Date", "Start date", "Start Date and Time", "Start_Date", "start_date")
  value_candidates <- c("value", "Value", "concentration", "Concentration", "RM", "RM_AVG", "CM", "PM", "measurement")

  size_col <- intersect(size_candidates, names(raw))[1]
  campaign_col <- intersect(campaign_candidates, names(raw))[1]
  date_col <- intersect(date_candidates, names(raw))[1]
  value_col <- intersect(value_candidates, names(raw))[1]

  if (is.na(size_col) || is.na(campaign_col) || is.na(date_col) || is.na(value_col)) {
    stop(
      "Auxiliary autocorrelation file must contain columns for site, pollutant, date and value. ",
      "Accepted examples: Site/Pollutant/date/value or Campaign/Size/date/concentration."
    )
  }

  out <- raw %>%
    transmute(
      Size = as.character(.data[[size_col]]),
      Campaign = as.character(.data[[campaign_col]]),
      date = .data[[date_col]],
      aux_value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    mutate(
      Size = case_when(
        grepl("PM\\s*2[.,]?5|PM25", Size, ignore.case = TRUE) ~ "PM2.5",
        grepl("PM\\s*10", Size, ignore.case = TRUE) ~ "PM10",
        TRUE ~ Size
      )
    )

  out$date <- parse_date_flexible(out$date)

  out %>% filter(!is.na(Size), !is.na(Campaign), !is.na(date), is.finite(aux_value))
}

estimate_V_table <- function(df, config = default_config) {
  keys <- df %>%
    distinct(Instrument, Size, Campaign)

  rm_v <- df %>%
    group_by(Instrument, Size, Campaign) %>%
    group_modify(~ lag1_consecutive(.x, value_col = "RM_AVG")) %>%
    ungroup() %>%
    rename(
      rm_n_observations = n_observations,
      rm_n_dates = n_dates,
      rm_rho1 = rho1,
      rm_n_pairs = n_pairs,
      rm_V = V,
      rm_date_status = date_status
    )

  cm_cols <- intersect(c("CM1", "CM2"), names(df))

  cm_df <- df
  if (length(cm_cols) > 1) {
    cm_df <- cm_df %>%
      mutate(
        CM_AVG_for_V = rowMeans(across(all_of(cm_cols)), na.rm = TRUE),
        CM_AVG_for_V = ifelse(is.nan(CM_AVG_for_V), NA_real_, CM_AVG_for_V)
      )
  } else if (length(cm_cols) == 1) {
    cm_df <- cm_df %>%
      mutate(CM_AVG_for_V = suppressWarnings(as.numeric(.data[[cm_cols[1]]])))
  } else {
    cm_df <- cm_df %>%
      mutate(CM_AVG_for_V = NA_real_)
  }

  cm_v <- cm_df %>%
    group_by(Instrument, Size, Campaign) %>%
    group_modify(~ lag1_consecutive(.x, value_col = "CM_AVG_for_V")) %>%
    ungroup() %>%
    rename(
      cm_n_observations = n_observations,
      cm_n_dates = n_dates,
      cm_rho1 = rho1,
      cm_n_pairs = n_pairs,
      cm_V = V,
      cm_date_status = date_status
    )

  aux_file <- config$auxiliary_autocorrelation_file %||% NA_character_
  aux_v <- tibble::tibble(
    Size = character(),
    Campaign = character(),
    aux_n_observations = integer(),
    aux_n_dates = integer(),
    aux_rho1 = numeric(),
    aux_n_pairs = integer(),
    aux_V = numeric(),
    aux_date_status = character()
  )
  if (isTRUE(config$auxiliary_autocorrelation_allowed) &&
      !is.na(aux_file) && nzchar(aux_file) && file.exists(aux_file)) {
    aux_data <- standardise_auxiliary_autocorrelation_data(aux_file)
    if (nrow(aux_data) > 0) {
      aux_v <- aux_data %>%
        group_by(Size, Campaign) %>%
        group_modify(~ lag1_consecutive(.x, value_col = "aux_value")) %>%
        ungroup() %>%
        rename(
          aux_n_observations = n_observations,
          aux_n_dates = n_dates,
          aux_rho1 = rho1,
          aux_n_pairs = n_pairs,
          aux_V = V,
          aux_date_status = date_status
        )
    }
  }

  out <- keys %>%
    left_join(rm_v, by = c("Instrument", "Size", "Campaign")) %>%
    left_join(cm_v, by = c("Instrument", "Size", "Campaign")) %>%
    left_join(aux_v, by = c("Size", "Campaign")) %>%
    left_join(config$vrep_table, by = "Size") %>%
    mutate(
      rm_ok = is.finite(rm_V) & !is.na(rm_n_pairs) & rm_n_pairs >= config$min_consecutive_pairs_for_V,
      cm_ok = is.finite(cm_V) & !is.na(cm_n_pairs) & cm_n_pairs >= config$min_consecutive_pairs_for_V,
      aux_ok = is.finite(aux_V) & !is.na(aux_n_pairs) & aux_n_pairs >= config$auxiliary_min_consecutive_pairs_for_V,
      V_source = case_when(
        rm_ok ~ "equivalence_RMAVG",
        cm_ok ~ "equivalence_CM",
        aux_ok ~ "auxiliary_site_series",
        TRUE ~ "pollutant_representative_Vrep"
      ),
      n_observations = case_when(
        V_source == "equivalence_RMAVG" ~ rm_n_observations,
        V_source == "equivalence_CM" ~ cm_n_observations,
        V_source == "auxiliary_site_series" ~ aux_n_observations,
        TRUE ~ rm_n_observations
      ),
      n_dates = case_when(
        V_source == "equivalence_RMAVG" ~ rm_n_dates,
        V_source == "equivalence_CM" ~ cm_n_dates,
        V_source == "auxiliary_site_series" ~ aux_n_dates,
        TRUE ~ rm_n_dates
      ),
      rho1 = case_when(
        V_source == "equivalence_RMAVG" ~ rm_rho1,
        V_source == "equivalence_CM" ~ cm_rho1,
        V_source == "auxiliary_site_series" ~ aux_rho1,
        TRUE ~ NA_real_
      ),
      n_pairs = case_when(
        V_source == "equivalence_RMAVG" ~ rm_n_pairs,
        V_source == "equivalence_CM" ~ cm_n_pairs,
        V_source == "auxiliary_site_series" ~ aux_n_pairs,
        TRUE ~ rm_n_pairs
      ),
      V = case_when(
        V_source == "equivalence_RMAVG" ~ rm_V,
        V_source == "equivalence_CM" ~ cm_V,
        V_source == "auxiliary_site_series" ~ aux_V,
        TRUE ~ NA_real_
      ),
      date_status = case_when(
        V_source == "equivalence_RMAVG" ~ rm_date_status,
        V_source == "equivalence_CM" ~ cm_date_status,
        V_source == "auxiliary_site_series" ~ aux_date_status,
        TRUE ~ rm_date_status
      ),
      V_used = dplyr::case_when(
        V_source == "pollutant_representative_Vrep" ~ Vrep,
        is.finite(V) & V > 0 ~ V,
        TRUE ~ Vrep
      ),
      Neff_input = ifelse(is.finite(V_used) & V_used > 0, n_observations / V_used, NA_real_)
    ) %>%
    select(-rm_ok, -cm_ok, -aux_ok)

  out
}

# ---- Component calculations ---------------------------------------------------
calc_components <- function(df, bias_group_vars, ubsRM = 0) {
  # df must contain diff and the grouping variables used for bias blocks.
  if (nrow(df) == 0) {
    return(tibble::tibble(
      n = 0L, G = 0L, s_epsilon = NA_real_, scorr = NA_real_,
      bias_signed_abs = NA_real_, bias_mean_abs = NA_real_, ci95_abs = NA_real_,
      mean_RM = NA_real_, mean_CM = NA_real_, MBE = NA_real_, SD_diff = NA_real_, RMSE = NA_real_
    ))
  }

  bias_table <- df %>%
    group_by(across(all_of(bias_group_vars))) %>%
    summarise(n_block = n(), b_block = mean(diff, na.rm = TRUE), .groups = "drop") %>%
    mutate(w_block = n_block / sum(n_block))

  df_res <- df %>%
    left_join(bias_table %>% select(all_of(bias_group_vars), b_block), by = bias_group_vars) %>%
    mutate(epsilon = diff - b_block)

  n <- nrow(df_res)
  G <- nrow(bias_table)
  denom <- n - G
  s_epsilon <- if (denom > 0) sqrt(sum(df_res$epsilon^2, na.rm = TRUE) / denom) else NA_real_
  scorr <- if (is.finite(s_epsilon)) sqrt(max(s_epsilon^2 - (ubsRM %||% 0)^2, 0)) else NA_real_

  tibble::tibble(
    n = n,
    G = G,
    s_epsilon = s_epsilon,
    scorr = scorr,
    bias_signed_abs = abs(sum(bias_table$w_block * bias_table$b_block, na.rm = TRUE)),
    bias_mean_abs = sum(bias_table$w_block * abs(bias_table$b_block), na.rm = TRUE),
    bias_max_abs = max(abs(bias_table$b_block), na.rm = TRUE),
    ci95_abs = safe_quantile(abs(df$diff), 0.95),
    mean_RM = mean(df$RM_AVG, na.rm = TRUE),
    mean_CM = mean(df$CM_value, na.rm = TRUE),
    MBE = mean(df$diff, na.rm = TRUE),
    SD_diff = safe_sd(df$diff),
    RMSE = sqrt(mean(df$diff^2, na.rm = TRUE))
  )
}

get_dqo <- function(Size, lv_type, config = default_config) {
  v <- config$dqo_table %>%
    filter(.data$Size == !!Size, .data$LV_type == !!lv_type) %>%
    pull(DQO_percent)
  if (length(v) == 0) NA_real_ else as.numeric(v[[1]])
}


daily_metric_value <- function(metric, comp, LV, ubsRM = 0) {
  metric <- toupper(metric %||% "CI95")
  sd2 <- if (is.finite(comp$SD_diff)) comp$SD_diff^2 else NA_real_
  mbe2 <- if (is.finite(comp$MBE)) comp$MBE^2 else NA_real_

  dplyr::case_when(
    metric == "UDAILY" ~ 100 * (2 * comp$scorr + comp$bias_signed_abs) / LV,
    metric == "CI95" ~ 100 * comp$ci95_abs / LV,
    metric == "UTOT" & is.finite(sd2) & is.finite(mbe2) ~ 100 * 2 * sqrt(sd2 + mbe2) / LV,
    metric == "UDIFF" & is.finite(sd2) & is.finite(mbe2) ~ 100 * 2 * sqrt(max(sd2 - (ubsRM %||% 0)^2, 0) + mbe2) / LV,
    TRUE ~ NA_real_
  )
}

daily_metric_label <- function(config = default_config) {
  metric <- toupper(config$daily_decision_metric %||% "CI95")
  dplyr::case_when(
    metric == "UDAILY" ~ "Udaily (%LVd)",
    metric == "CI95" ~ "CI95 (%LVd)",
    metric == "UTOT" ~ "Utot (%LVd)",
    metric == "UDIFF" ~ "Udiff (%LVd)",
    TRUE ~ metric
  )
}


classify_n_daily <- function(n, config = default_config) {
  case_when(
    is.na(n) | n == 0 ~ "NO_DATA",
    n >= config$daily_n_ok ~ "OK",
    n >= config$daily_n_warning ~ "WARNING_LOW_N",
    n < config$daily_n_warning ~ "NOT_ASSESSABLE_DAILY",
    TRUE ~ "UNKNOWN"
  )
}

status_from_u <- function(u_rel, dqo, n_flag = "OK") {
  case_when(
    n_flag %in% c("NO_DATA", "NOT_ASSESSABLE_DAILY") ~ n_flag,

    # With low-N daily data, the metric is shown but it is not used to make
    # a PASS/FAIL daily decision. This avoids converting an unstable estimate
    # into a formal daily FAIL.
    n_flag == "WARNING_LOW_N" ~ "LOW_N",

    !is.finite(u_rel) | !is.finite(dqo) ~ "NOT_ASSESSABLE",
    u_rel <= dqo ~ "PASS",
    u_rel > dqo ~ "FAIL",
    TRUE ~ "UNKNOWN"
  )
}

calc_daily_unit <- function(stage_df, ubs_table, config = default_config) {
  stage_df %>%
    filter(RM_AVG >= config$daily_lv_window_lower_factor * LV_daily, RM_AVG <= config$daily_lv_window_upper_factor * LV_daily) %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
    group_modify(~ {
      ubs <- ubs_table %>%
        filter(Instrument == .y$Instrument, Size == .y$Size) %>%
        pull(ubsRM)
      ubs <- ubs[1] %||% 0
      comp <- calc_components(.x, bias_group_vars = c("Campaign"), ubsRM = ubs)
      LV <- unique(.x$LV_daily)[1]
      dqo <- get_dqo(.y$Size, "daily", config)
      # Daily LV assessment: the decision metric is the empirical CI95
      # of absolute CM-RM differences inside the daily LV window.
      # The decomposed daily metric is retained only as a comparison/diagnostic.
      u_abs <- 2 * comp$scorr + comp$bias_signed_abs
      u_rel <- 100 * u_abs / LV
      ci95_rel <- 100 * comp$ci95_abs / LV
      n_flag <- classify_n_daily(comp$n, config)
      tibble::tibble(
        LV_daily = LV,
        DQO_percent = dqo,
        n_LV = comp$n,
        n_flag = n_flag,
        mean_RM = comp$mean_RM,
        mean_CM = comp$mean_CM,
        MBE = comp$MBE,
        SD_diff = comp$SD_diff,
        RMSE = comp$RMSE,
        s_epsilon = comp$s_epsilon,
        scorr = comp$scorr,
        u_daily_random = comp$scorr,
        u_daily_bias = comp$bias_signed_abs,
        u_daily_bias_mean_abs_diagnostic = comp$bias_mean_abs,
        u_daily_bias_max_abs = comp$bias_max_abs,
        u_daily_abs = u_abs,
        u_daily_rel_LV_pct = u_rel,
        u_daily_quad_GDEstyle_abs = 2 * sqrt(comp$scorr^2 + comp$bias_signed_abs^2),
        u_daily_quad_expandedRandom_abs = sqrt((2 * comp$scorr)^2 + comp$bias_signed_abs^2),
        ci95_abs = comp$ci95_abs,
        ci95_rel_LV_pct = ci95_rel,
        daily_metric_rel_LV_pct = daily_metric_value(config$daily_decision_metric, comp, LV, ubsRM = ubs),
        daily_decision_metric = toupper(config$daily_decision_metric),
        daily_status = status_from_u(daily_metric_rel_LV_pct, dqo, n_flag)
      )
    }, .keep = TRUE) %>%
    ungroup()
}

calc_daily_site <- function(stage_df, ubs_table, config = default_config) {
  stage_df %>%
    filter(RM_AVG >= config$daily_lv_window_lower_factor * LV_daily, RM_AVG <= config$daily_lv_window_upper_factor * LV_daily) %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type, Site, Campaign) %>%
    group_modify(~ {
      ubs <- ubs_table %>%
        filter(Instrument == .y$Instrument, Size == .y$Size) %>%
        pull(ubsRM)
      ubs <- ubs[1] %||% 0
      comp <- calc_components(.x, bias_group_vars = c("Campaign"), ubsRM = ubs)
      LV <- unique(.x$LV_daily)[1]
      dqo <- get_dqo(.y$Size, "daily", config)
      u_abs <- 2 * comp$scorr + comp$bias_mean_abs
      u_rel <- 100 * u_abs / LV
      ci95_rel <- 100 * comp$ci95_abs / LV
      n_flag <- classify_n_daily(comp$n, config)
      tibble::tibble(
        LV_daily = LV,
        DQO_percent = dqo,
        n_LV = comp$n,
        n_flag = n_flag,
        mean_RM = comp$mean_RM,
        mean_CM = comp$mean_CM,
        MBE = comp$MBE,
        SD_diff = comp$SD_diff,
        scorr = comp$scorr,
        u_daily_random = comp$scorr,
        u_daily_bias = comp$bias_mean_abs,
        u_daily_abs = u_abs,
        u_daily_rel_LV_pct = u_rel,
        ci95_abs = comp$ci95_abs,
        ci95_rel_LV_pct = ci95_rel,
        daily_metric_rel_LV_pct = daily_metric_value(config$daily_decision_metric, comp, LV, ubsRM = ubs),
        daily_decision_metric = toupper(config$daily_decision_metric),
        daily_site_status = status_from_u(daily_metric_rel_LV_pct, dqo, n_flag)
      )
    }, .keep = TRUE) %>%
    ungroup()
}


calc_wams_rel_LV <- function(df, LV, ubsRM = 0) {
  df <- df %>% filter(is.finite(RM_AVG), is.finite(CM_value))
  n <- nrow(df)
  if (n < 3 || !is.finite(LV) || LV == 0) return(NA_real_)

  fit <- tls_fit(df$RM_AVG, df$CM_value)
  if (!is.finite(fit$slope) || !is.finite(fit$intercept)) return(NA_real_)

  yfit <- fit$intercept + fit$slope * df$RM_AVG
  rss <- sum((df$CM_value - yfit)^2, na.rm = TRUE)
  random <- sqrt(max(rss / (n - 2) - (ubsRM %||% 0)^2, 0))
  bias_LV <- fit$intercept + (fit$slope - 1) * LV

  100 * 2 * sqrt(random^2 + bias_LV^2) / LV
}

calc_daily_comparison_metrics <- function(stage_df, daily_unit, ubs_table, config = default_config) {
  # Annex table for daily LV metrics:
  # - WAMS full: historical full-range regression-based GDE-style metric.
  # - WAMS LV-window: sensitivity metric fitted only inside the daily LV window.
  # - CI95: empirical 95th percentile of |CM - RMAVG| inside the daily LV window.
  # - Utot/Udiff: difference-based indicators inside the daily LV window.
  base <- daily_unit %>%
    select(any_of(c(
      "Stage", "correction_model", "Instrument", "Size", "CM_type",
      "LV_daily", "DQO_percent", "n_LV", "u_daily_rel_LV_pct", "daily_status"
    )))

  lv_metrics <- stage_df %>%
    filter(RM_AVG >= config$daily_lv_window_lower_factor * LV_daily, RM_AVG <= config$daily_lv_window_upper_factor * LV_daily) %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
    group_modify(~ {
      ubs <- ubs_table %>%
        filter(Instrument == .y$Instrument, Size == .y$Size) %>%
        pull(ubsRM)
      ubs <- ubs[1] %||% 0

      LV <- unique(.x$LV_daily)[1]
      d <- .x$diff
      n <- length(d)
      mean_diff <- mean(d, na.rm = TRUE)
      var_diff <- stats::var(d, na.rm = TRUE)
      if (!is.finite(var_diff)) var_diff <- NA_real_

      tibble::tibble(
        Utot_rel_LV_pct = if (is.finite(var_diff) && is.finite(LV) && LV != 0) {
          100 * 2 * sqrt(var_diff + mean_diff^2) / LV
        } else NA_real_,
        Udiff_rel_LV_pct = if (is.finite(var_diff) && is.finite(LV) && LV != 0) {
          100 * 2 * sqrt(max(var_diff - (ubs %||% 0)^2, 0) + mean_diff^2) / LV
        } else NA_real_,
        CI95_rel_LV_pct = if (is.finite(LV) && LV != 0 && n > 0) {
          100 * as.numeric(stats::quantile(abs(d), 0.95, na.rm = TRUE, names = FALSE)) / LV
        } else NA_real_,
        WAMS_LV_window_rel_LV_pct = calc_wams_rel_LV(.x, LV = LV, ubsRM = ubs)
      )
    }) %>%
    ungroup()

  wams_full <- stage_df %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
    group_modify(~ {
      ubs <- ubs_table %>%
        filter(Instrument == .y$Instrument, Size == .y$Size) %>%
        pull(ubsRM)
      ubs <- ubs[1] %||% 0
      LV <- unique(.x$LV_daily)[1]
      tibble::tibble(WAMS_full_rel_LV_pct = calc_wams_rel_LV(.x, LV = LV, ubsRM = ubs))
    }) %>%
    ungroup()

  base %>%
    left_join(wams_full, by = c("Stage", "correction_model", "Instrument", "Size", "CM_type")) %>%
    left_join(lv_metrics, by = c("Stage", "correction_model", "Instrument", "Size", "CM_type")) %>%
    rename(CI95_rel_LV_pct = CI95_rel_LV_pct) %>%
    select(any_of(c(
      "Instrument", "Size", "CM_type", "Stage", "n_LV",
      "WAMS_full_rel_LV_pct", "WAMS_LV_window_rel_LV_pct",
      "CI95_rel_LV_pct", "Utot_rel_LV_pct", "Udiff_rel_LV_pct",
      "u_daily_rel_LV_pct", "daily_status"
    )))
}


calc_annual_unit <- function(stage_df, ubs_table, V_table, config = default_config) {
  stage_df %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
    group_modify(~ {
      ubs <- ubs_table %>%
        filter(Instrument == .y$Instrument, Size == .y$Size) %>%
        pull(ubsRM)
      ubs <- ubs[1] %||% 0
      comp <- calc_components(.x, bias_group_vars = c("Campaign"), ubsRM = ubs)
      LV <- unique(.x$LV_annual)[1]
      dqo <- get_dqo(.y$Size, "annual", config)

      n_by_campaign <- .x %>% dplyr::count(Instrument, Size, Campaign, name = "n_campaign")
      neff <- n_by_campaign %>%
        left_join(V_table %>% select(Instrument, Size, Campaign, V_used, V_source, rho1, n_pairs), by = c("Instrument", "Size", "Campaign")) %>%
        mutate(V_used = ifelse(is.finite(V_used) & V_used > 0, V_used, 1), Neff_campaign = n_campaign / V_used)
      Neff <- sum(neff$Neff_campaign, na.rm = TRUE)
      r_ann <- if (is.finite(comp$scorr) && is.finite(Neff) && Neff > 0) comp$scorr / sqrt(Neff) else NA_real_
      u_abs <- 2 * r_ann + comp$bias_signed_abs
      u_rel <- 100 * u_abs / LV
      tibble::tibble(
        LV_annual = LV,
        DQO_percent = dqo,
        n_annual = comp$n,
        Neff = Neff,
        mean_RM = comp$mean_RM,
        mean_CM = comp$mean_CM,
        MBE = comp$MBE,
        SD_diff = comp$SD_diff,
        s_epsilon = comp$s_epsilon,
        scorr = comp$scorr,
        u_annual_random = r_ann,
        u_annual_bias = comp$bias_signed_abs,
        u_annual_bias_mean_abs_diagnostic = comp$bias_mean_abs,
        u_annual_abs = u_abs,
        u_annual_rel_LV_pct = u_rel,
        u_annual_quad_GDEstyle_abs = 2 * sqrt(r_ann^2 + comp$bias_signed_abs^2),
        u_annual_quad_expandedRandom_abs = sqrt((2 * r_ann)^2 + comp$bias_signed_abs^2),
        annual_status = status_from_u(u_rel, dqo, "OK")
      )
    }, .keep = TRUE) %>%
    ungroup()
}

calc_annual_site <- function(stage_df, ubs_table, V_table, config = default_config) {
  stage_df %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type, Site) %>%
    group_modify(~ {
      ubs <- ubs_table %>%
        filter(Instrument == .y$Instrument, Size == .y$Size) %>%
        pull(ubsRM)
      ubs <- ubs[1] %||% 0
      comp <- calc_components(.x, bias_group_vars = c("Campaign"), ubsRM = ubs)
      LV <- unique(.x$LV_annual)[1]
      dqo <- get_dqo(.y$Size, "annual", config)
      n_by_campaign <- .x %>% dplyr::count(Instrument, Size, Campaign, name = "n_campaign")
      neff <- n_by_campaign %>%
        left_join(V_table %>% select(Instrument, Size, Campaign, V_used, V_source), by = c("Instrument", "Size", "Campaign")) %>%
        mutate(V_used = ifelse(is.finite(V_used) & V_used > 0, V_used, 1), Neff_campaign = n_campaign / V_used)
      Neff <- sum(neff$Neff_campaign, na.rm = TRUE)
      r_ann <- if (is.finite(comp$scorr) && is.finite(Neff) && Neff > 0) comp$scorr / sqrt(Neff) else NA_real_
      u_abs <- 2 * r_ann + comp$bias_signed_abs
      u_rel <- 100 * u_abs / LV
      tibble::tibble(
        LV_annual = LV,
        DQO_percent = dqo,
        n_annual = comp$n,
        Neff = Neff,
        mean_RM = comp$mean_RM,
        mean_CM = comp$mean_CM,
        MBE = comp$MBE,
        SD_diff = comp$SD_diff,
        scorr = comp$scorr,
        u_annual_random = r_ann,
        u_annual_bias = comp$bias_signed_abs,
        u_annual_abs = u_abs,
        u_annual_rel_LV_pct = u_rel,
        annual_site_status = status_from_u(u_rel, dqo, "OK")
      )
    }, .keep = TRUE) %>%
    ungroup()
}

# ---- Diagnostics --------------------------------------------------------------
calc_linearity_diagnostics <- function(stage_df, scenario = c("type_testing", "ongoing_verification"), config = default_config) {
  scenario <- match.arg(scenario)
  r2_thr <- if (scenario == "type_testing") config$type_testing_r2_threshold else config$ongoing_r2_threshold

  stage_df %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
    group_modify(~ {
      d_total <- .x %>% filter(is.finite(RM_AVG), is.finite(CM_value))
      d <- filter_tls_fit_range(.x, config, value_col = "CM_value")
      n_total <- nrow(d_total)
      n_excluded <- max(n_total - nrow(d), 0)

      if (nrow(d) < 3) {
        return(tibble::tibble(
          n = nrow(d),
          n_linearity_total = n_total,
          n_linearity_excluded = n_excluded,
          tls_fit_range = tls_fit_range_label(config),
          R2 = NA_real_,
          residual_SD = NA_real_,
          R2_threshold = r2_thr,
          linearity_status = "NOT_ASSESSABLE"
        ))
      }

      fit <- lm(CM_value ~ RM_AVG, data = d)
      r2 <- summary(fit)$r.squared
      rsd <- sd(residuals(fit), na.rm = TRUE)
      status <- ifelse(is.finite(r2) && round(r2, 2) >= round(r2_thr, 2), "PASS", "FAIL")

      tibble::tibble(
        n = nrow(d),
        n_linearity_total = n_total,
        n_linearity_excluded = n_excluded,
        tls_fit_range = tls_fit_range_label(config),
        R2 = r2,
        residual_SD = rsd,
        R2_threshold = r2_thr,
        linearity_status = status
      )
    }, .keep = TRUE) %>%
    ungroup()
}

calc_difference_diagnostics <- function(stage_df, config = default_config) {
  stage_df %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
    summarise(
      n = n(),
      MBE = mean(diff, na.rm = TRUE),
      SD_diff = safe_sd(diff),
      RMSE = sqrt(mean(diff^2, na.rm = TRUE)),
      CI95_abs = safe_quantile(abs(diff), 0.95),
      skewness = safe_skewness(diff),
      kurtosis = safe_kurtosis(diff),
      MBE_threshold = config$diagnostic_mbe_threshold,
      SD_threshold = config$diagnostic_sd_threshold,
      skewness_threshold = config$diagnostic_skewness_threshold,
      kurtosis_threshold = config$diagnostic_kurtosis_threshold,
      MBE_flag = ifelse(is.finite(MBE) & abs(MBE) <= config$diagnostic_mbe_threshold, "OK", "BIAS_WARNING"),
      SD_flag = ifelse(is.finite(SD_diff) & SD_diff <= config$diagnostic_sd_threshold, "OK", "NOISE_WARNING"),
      skewness_flag = ifelse(is.finite(skewness) & abs(skewness) <= config$diagnostic_skewness_threshold, "OK", "SKEWNESS_WARNING"),
      kurtosis_flag = ifelse(is.finite(kurtosis) & kurtosis <= config$diagnostic_kurtosis_threshold, "OK", "KURTOSIS_WARNING"),
      diagnostic_status = ifelse(MBE_flag == "OK" & SD_flag == "OK" & skewness_flag == "OK" & kurtosis_flag == "OK", "OK", "DIAGNOSTIC_WARNING"),
      .groups = "drop"
    )
}

calc_difference_site_diagnostics <- function(stage_df, config = default_config) {
  stage_df %>%
    mutate(diff = CM_value - RM_AVG) %>%
    group_by(Stage, correction_model, Instrument, Size, CM_type, Site, Campaign) %>%
    summarise(
      n = n(),
      MBE = mean(diff, na.rm = TRUE),
      SD_diff = safe_sd(diff),
      RMSE = sqrt(mean(diff^2, na.rm = TRUE)),
      CI95_abs = safe_quantile(abs(diff), 0.95),
      skewness = safe_skewness(diff),
      kurtosis = safe_kurtosis(diff),
      MBE_threshold = config$diagnostic_mbe_threshold,
      SD_threshold = config$diagnostic_sd_threshold,
      skewness_threshold = config$diagnostic_skewness_threshold,
      kurtosis_threshold = config$diagnostic_kurtosis_threshold,
      MBE_flag = ifelse(is.finite(MBE) & abs(MBE) <= config$diagnostic_mbe_threshold, "OK", "BIAS_WARNING"),
      SD_flag = ifelse(is.finite(SD_diff) & SD_diff <= config$diagnostic_sd_threshold, "OK", "NOISE_WARNING"),
      skewness_flag = ifelse(is.finite(skewness) & abs(skewness) <= config$diagnostic_skewness_threshold, "OK", "SKEWNESS_WARNING"),
      kurtosis_flag = ifelse(is.finite(kurtosis) & kurtosis <= config$diagnostic_kurtosis_threshold, "OK", "KURTOSIS_WARNING"),
      diagnostic_status = ifelse(MBE_flag == "OK" & SD_flag == "OK" & skewness_flag == "OK" & kurtosis_flag == "OK", "OK", "DIAGNOSTIC_WARNING"),
      .groups = "drop"
    )
}


complete_daily_unit_results <- function(daily_unit, stage_df, config = default_config) {
  grid <- stage_df %>%
    distinct(Stage, correction_model, Instrument, Size, CM_type, LV_daily) %>%
    mutate(DQO_percent = vapply(Size, function(z) get_dqo(z, "daily", config), numeric(1)))

  grid %>%
    left_join(daily_unit, by = c("Stage", "correction_model", "Instrument", "Size", "CM_type", "LV_daily", "DQO_percent")) %>%
    mutate(
      n_LV = replace_na(n_LV, 0L),
      n_flag = ifelse(is.na(n_flag), "NO_DATA", n_flag),
      daily_status = ifelse(is.na(daily_status), "NO_DATA", daily_status)
    )
}

complete_annual_unit_results <- function(annual_unit, stage_df, config = default_config) {
  grid <- stage_df %>%
    distinct(Stage, correction_model, Instrument, Size, CM_type, LV_annual) %>%
    mutate(DQO_percent = vapply(Size, function(z) get_dqo(z, "annual", config), numeric(1)))

  grid %>%
    left_join(annual_unit, by = c("Stage", "correction_model", "Instrument", "Size", "CM_type", "LV_annual", "DQO_percent")) %>%
    mutate(
      n_annual = replace_na(n_annual, 0L),
      annual_status = ifelse(is.na(annual_status), "NOT_ASSESSABLE", annual_status)
    )
}

# ---- Decision logic -----------------------------------------------------------

weighted_mean_nonmissing <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
}

combine_site_daily_annual <- function(daily_site, annual_site) {
  key_cols <- c("Stage", "correction_model", "Instrument", "Size", "CM_type", "Site")

  is_daily_low_n <- function(x) {
    x <- as.character(x)
    grepl("LOW_N|LOW N|WARNING_LOW_N|WARNING", x, ignore.case = TRUE)
  }

  is_not_assessable <- function(x) {
    x <- as.character(x)
    x %in% c("NO_DATA", "NOT_ASSESSABLE_DAILY", "NOT_ASSESSABLE_ANNUAL",
             "NOT_ASSESSABLE", "NOT ASSESSABLE")
  }

  daily_site_summary <- if (!is.null(daily_site) && nrow(daily_site) > 0) {
    daily_site %>%
      group_by(across(all_of(key_cols))) %>%
      summarise(
        n_daily_campaigns = n_distinct(Campaign),
        n_LV_total = sum(n_LV, na.rm = TRUE),
        mean_RM_daily = weighted_mean_nonmissing(mean_RM, n_LV),
        mean_CM_daily = weighted_mean_nonmissing(mean_CM, n_LV),
        daily_decision_metric = dplyr::first(stats::na.omit(daily_decision_metric)),
        daily_metric_rel_LV_pct = weighted_mean_nonmissing(daily_metric_rel_LV_pct, n_LV),
        u_daily_rel_LV_pct = weighted_mean_nonmissing(u_daily_rel_LV_pct, n_LV),
        worst_daily_rel_LV_pct = suppressWarnings(max(daily_metric_rel_LV_pct, na.rm = TRUE)),
        daily_assessment_status = case_when(
          any(daily_site_status == "FAIL", na.rm = TRUE) ~ "FAIL",
          any(daily_site_status == "PASS", na.rm = TRUE) &
            any(is_daily_low_n(daily_site_status), na.rm = TRUE) ~ "LOW_N",
          any(daily_site_status == "PASS", na.rm = TRUE) ~ "PASS",
          any(is_daily_low_n(daily_site_status), na.rm = TRUE) ~ "LOW_N",
          any(is_not_assessable(daily_site_status), na.rm = TRUE) ~ "NOT_ASSESSABLE_DAILY",
          TRUE ~ "NOT_ASSESSABLE_DAILY"
        ),
        .groups = "drop"
      ) %>%
      mutate(
        worst_daily_rel_LV_pct = ifelse(is.infinite(worst_daily_rel_LV_pct), NA_real_, worst_daily_rel_LV_pct)
      )
  } else {
    tibble::tibble(
      Stage = character(), correction_model = character(), Instrument = character(), Size = character(),
      CM_type = character(), Site = character(), n_daily_campaigns = integer(), n_LV_total = integer(),
      mean_RM_daily = numeric(), mean_CM_daily = numeric(), daily_decision_metric = character(),
      daily_metric_rel_LV_pct = numeric(), u_daily_rel_LV_pct = numeric(),
      worst_daily_rel_LV_pct = numeric(), daily_assessment_status = character()
    )
  }

  annual_site_summary <- if (!is.null(annual_site) && nrow(annual_site) > 0) {
    annual_site %>%
      transmute(
        Stage, correction_model, Instrument, Size, CM_type, Site,
        n_annual,
        Neff,
        mean_RM_annual = mean_RM,
        mean_CM_annual = mean_CM,
        u_annual_rel_LV_pct,
        annual_assessment_status = annual_site_status
      )
  } else {
    tibble::tibble(
      Stage = character(), correction_model = character(), Instrument = character(), Size = character(),
      CM_type = character(), Site = character(), n_annual = integer(), Neff = numeric(),
      mean_RM_annual = numeric(), mean_CM_annual = numeric(), u_annual_rel_LV_pct = numeric(),
      annual_assessment_status = character()
    )
  }

  full_join(daily_site_summary, annual_site_summary, by = key_cols) %>%
    mutate(
      daily_assessment_status = ifelse(is.na(daily_assessment_status), "NOT_ASSESSABLE_DAILY", daily_assessment_status),
      annual_assessment_status = ifelse(is.na(annual_assessment_status), "NOT_ASSESSABLE_ANNUAL", annual_assessment_status),

      final_site_status = case_when(
        # Annual fail dominates the annual fallback logic.
        annual_assessment_status == "FAIL" ~ "FAIL",

        # A true daily failure is still a failure.
        daily_assessment_status == "FAIL" ~ "FAIL",

        # Daily PASS, LOW N, or NOT ASSESSABLE can all lead to PASS when annual passes.
        annual_assessment_status == "PASS" &
          daily_assessment_status %in% c("PASS", "LOW_N", "LOW N",
                                         "NOT_ASSESSABLE_DAILY", "NO_DATA",
                                         "NOT_ASSESSABLE", "NOT ASSESSABLE") ~ "PASS",

        annual_assessment_status %in% c("NOT_ASSESSABLE_ANNUAL", "NOT_ASSESSABLE", "NOT ASSESSABLE") ~ "NOT_ASSESSABLE",
        TRUE ~ "NOT_ASSESSABLE"
      ),

      decision_basis = case_when(
        annual_assessment_status == "FAIL" ~ "Y fail",
        daily_assessment_status == "FAIL" ~ "D fail",
        annual_assessment_status == "PASS" & daily_assessment_status == "PASS" ~ "D+Y pass",
        annual_assessment_status == "PASS" & daily_assessment_status %in% c("LOW_N", "LOW N") ~ "LOW N + Y pass",
        annual_assessment_status == "PASS" &
          daily_assessment_status %in% c("NOT_ASSESSABLE_DAILY", "NO_DATA", "NOT_ASSESSABLE", "NOT ASSESSABLE") ~ "Y fallback",
        TRUE ~ "Not assessable"
      )
    ) %>%
    arrange(Stage, correction_model, Instrument, Size, CM_type, Site)
}


combine_method_status <- function(daily_unit,
                                  daily_site,
                                  annual_unit,
                                  annual_site = NULL,
                                  scenario = c("type_testing", "ongoing_verification")) {
  scenario <- match.arg(scenario)

  is_pass <- function(x) x == "PASS"
  is_fail <- function(x) x == "FAIL"
  is_low_n <- function(x) grepl("WARNING|LOW_N|LOW N", x, ignore.case = TRUE)
  is_not_assessable <- function(x) x %in% c("NO_DATA", "NOT_ASSESSABLE_DAILY", "NOT_ASSESSABLE_ANNUAL", "NOT_ASSESSABLE", "NOT ASSESSABLE")

  # Daily result is evaluated at CM-unit level first.
  # Site/campaign LOW N does not make a CM fail when at least one site/campaign
  # provides a PASS and no site/campaign FAIL is present. If there are two CM
  # units, both must satisfy this CM-level rule for the method-level daily result
  # to be PASS.
  daily_site_cm_guard <- if (!is.null(daily_site) && nrow(daily_site) > 0) {
    daily_site %>%
      group_by(Stage, correction_model, Instrument, Size, CM_type) %>%
      summarise(
        any_cm_site_daily_fail = any(is_fail(daily_site_status), na.rm = TRUE),
        n_cm_site_daily_fails = sum(is_fail(daily_site_status), na.rm = TRUE),
        any_cm_site_daily_pass = any(is_pass(daily_site_status), na.rm = TRUE),
        any_cm_site_daily_warning = any(is_low_n(daily_site_status), na.rm = TRUE),
        any_cm_site_daily_not_assessable = any(is_not_assessable(daily_site_status), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      Stage = character(), correction_model = character(), Instrument = character(), Size = character(), CM_type = character(),
      any_cm_site_daily_fail = logical(), n_cm_site_daily_fails = integer(),
      any_cm_site_daily_pass = logical(), any_cm_site_daily_warning = logical(), any_cm_site_daily_not_assessable = logical()
    )
  }

  daily_cm <- daily_unit %>%
    left_join(daily_site_cm_guard, by = c("Stage", "correction_model", "Instrument", "Size", "CM_type")) %>%
    mutate(
      across(any_of(c(
        "any_cm_site_daily_fail", "any_cm_site_daily_pass",
        "any_cm_site_daily_warning", "any_cm_site_daily_not_assessable"
      )), ~ replace_na(.x, FALSE)),
      across(any_of(c("n_cm_site_daily_fails")), ~ replace_na(.x, 0L)),
      cm_daily_status = case_when(
        is_fail(daily_status) | any_cm_site_daily_fail ~ "FAIL",
        is_pass(daily_status) | any_cm_site_daily_pass ~ "PASS",
        is_low_n(daily_status) | any_cm_site_daily_warning ~ "LOW N",
        is_not_assessable(daily_status) | any_cm_site_daily_not_assessable ~ "NOT ASSESSABLE",
        TRUE ~ "NOT ASSESSABLE"
      )
    )

  daily_method <- daily_cm %>%
    group_by(Stage, correction_model, Instrument, Size) %>%
    summarise(
      n_CM_units = n_distinct(CM_type),
      n_CM_pass = sum(cm_daily_status == "PASS", na.rm = TRUE),
      n_CM_fail = sum(cm_daily_status == "FAIL", na.rm = TRUE),
      n_CM_low_n = sum(cm_daily_status == "LOW N", na.rm = TRUE),
      n_CM_not_assessable = sum(cm_daily_status == "NOT ASSESSABLE", na.rm = TRUE),
      any_global_daily_fail = any(is_fail(daily_status), na.rm = TRUE),
      any_global_daily_warning = any(is_low_n(daily_status), na.rm = TRUE),
      any_global_daily_not_assessable = any(is_not_assessable(daily_status), na.rm = TRUE),
      .groups = "drop"
    )

  daily_site_guard <- if (nrow(daily_site_cm_guard) > 0) {
    daily_site_cm_guard %>%
      group_by(Stage, correction_model, Instrument, Size) %>%
      summarise(
        any_site_daily_fail = any(any_cm_site_daily_fail, na.rm = TRUE),
        n_site_daily_fails = sum(n_cm_site_daily_fails, na.rm = TRUE),
        any_site_daily_pass = any(any_cm_site_daily_pass, na.rm = TRUE),
        any_site_daily_warning = any(any_cm_site_daily_warning, na.rm = TRUE),
        any_site_daily_not_assessable = any(any_cm_site_daily_not_assessable, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      Stage = character(), correction_model = character(), Instrument = character(), Size = character(),
      any_site_daily_fail = logical(), n_site_daily_fails = integer(), any_site_daily_pass = logical(),
      any_site_daily_warning = logical(), any_site_daily_not_assessable = logical()
    )
  }

  annual_method <- annual_unit %>%
    group_by(Stage, correction_model, Instrument, Size) %>%
    summarise(
      n_CM_units_annual = n_distinct(CM_type),
      n_annual_CM_pass = sum(is_pass(annual_status), na.rm = TRUE),
      n_annual_CM_fail = sum(is_fail(annual_status), na.rm = TRUE),
      n_annual_CM_not_assessable = sum(is_not_assessable(annual_status), na.rm = TRUE),
      any_global_annual_fail = any(is_fail(annual_status), na.rm = TRUE),
      any_global_annual_not_assessable = any(is_not_assessable(annual_status), na.rm = TRUE),
      .groups = "drop"
    )

  annual_site_guard <- if (!is.null(annual_site) && nrow(annual_site) > 0) {
    annual_site %>%
      group_by(Stage, correction_model, Instrument, Size) %>%
      summarise(
        any_site_annual_fail = any(is_fail(annual_site_status), na.rm = TRUE),
        n_site_annual_fails = sum(is_fail(annual_site_status), na.rm = TRUE),
        any_site_annual_not_assessable = any(is_not_assessable(annual_site_status), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      Stage = character(), correction_model = character(), Instrument = character(), Size = character(),
      any_site_annual_fail = logical(), n_site_annual_fails = integer(),
      any_site_annual_not_assessable = logical()
    )
  }

  out <- full_join(daily_method, daily_site_guard, by = c("Stage", "correction_model", "Instrument", "Size")) %>%
    full_join(annual_method, by = c("Stage", "correction_model", "Instrument", "Size")) %>%
    full_join(annual_site_guard, by = c("Stage", "correction_model", "Instrument", "Size")) %>%
    mutate(
      across(any_of(c(
        "any_global_daily_fail", "any_global_daily_warning", "any_global_daily_not_assessable",
        "any_site_daily_fail", "any_site_daily_pass", "any_site_daily_warning", "any_site_daily_not_assessable",
        "any_global_annual_fail", "any_global_annual_not_assessable",
        "any_site_annual_fail", "any_site_annual_not_assessable"
      )), ~ replace_na(.x, FALSE)),
      across(any_of(c(
        "n_CM_units", "n_CM_pass", "n_CM_fail", "n_CM_low_n", "n_CM_not_assessable",
        "n_CM_units_annual", "n_annual_CM_pass", "n_annual_CM_fail", "n_annual_CM_not_assessable",
        "n_site_daily_fails", "n_site_annual_fails"
      )), ~ replace_na(.x, 0L)),

      n_site_fails = n_site_daily_fails + n_site_annual_fails,

      # Daily result:
      # - any global or site daily FAIL -> FAIL
      # - all CM units must pass the CM-level daily rule -> PASS
      # - for a CM unit, at least one PASS site/campaign is sufficient when the
      #   remaining site/campaign results are LOW N and no FAIL is present
      # - if at least one CM unit is LOW N / NOT ASSESSABLE and not all CM units
      #   pass, the method-level daily result remains LOW N / NOT ASSESSABLE
      daily_method_status = case_when(
        any_global_daily_fail | any_site_daily_fail | n_CM_fail > 0 ~ "FAIL",
        n_CM_units > 0 & n_CM_pass == n_CM_units ~ "PASS",
        n_CM_low_n > 0 ~ "LOW N",
        n_CM_not_assessable > 0 ~ "NOT ASSESSABLE",
        any_global_daily_warning | any_site_daily_warning ~ "LOW N",
        any_global_daily_not_assessable | any_site_daily_not_assessable ~ "NOT ASSESSABLE",
        TRUE ~ "NOT ASSESSABLE"
      ),

      # Annual result:
      # - annual FAIL dominates
      # - all CM units must pass annual, and no site annual failure may be present
      annual_method_status = case_when(
        any_global_annual_fail | any_site_annual_fail ~ "FAIL",
        n_CM_units_annual > 0 & n_annual_CM_pass == n_CM_units_annual ~ "PASS",
        any_global_annual_not_assessable | any_site_annual_not_assessable ~ "NOT ASSESSABLE",
        TRUE ~ "NOT ASSESSABLE"
      ),

      # Final result:
      # - annual FAIL -> FAIL
      # - daily FAIL -> FAIL
      # - daily PASS/LOW N/NOT ASSESSABLE + annual PASS -> PASS
      # - annual NOT ASSESSABLE without daily PASS -> NOT ASSESSABLE
      final_method_status = case_when(
        annual_method_status == "FAIL" ~ "FAIL",
        daily_method_status == "FAIL" ~ "FAIL",
        annual_method_status == "PASS" & daily_method_status %in% c("PASS", "LOW N", "LOW_N", "NOT ASSESSABLE", "NOT_ASSESSABLE", "NO_DATA") ~ "PASS",
        daily_method_status == "PASS" & annual_method_status == "PASS" ~ "PASS",
        TRUE ~ "NOT ASSESSABLE"
      ),

      main_issue = {
        issues <- purrr::pmap_chr(
          list(
            final_method_status,
            any_global_daily_fail, any_global_annual_fail,
            any_site_daily_fail, any_site_annual_fail,
            n_site_daily_fails, n_site_annual_fails,
            daily_method_status, annual_method_status
          ),
          function(final, gd, gy, sd, sy, nsd, nsy, ds, ys) {
            x <- character(0)

            if (identical(final, "PASS")) {
              if (ds %in% c("LOW N", "LOW_N") && identical(ys, "PASS")) return("LOW N + annual pass")
              if (ds %in% c("NOT ASSESSABLE", "NOT_ASSESSABLE", "NO_DATA") && identical(ys, "PASS")) return("annual fallback")
              return("none")
            }

            if (identical(final, "FAIL")) {
              if (isTRUE(gd)) x <- c(x, "global daily")
              if (isTRUE(gy)) x <- c(x, "global annual")
              if (isTRUE(sd)) x <- c(x, paste0("site daily (n=", nsd, ")"))
              if (isTRUE(sy)) x <- c(x, paste0("site annual (n=", nsy, ")"))
              if (length(x) == 0 && identical(ys, "FAIL")) x <- c(x, "annual fail")
              if (length(x) == 0 && identical(ds, "FAIL")) x <- c(x, "daily fail")
              if (length(x) == 0) x <- "unspecified fail"
              return(paste(x, collapse = "; "))
            }

            if (ds %in% c("LOW N", "LOW_N")) return("LOW N")
            if (ds %in% c("NOT ASSESSABLE", "NOT_ASSESSABLE", "NO_DATA") && identical(ys, "PASS")) return("annual fallback")
            if (identical(ys, "NOT ASSESSABLE")) return("annual not assessable")
            "not assessable"
          }
        )
        issues
      },
      .after = final_method_status
    )

  out
}

# ---- Output helpers -----------------------------------------------------------
status_columns <- function(df) {
  grep("status|flag|pass", names(df), ignore.case = TRUE, value = TRUE)
}

sort_output_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }
  if ("Stage" %in% names(df)) {
    df <- df %>%
      mutate(Stage = factor(Stage, levels = c("No correction", "After correction")))
  }
  order_cols <- intersect(c("Instrument", "Size", "CM_type", "Site", "Campaign", "Stage", "correction_model"), names(df))
  if (length(order_cols) == 0) {
    return(df)
  }
  df %>%
    arrange(across(all_of(order_cols))) %>%
    mutate(across(any_of("Stage"), as.character))
}

round_output_numbers <- function(df, digits = 1) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }
  count_cols <- intersect(c("n", "n_LV", "n_annual", "n_rows", "n_RM_duplicates", "n_RM_excluded", "n_regression", "n_regression_total", "n_regression_excluded", "n_linearity_total", "n_linearity_excluded", "n_pairs", "G"), names(df))
  df %>%
    mutate(across(where(is.numeric), ~ round(.x, digits))) %>%
    mutate(across(all_of(count_cols), ~ ifelse(is.na(.x), NA, as.integer(round(.x, 0)))))
}

prepare_output_table <- function(df, digits = 1) {
  df %>%
    sort_output_table() %>%
    round_output_numbers(digits = digits)
}

write_excel_tables <- function(file_name, tables) {
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", border = "bottom")
  body_style <- openxlsx::createStyle(halign = "center", valign = "center")
  num_style <- openxlsx::createStyle(numFmt = "0.0", halign = "center", valign = "center")
  pass_style <- openxlsx::createStyle(fontColour = "#008000", textDecoration = "bold", halign = "center", valign = "center")
  fail_style <- openxlsx::createStyle(fontColour = "#C00000", textDecoration = "bold", halign = "center", valign = "center")
  warn_style <- openxlsx::createStyle(fontColour = "#C08000", textDecoration = "bold", halign = "center", valign = "center")
  diag_exceed_style <- openxlsx::createStyle(fgFill = "#F4B183", textDecoration = "bold", halign = "center", valign = "center")

  add_exceed_style <- function(sheet_name, df, metric, rows) {
    if (metric %in% names(df) && length(rows) > 0) {
      openxlsx::addStyle(
        wb, sheet_name, diag_exceed_style,
        rows = rows + 1, cols = which(names(df) == metric),
        gridExpand = TRUE, stack = TRUE
      )
    }
  }

  for (nm in names(tables)) {
    df <- prepare_output_table(tables[[nm]], digits = 1)
    sh <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, df, headerStyle = header_style, withFilter = TRUE)
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
    if (ncol(df) > 0 && nrow(df) > 0) {
      openxlsx::addStyle(wb, sh, body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
      num_cols <- which(vapply(df, is.numeric, logical(1)))
      if (length(num_cols) > 0) openxlsx::addStyle(wb, sh, num_style, rows = 2:(nrow(df) + 1), cols = num_cols, gridExpand = TRUE, stack = TRUE)
      for (sc in status_columns(df)) {
        col_idx <- which(names(df) == sc)
        vals <- as.character(df[[sc]])
        pass_rows <- which(grepl("PASS", vals, ignore.case = TRUE)) + 1
        fail_rows <- which(grepl("FAIL", vals, ignore.case = TRUE)) + 1
        warn_rows <- which(grepl("WARNING|NOT_ASSESSABLE|NO_DATA|LOW_N", vals, ignore.case = TRUE)) + 1
        if (length(pass_rows) > 0) openxlsx::addStyle(wb, sh, pass_style, rows = pass_rows, cols = col_idx, gridExpand = TRUE, stack = TRUE)
        if (length(fail_rows) > 0) openxlsx::addStyle(wb, sh, fail_style, rows = fail_rows, cols = col_idx, gridExpand = TRUE, stack = TRUE)
        if (length(warn_rows) > 0) openxlsx::addStyle(wb, sh, warn_style, rows = warn_rows, cols = col_idx, gridExpand = TRUE, stack = TRUE)
      }
      if (grepl("Difference_diagnostics", nm, ignore.case = TRUE)) {
        if (all(c("MBE", "MBE_threshold") %in% names(df))) {
          add_exceed_style(sh, df, "MBE", which(is.finite(df$MBE) & abs(df$MBE) > df$MBE_threshold))
        }
        if (all(c("SD_diff", "SD_threshold") %in% names(df))) {
          add_exceed_style(sh, df, "SD_diff", which(is.finite(df$SD_diff) & df$SD_diff > df$SD_threshold))
        }
        if (all(c("skewness", "skewness_threshold") %in% names(df))) {
          add_exceed_style(sh, df, "skewness", which(is.finite(df$skewness) & abs(df$skewness) > df$skewness_threshold))
        }
        if (all(c("kurtosis", "kurtosis_threshold") %in% names(df))) {
          add_exceed_style(sh, df, "kurtosis", which(is.finite(df$kurtosis) & df$kurtosis > df$kurtosis_threshold))
        }
        if (all(c("Ubs_CM", "Ubs_CM_threshold") %in% names(df))) {
          add_exceed_style(sh, df, "Ubs_CM", which(is.finite(df$Ubs_CM) & df$Ubs_CM > df$Ubs_CM_threshold))
        }
      }

      if (grepl("Linearity_diagnostics", nm, ignore.case = TRUE)) {
        if (all(c("R2", "R2_threshold") %in% names(df))) {
          add_exceed_style(sh, df, "R2", which(is.finite(df$R2) & df$R2 < df$R2_threshold))
        }
      }

      openxlsx::setColWidths(wb, sh, cols = seq_len(ncol(df)), widths = "auto")
    }
  }
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
}

make_rm_diagnostics <- function(df, config = default_config) {
  if (!all(c("RM1", "RM2") %in% names(df))) {
    return(tibble::tibble())
  }
  df %>%
    mutate(
      RM_avg_for_diag = rowMeans(across(any_of(c("RM1", "RM2"))), na.rm = TRUE),
      RM_avg_for_diag = ifelse(is.nan(RM_avg_for_diag), NA_real_, RM_avg_for_diag),
      RM_diff = RM1 - RM2,
      RM_abs_diff = abs(RM_diff),
      RM_excluded = !is.na(RM_abs_diff) & RM_abs_diff > config$rm_duplicate_abs_threshold
    ) %>%
    group_by(Country, Instrument, Size, Campaign) %>%
    summarise(
      n_rows = n(),
      n_RM_duplicates = sum(!is.na(RM1) & !is.na(RM2)),
      n_RM_excluded = sum(RM_excluded, na.rm = TRUE),
      pct_RM_excluded = ifelse(n_RM_duplicates > 0, 100 * n_RM_excluded / n_RM_duplicates, NA_real_),
      RM_diff_mean = mean(RM_diff, na.rm = TRUE),
      RM_diff_SD = safe_sd(RM_diff),
      RM_abs_diff_mean = mean(RM_abs_diff, na.rm = TRUE),
      RM_abs_diff_CI95 = safe_quantile(RM_abs_diff, 0.95),
      RM_diff_skewness = safe_skewness(RM_diff),
      RM_diff_kurtosis = safe_kurtosis(RM_diff),
      .groups = "drop"
    )
}

make_plot_placeholder <- function(title, message = "No data available for this plot") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 5) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void()
}

save_plot_or_placeholder <- function(data, plot_expr, file, title, width = 9, height = 6, dpi = 150) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (is.null(data) || nrow(data) == 0) {
    p <- make_plot_placeholder(title, "No finite data available")
  } else {
    p <- force(plot_expr)
  }
  ggplot2::ggsave(file, p, width = width, height = height, dpi = dpi)
  invisible(file)
}

make_rm_diagnostics <- function(df, config = default_config) {
  if (!all(c("RM1", "RM2") %in% names(df))) {
    return(tibble::tibble())
  }
  df %>%
    mutate(
      RM_avg_for_diag = rowMeans(across(any_of(c("RM1", "RM2"))), na.rm = TRUE),
      RM_avg_for_diag = ifelse(is.nan(RM_avg_for_diag), NA_real_, RM_avg_for_diag),
      RM_diff = RM1 - RM2,
      RM_abs_diff = abs(RM_diff),
      RM_excluded = !is.na(RM_abs_diff) & RM_abs_diff > config$rm_duplicate_abs_threshold
    ) %>%
    group_by(Country, Instrument, Size, Campaign) %>%
    summarise(
      n_rows = n(),
      n_RM_duplicates = sum(!is.na(RM1) & !is.na(RM2)),
      n_RM_excluded = sum(RM_excluded, na.rm = TRUE),
      pct_RM_excluded = ifelse(n_RM_duplicates > 0, 100 * n_RM_excluded / n_RM_duplicates, NA_real_),
      RM_diff_mean = mean(RM_diff, na.rm = TRUE),
      RM_diff_SD = safe_sd(RM_diff),
      RM_abs_diff_mean = mean(RM_abs_diff, na.rm = TRUE),
      RM_abs_diff_CI95 = safe_quantile(RM_abs_diff, 0.95),
      RM_diff_skewness = safe_skewness(RM_diff),
      RM_diff_kurtosis = safe_kurtosis(RM_diff),
      .groups = "drop"
    )
}

make_diagnostic_plots <- function(output_dir, df_before_screening, stage_df, diagnostics_linearity, diagnostics_diff, config = default_config) {
  diag_dir <- file.path(output_dir, "02_diagnostics", "plots")
  colors_outlier <- c("OK" = "darkgreen", "Low (>2 µg/m³)" = "orange", "High (>25% RM)" = "red", "Outside fit range" = "black")
  colors_rm_screen <- c("Retained" = "darkgreen", "Excluded: |RM1-RM2| > threshold" = "red")
  correction_model_label <- toupper(config$correction_model %||% "TLS")
  fit_label <- dplyr::case_when(
    correction_model_label == "TLS" ~ "TLS fit",
    correction_model_label == "OLS" ~ "OLS fit",
    correction_model_label == "MEAN_RATIO" ~ "Mean-ratio fit",
    TRUE ~ "TLS diagnostic fit"
  )
  colors_line_type <- c("Loess" = "black", "Reference" = "red")
  colors_line_type <- c(colors_line_type, stats::setNames("blue", fit_label))
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  saved <- list()

  # RM plots: in ongoing datasets RM2 is often absent/empty. In that case we write placeholders
  # instead of stopping with ggplot faceting errors.
  if (all(c("RM1", "RM2") %in% names(df_before_screening))) {
    rm_plot_data <- df_before_screening %>%
      mutate(
        RM_AVG_diag = rowMeans(across(any_of(c("RM1", "RM2"))), na.rm = TRUE),
        RM_AVG_diag = ifelse(is.nan(RM_AVG_diag), NA_real_, RM_AVG_diag),
        RM_diff = RM1 - RM2,
        RM_abs_diff = abs(RM_diff),
        RM_flag = ifelse(RM_abs_diff > config$rm_duplicate_abs_threshold, "Excluded: |RM1-RM2| > threshold", "Retained")
      )

    d_scatter <- rm_plot_data %>% filter(is.finite(RM1), is.finite(RM2))
    f <- file.path(diag_dir, "rm_duplicate_scatter.png")
    save_plot_or_placeholder(
      d_scatter,
      d_scatter %>% ggplot(aes(x = RM1, y = RM2, color = RM_flag)) +
        geom_point(alpha = 0.65, size = 1.7) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        facet_wrap(~Size, scales = "free") +
        scale_color_manual(values = colors_rm_screen, drop = FALSE) +
        labs(title = "RM duplicate comparison", x = "RM1 (µg/m³)", y = "RM2 (µg/m³)", color = "RM screening") +
        theme_bw(base_size = 11),
      f, "RM duplicate comparison"
    )
    saved$rm_duplicate_scatter <- f

    d_ba <- rm_plot_data %>% filter(is.finite(RM_AVG_diag), is.finite(RM_diff))
    f <- file.path(diag_dir, "rm_duplicate_bland_altman.png")
    save_plot_or_placeholder(
      d_ba,
      d_ba %>% ggplot(aes(x = RM_AVG_diag, y = RM_diff, color = RM_flag)) +
        geom_point(alpha = 0.65, size = 1.7) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = c(-config$rm_duplicate_abs_threshold, config$rm_duplicate_abs_threshold), linetype = "dotted") +
        facet_wrap(~Size, scales = "free") +
        scale_color_manual(values = colors_rm_screen, drop = FALSE) +
        labs(title = "RM duplicate difference vs RM average", x = "RM average (µg/m³)", y = "RM1 - RM2 (µg/m³)", color = "RM screening") +
        theme_bw(base_size = 11),
      f, "RM duplicate difference vs RM average"
    )
    saved$rm_duplicate_bland_altman <- f

    d_hist <- rm_plot_data %>% filter(is.finite(RM_diff))
    f <- file.path(diag_dir, "rm_duplicate_histogram.png")
    save_plot_or_placeholder(
      d_hist,
      d_hist %>% ggplot(aes(x = RM_diff)) +
        geom_histogram(bins = 30, color = "black", fill = "grey80") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = c(-config$rm_duplicate_abs_threshold, config$rm_duplicate_abs_threshold), linetype = "dotted") +
        facet_wrap(~Size, scales = "free") +
        labs(title = "Distribution of RM duplicate differences", x = "RM1 - RM2 (µg/m³)", y = "Count") +
        theme_bw(base_size = 11),
      f, "Distribution of RM duplicate differences"
    )
    saved$rm_duplicate_histogram <- f

    d_time <- rm_plot_data %>% filter(is.finite(RM_diff))
    f <- file.path(diag_dir, "rm_duplicate_time_series.png")
    save_plot_or_placeholder(
      d_time,
      d_time %>% ggplot(aes(x = date, y = RM_diff, color = RM_flag)) +
        geom_point(alpha = 0.7, size = 1.5) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = c(-config$rm_duplicate_abs_threshold, config$rm_duplicate_abs_threshold), linetype = "dotted") +
        facet_wrap(~Size, scales = "free_y") +
        scale_color_manual(values = colors_rm_screen, drop = FALSE) +
        labs(title = "RM duplicate differences over time", x = "Date", y = "RM1 - RM2 (µg/m³)", color = "RM screening") +
        theme_bw(base_size = 11),
      f, "RM duplicate differences over time"
    )
    saved$rm_duplicate_time_series <- f
  }

  cm_plot_data <- stage_df %>%
    filter(Stage == "No correction", is.finite(RM_AVG), is.finite(CM_value)) %>%
    mutate(
      diff = CM_value - RM_AVG,
      abs_diff = abs(diff),
      outside_fit_range = RM_AVG < config$tls_fit_rm_min |
        RM_AVG > config$tls_fit_rm_max_factor_daily_LV * LV_daily,
      outlier_cm = case_when(
        outside_fit_range ~ "Outside fit range",
        abs_diff > 2 & abs_diff > 0.25 * abs(RM_AVG) ~ "High (>25% RM)",
        abs_diff > 2 ~ "Low (>2 µg/m³)",
        TRUE ~ "OK"
      ),
      thr_mixed_pos = ifelse(RM_AVG <= 8, 2, 0.25 * RM_AVG),
      thr_mixed_neg = -thr_mixed_pos
    )

  cm_linearity_plot_data <- filter_tls_fit_range(cm_plot_data, config, value_col = "CM_value")
  cm_ba_loess_data <- filter_tls_fit_range(cm_plot_data, config, value_col = "CM_value")

  fit_line_data <- cm_linearity_plot_data %>%
    group_by(Size, CM_type) %>%
    group_modify(~ {
      model <- toupper(config$correction_model %||% "TLS")
      if (model == "OLS") {
        fit <- ols_fit_internal(.x$RM_AVG, .x$CM_value)
        intercept <- fit$intercept
        slope <- fit$slope
      } else if (model == "MEAN_RATIO") {
        ok <- is.finite(.x$RM_AVG) & is.finite(.x$CM_value)
        ratio <- if (sum(ok) > 0 && mean(.x$RM_AVG[ok], na.rm = TRUE) != 0) {
          mean(.x$CM_value[ok], na.rm = TRUE) / mean(.x$RM_AVG[ok], na.rm = TRUE)
        } else NA_real_
        intercept <- 0
        slope <- ratio
      } else {
        fit <- tls_fit(.x$RM_AVG, .x$CM_value)
        intercept <- fit$intercept
        slope <- fit$slope
      }
      tibble::tibble(intercept = intercept, slope = slope, fit_label = fit_label)
    }) %>%
    ungroup() %>%
    filter(is.finite(intercept), is.finite(slope))

  f <- file.path(diag_dir, "cm_vs_rm_linearity.png")
  save_plot_or_placeholder(
    cm_linearity_plot_data,
    cm_linearity_plot_data %>% ggplot(aes(x = RM_AVG, y = CM_value, shape = CM_type)) +
      geom_point(alpha = 0.60, size = 1.7, color = "grey35") +
      geom_abline(aes(slope = 1, intercept = 0, color = "Reference"), linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
      geom_smooth(aes(color = "Loess", group = CM_type), method = "loess", formula = y ~ x, se = TRUE, alpha = 0.25, linewidth = 0.8) +
      geom_abline(data = fit_line_data, aes(slope = slope, intercept = intercept, color = fit_label), linewidth = 1.0, inherit.aes = FALSE) +
      scale_color_manual(name = "Line type", values = colors_line_type, breaks = c(fit_label, "Loess", "Reference")) +
      facet_grid(CM_type ~ Size, scales = "free") +
      labs(title = paste0("CM vs RM linearity diagnostic (fit range: ", tls_fit_range_label(config), ")"), x = "RM average (µg/m³)", y = "CM (µg/m³)") +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom"),
    f, "CM vs RM linearity diagnostic"
  )
  saved$cm_vs_rm_linearity <- f

  f <- file.path(diag_dir, "cm_bland_altman.png")
  save_plot_or_placeholder(
    cm_plot_data,
    cm_plot_data %>% ggplot(aes(x = RM_AVG, y = diff, color = outlier_cm, shape = CM_type)) +
      geom_point(alpha = 0.7, size = 1.7) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line(aes(y = thr_mixed_pos), linetype = "dotted") +
      geom_line(aes(y = thr_mixed_neg), linetype = "dotted") +
      geom_smooth(
        data = cm_ba_loess_data,
        aes(x = RM_AVG, y = diff, group = CM_type),
        method = "loess", formula = y ~ x, color = "black",
        se = TRUE, linewidth = 0.7, inherit.aes = FALSE
      ) +
      scale_color_manual(values = colors_outlier, drop = FALSE) +
      facet_grid(CM_type ~ Size, scales = "free") +
      labs(title = "Bland-Altman diagnostic: CM - RM vs RM", subtitle = "LOESS fitted only in the configured fitting range; outside-range points shown in black", x = "RM average (µg/m³)", y = "CM - RM (µg/m³)", color = "Point category") +
      theme_bw(base_size = 11),
    f, "Bland-Altman diagnostic: CM - RM vs RM",
    width = 10, height = 7
  )
  saved$cm_bland_altman <- f

  f <- file.path(diag_dir, "cm_difference_histogram.png")
  save_plot_or_placeholder(
    cm_plot_data,
    cm_plot_data %>% ggplot(aes(x = diff)) +
      geom_histogram(bins = 30, color = "black", fill = "lightblue", alpha = 0.70) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      facet_grid(CM_type ~ Size, scales = "free") +
      labs(title = "Distribution of CM-RM differences", subtitle = "Skewness and kurtosis are reported in difference_diagnostics.csv", x = "CM - RM (µg/m³)", y = "Count") +
      theme_bw(base_size = 11),
    f, "Distribution of CM-RM differences",
    width = 9, height = 7
  )
  saved$cm_difference_histogram <- f

  f <- file.path(diag_dir, "cm_difference_time_series.png")
  save_plot_or_placeholder(
    cm_plot_data,
    cm_plot_data %>% ggplot(aes(x = date, y = diff, color = outlier_cm, shape = CM_type)) +
      geom_point(alpha = 0.7, size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = c(-2, 2), color = "orange", linetype = "dotted") +
      scale_color_manual(values = colors_outlier, drop = FALSE) +
      facet_grid(CM_type ~ Size, scales = "free_y") +
      labs(title = "CM-RM differences over time", x = "Date", y = "CM - RM (µg/m³)", color = "Outlier category") +
      theme_bw(base_size = 11),
    f, "CM-RM differences over time",
    width = 10, height = 7
  )
  saved$cm_difference_time_series <- f

  tibble::tibble(plot = names(saved), file = unlist(saved, use.names = FALSE))
}

make_plots <- function(output_dir, daily_unit, annual_unit, daily_site, annual_site) {
  plot_dir <- file.path(output_dir, "04_plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  saved <- list()

  d_daily <- daily_unit %>% filter(is.finite(u_daily_rel_LV_pct)) %>%
    mutate(
      total_random_pct = 100 * (2 * scorr) / LV_daily,
      total_bias_pct = 100 * u_daily_bias / LV_daily
    ) %>%
    tidyr::pivot_longer(cols = c(total_random_pct, total_bias_pct), names_to = "component", values_to = "value") %>%
    mutate(component = dplyr::recode(component,
      total_random_pct = "Random component",
      total_bias_pct = "Bias component"
    ))
  f <- file.path(plot_dir, "daily_primary_uncertainty.png")
  save_plot_or_placeholder(
    d_daily,
    d_daily %>% ggplot(aes(x = reorder(paste(Instrument, CM_type, sep = " | "), value, function(z) max(z, na.rm = TRUE)), y = value, fill = component)) +
      geom_col() +
      geom_hline(data = daily_unit %>% distinct(Stage, Size, DQO_percent), aes(yintercept = DQO_percent), linetype = "dashed", inherit.aes = FALSE) +
      coord_flip() +
      facet_grid(Size ~ Stage, scales = "free_y", space = "free_y") +
      labs(title = "Decomposed daily metric by CM unit", x = "Instrument | CM", y = "Decomposed daily components (%LVd)", fill = "Component") +
      theme_bw(base_size = 11),
    f, "Decomposed daily metric by CM unit",
    width = 12, height = 9
  )
  saved$daily_primary_uncertainty <- f

  d_annual <- annual_unit %>% filter(is.finite(u_annual_rel_LV_pct)) %>%
    mutate(
      total_random_pct = 100 * (2 * u_annual_random) / LV_annual,
      total_bias_pct = 100 * u_annual_bias / LV_annual
    ) %>%
    tidyr::pivot_longer(cols = c(total_random_pct, total_bias_pct), names_to = "component", values_to = "value") %>%
    mutate(component = dplyr::recode(component,
      total_random_pct = "Random component",
      total_bias_pct = "Bias component"
    ))
  f <- file.path(plot_dir, "annual_primary_uncertainty.png")
  save_plot_or_placeholder(
    d_annual,
    d_annual %>% ggplot(aes(x = reorder(paste(Instrument, CM_type, sep = " | "), value, function(z) max(z, na.rm = TRUE)), y = value, fill = component)) +
      geom_col() +
      geom_hline(data = annual_unit %>% distinct(Stage, Size, DQO_percent), aes(yintercept = DQO_percent), linetype = "dashed", inherit.aes = FALSE) +
      coord_flip() +
      facet_grid(Size ~ Stage, scales = "free_y", space = "free_y") +
      labs(title = "Annual uncertainty components by CM unit", x = "Instrument | CM", y = "Uann components (%LVy)", fill = "Component") +
      theme_bw(base_size = 11),
    f, "Annual uncertainty components by CM unit",
    width = 12, height = 9
  )
  saved$annual_primary_uncertainty <- f

  d_site <- daily_site %>%
    mutate(
      daily_plot_metric_rel_LV_pct = daily_metric_rel_LV_pct,
      daily_plot_metric_name = daily_decision_metric,
      daily_plot_metric_name = ifelse(is.na(daily_plot_metric_name) | !nzchar(daily_plot_metric_name), "configured daily metric", daily_plot_metric_name)
    ) %>%
    filter(is.finite(daily_plot_metric_rel_LV_pct)) %>%
    mutate(
      site_label = paste(Site, CM_type, sep = " | "),
      site_label = stringr::str_trunc(site_label, width = 42)
    )
  daily_x_label <- if (nrow(d_site) > 0) {
    paste0(unique(d_site$daily_plot_metric_name)[1], " site/campaign (% LVd)")
  } else {
    "Configured daily metric site/campaign (% LVd)"
  }
  f <- file.path(plot_dir, "daily_site_campaign_guardrail.png")
  save_plot_or_placeholder(
    d_site,
    d_site %>% ggplot(aes(x = daily_plot_metric_rel_LV_pct, y = reorder(site_label, daily_plot_metric_rel_LV_pct), color = Stage, shape = Stage)) +
      geom_point(size = 3.0, alpha = 0.9) +
      geom_vline(aes(xintercept = DQO_percent), linetype = "dashed") +
      facet_wrap(~Size, scales = "free_y") +
      labs(title = "Daily site/campaign LV assessment", x = daily_x_label, y = "Site | CM", color = "Stage", shape = "Stage") +
      theme_bw(base_size = 11) +
      theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        strip.text = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold"),
        legend.position = "bottom"
      ),
    f, "Daily site/campaign LV assessment",
    width = 10.5, height = 7.2
  )
  saved$daily_site_campaign_guardrail <- f

  d_annual_site <- annual_site %>%
    filter(is.finite(u_annual_rel_LV_pct)) %>%
    mutate(
      site_label = paste(Site, CM_type, sep = " | "),
      site_label = stringr::str_trunc(site_label, width = 42)
    )
  f <- file.path(plot_dir, "annual_site_campaign_uncertainty.png")
  save_plot_or_placeholder(
    d_annual_site,
    d_annual_site %>% ggplot(aes(x = u_annual_rel_LV_pct, y = reorder(site_label, u_annual_rel_LV_pct), color = Stage, shape = Stage)) +
      geom_point(size = 3.0, alpha = 0.9) +
      geom_vline(aes(xintercept = DQO_percent), linetype = "dashed") +
      facet_wrap(~Size, scales = "free_y") +
      labs(title = "Annual site/campaign uncertainty", x = "Uann site/campaign (%LVy)", y = "Site | CM", color = "Stage", shape = "Stage") +
      theme_bw(base_size = 11) +
      theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        strip.text = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold"),
        legend.position = "bottom"
      ),
    f, "Annual site/campaign uncertainty",
    width = 10.5, height = 7.2
  )
  saved$annual_site_campaign_uncertainty <- f

  tibble::tibble(plot = names(saved), file = unlist(saved, use.names = FALSE))
}


# ---- HTML report rendering ----------------------------------------------------
render_report <- function(output_dir, input_file, scenario, run_mode, config) {
  # Treat output_dir as the base workflow output directory. If a report folder
  # is accidentally passed, recover the parent to avoid 05_report duplication.
  output_dir_abs <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (basename(output_dir_abs) == "05_report") {
    output_dir_abs <- dirname(output_dir_abs)
  }

  report_dir <- file.path(output_dir_abs, "05_report")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

  input_file_abs <- normalizePath(input_file, winslash = "/", mustWork = FALSE)

  template_candidates <- unique(c(
    file.path(.workflow_script_dir, "equivalence_report_template_v1_61.qmd"),
    file.path(getwd(), "equivalence_report_template_v1_61.qmd"),
    file.path(dirname(input_file_abs), "equivalence_report_template_v1_61.qmd")
  ))

  template_in <- template_candidates[file.exists(template_candidates)][1]
  if (is.na(template_in) || length(template_in) == 0) {
    stop(
      "Report template not found. Put equivalence_report_template_v1_61.qmd ",
      "in the same folder as equivalence_workflow_v1_61.R."
    )
  }

  message("Using report template: ", normalizePath(template_in, winslash = "/", mustWork = FALSE))

  qmd_lines <- readLines(template_in, warn = FALSE, encoding = "UTF-8")

  # Validate YAML front matter and then rewrite it explicitly. This prevents
  # malformed templates or path substitutions from breaking Quarto.
  if (length(qmd_lines) < 3 || trimws(qmd_lines[[1]]) != "---") {
    stop("Invalid QMD template: YAML front matter must start with '---'.")
  }
  yaml_end <- which(trimws(qmd_lines[-1]) == "---")[1] + 1
  if (is.na(yaml_end) || yaml_end <= 1) {
    stop("Invalid QMD template: YAML front matter must end with '---'.")
  }

  qmd_body <- qmd_lines[(yaml_end + 1):length(qmd_lines)]

  esc_yaml <- function(x) {
    x <- gsub("\\\\", "/", as.character(x))
    x <- gsub('"', '\\"', x, fixed = TRUE)
    paste0('"', x, '"')
  }

  new_yaml <- c(
    "---",
    'title: "Candidate Method Equivalence Report"',
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 3",
    "    number-sections: true",
    "    theme: cosmo",
    "    embed-resources: true",
    "params:",
    paste0("  output_dir: ", esc_yaml(output_dir_abs)),
    paste0("  input_file: ", esc_yaml(basename(input_file))),
    paste0("  scenario: ", esc_yaml(scenario)),
    paste0("  run_mode: ", esc_yaml(run_mode)),
    "---",
    ""
  )

  qmd_out <- file.path(report_dir, "equivalence_report.qmd")
  writeLines(c(new_yaml, qmd_body), qmd_out, useBytes = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(report_dir)

  args <- c(
    "render", "equivalence_report.qmd",
    "--to", "html",
    "--output", "equivalence_report.html"
  )

  message("Rendering HTML report in: ", normalizePath(report_dir, winslash = "/", mustWork = FALSE))
  res <- tryCatch(
    system2("quarto", args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) {
      stop("Quarto render failed: ", conditionMessage(e))
    }
  )

  if (length(res) > 0) message(paste(res, collapse = "\n"))

  html_out <- file.path(report_dir, "equivalence_report.html")
  if (!file.exists(html_out)) {
    stop("Quarto completed but the HTML report was not created at: ", html_out)
  }

  message("HTML report written to: ", normalizePath(html_out, winslash = "/", mustWork = FALSE))
  invisible(html_out)
}

# ---- Main workflow ------------------------------------------------------------
run_equivalence_workflow <- function(input_file,
                                     output_dir = "output_equivalence",
                                     scenario = c("type_testing", "ongoing_verification"),
                                     run_mode = c("full_workflow", "uncertainty_only", "diagnostic_only"),
                                     stop_on_rm_fail = TRUE,
                                     render_html = TRUE,
                                     country_filter = NULL,
                                     instrument_filter = NULL,
                                     size_filter = NULL,
                                     campaign_filter = NULL,
                                     config_file = NULL,
                                     config = default_config) {
  scenario <- match.arg(scenario)
  run_mode <- match.arg(run_mode)
  config <- load_workflow_config(config_file = config_file, base_config = config)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "00_run_log"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "01_clean_data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "02_diagnostics"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "03_uncertainty_tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "04_plots"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "05_report"), recursive = TRUE, showWarnings = FALSE)

  config_summary <- config_summary_table(config)
  readr::write_csv(config_summary, file.path(output_dir, "00_run_log", "configuration_used.csv"))

  message("Reading input: ", input_file)
  raw <- read_equivalence_input(input_file)
  std_all <- standardise_cm_data(raw) %>% add_limit_values(config)
  readr::write_csv(make_available_filters(std_all), file.path(output_dir, "00_run_log", "available_filters.csv"))
  std <- filter_focus(std_all, country_filter = country_filter, instrument_filter = instrument_filter, size_filter = size_filter, campaign_filter = campaign_filter)

  input_check <- input_data_check(std, scenario)
  readr::write_csv(input_check, file.path(output_dir, "00_run_log", "input_data_check.csv"))

  rm_diagnostics <- make_rm_diagnostics(std, config)
  readr::write_csv(rm_diagnostics, file.path(output_dir, "02_diagnostics", "rm_diagnostics.csv"))

  rm <- rm_screening(std, config, stop_on_fail = FALSE)
  readr::write_csv(rm$summary, file.path(output_dir, "00_run_log", "rm_screening_summary.csv"))
  if (isTRUE(stop_on_rm_fail) && identical(rm$summary$rm_screen_flag[1], "RM_SCREEN_FAIL_REDO")) {
    run_log <- tibble::tibble(
      run_time = as.character(Sys.time()),
      workflow_version = config$workflow_version,
      config_file = config$config_file %||% NA_character_,
      correction_model = config$correction_model,
      daily_decision_metric = config$daily_decision_metric,
      input_file = normalizePath(input_file, winslash = "/", mustWork = FALSE),
      output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
      scenario = scenario,
      run_mode = run_mode,
      stop_on_rm_fail = stop_on_rm_fail,
      rm_screen_flag = rm$summary$rm_screen_flag[1],
      country_filter = paste(country_filter %||% "ALL", collapse = ";"),
      instrument_filter = paste(instrument_filter %||% "ALL", collapse = ";"),
      size_filter = paste(size_filter %||% "ALL", collapse = ";"),
      campaign_filter = paste(campaign_filter %||% "ALL", collapse = ";"),
      status = "STOPPED_RM_SCREEN_FAIL_REDO"
    )
    readr::write_csv(run_log, file.path(output_dir, "00_run_log", "run_log.csv"))
    stop(
      "RM screening failed: ", round(rm$summary$pct_excluded_duplicate_denominator[1], 2),
      "% of duplicate RM rows have |RM1 - RM2| > ", config$rm_duplicate_abs_threshold,
      " µg/m3. Output folder contains rm_screening_summary.csv and run_log.csv."
    )
  }
  clean <- rm$data
  readr::write_csv(clean, file.path(output_dir, "01_clean_data", "clean_screened_data.csv"))

  ubs_table <- estimate_ubsRM(clean)
  readr::write_csv(ubs_table, file.path(output_dir, "00_run_log", "ubsRM_table.csv"))

  long <- prepare_long_cm_data(clean, scenario, config)
  corrections <- estimate_corrections(long, config)
  readr::write_csv(corrections, file.path(output_dir, "03_uncertainty_tables", "correction_coefficients.csv"))

  stage_df <- apply_correction_stages(long, corrections, scenario)
  readr::write_csv(stage_df, file.path(output_dir, "01_clean_data", "long_stage_data.csv"))

  V_table <- estimate_V_table(clean, config)
  readr::write_csv(V_table, file.path(output_dir, "03_uncertainty_tables", "V_Neff_inputs.csv"))

  diagnostics_linearity <- calc_linearity_diagnostics(stage_df, scenario, config)
  diagnostics_diff <- calc_difference_diagnostics(stage_df, config)
  diagnostics_diff_site <- calc_difference_site_diagnostics(stage_df, config)
  readr::write_csv(diagnostics_linearity, file.path(output_dir, "02_diagnostics", "linearity_diagnostics.csv"))
  readr::write_csv(diagnostics_diff, file.path(output_dir, "02_diagnostics", "difference_diagnostics.csv"))
  readr::write_csv(diagnostics_diff_site, file.path(output_dir, "02_diagnostics", "difference_site_campaign_diagnostics.csv"))
  diagnostic_plot_files <- make_diagnostic_plots(output_dir, std, stage_df, diagnostics_linearity, diagnostics_diff, config)
  readr::write_csv(diagnostic_plot_files, file.path(output_dir, "02_diagnostics", "diagnostic_plot_index.csv"))

  daily_unit_raw <- calc_daily_unit(stage_df, ubs_table, config) %>% complete_daily_unit_results(stage_df, config)
  daily_comparison_raw <- calc_daily_comparison_metrics(stage_df, daily_unit_raw, ubs_table, config)
  daily_site_raw <- calc_daily_site(stage_df, ubs_table, config)
  annual_unit_raw <- calc_annual_unit(stage_df, ubs_table, V_table, config) %>% complete_annual_unit_results(stage_df, config)
  annual_site_raw <- calc_annual_site(stage_df, ubs_table, V_table, config)
  method_summary_raw <- combine_method_status(daily_unit_raw, daily_site_raw, annual_unit_raw, annual_site_raw, scenario)
  final_site_assessment_raw <- combine_site_daily_annual(daily_site_raw, annual_site_raw)

  # Sorted and rounded tables for user-facing CSV/Excel/HTML.
  method_summary <- prepare_output_table(method_summary_raw, digits = 1)
  final_site_assessment <- prepare_output_table(final_site_assessment_raw, digits = 1)
  daily_unit <- prepare_output_table(daily_unit_raw, digits = 1)
  daily_comparison <- prepare_output_table(daily_comparison_raw, digits = 1)
  daily_site <- prepare_output_table(daily_site_raw, digits = 1)
  annual_unit <- prepare_output_table(annual_unit_raw, digits = 1)
  annual_site <- prepare_output_table(annual_site_raw, digits = 1)
  diagnostics_linearity_out <- prepare_output_table(diagnostics_linearity, digits = 2)
  diagnostics_diff_out <- prepare_output_table(diagnostics_diff, digits = 1)
  diagnostics_diff_site_out <- prepare_output_table(diagnostics_diff_site, digits = 1)
  rm_diagnostics_out <- prepare_output_table(rm_diagnostics, digits = 1)
  corrections_out <- prepare_output_table(corrections, digits = 1)
  rm_summary_out <- prepare_output_table(rm$summary, digits = 1)
  ubs_table_out <- prepare_output_table(ubs_table, digits = 1)
  V_table_out <- prepare_output_table(V_table, digits = 1)

  # Daily first in outputs.
  readr::write_csv(method_summary, file.path(output_dir, "03_uncertainty_tables", "method_summary.csv"))
  readr::write_csv(final_site_assessment, file.path(output_dir, "03_uncertainty_tables", "final_site_combined_assessment.csv"))
  readr::write_csv(daily_unit, file.path(output_dir, "03_uncertainty_tables", "daily_unit_results.csv"))
  readr::write_csv(daily_comparison, file.path(output_dir, "03_uncertainty_tables", "daily_lv_comparison_metrics.csv"))
  readr::write_csv(daily_site, file.path(output_dir, "03_uncertainty_tables", "daily_site_campaign_results.csv"))
  readr::write_csv(annual_unit, file.path(output_dir, "03_uncertainty_tables", "annual_unit_results.csv"))
  readr::write_csv(annual_site, file.path(output_dir, "03_uncertainty_tables", "annual_site_results.csv"))

  xlsx_file <- file.path(output_dir, "equivalence_results_daily_first.xlsx")
  write_excel_tables(xlsx_file, list(
    Method_summary = method_summary,
    Final_site_combined = final_site_assessment,
    Daily_unit_results = daily_unit,
    Daily_LV_comparison = daily_comparison,
    Daily_site_campaign = daily_site,
    Annual_unit_results = annual_unit,
    Annual_site_results = annual_site,
    Linearity_diagnostics = diagnostics_linearity_out,
    Difference_diagnostics = diagnostics_diff_out,
    Difference_site_campaign = diagnostics_diff_site_out,
    RM_diagnostics = rm_diagnostics_out,
    Correction_coefficients = corrections_out,
    RM_screening = rm_summary_out,
    ubsRM = ubs_table_out,
    V_Neff_inputs = V_table_out,
    Input_check = input_check
  ))

  plot_files <- make_plots(output_dir, daily_unit_raw, annual_unit_raw, daily_site_raw, annual_site_raw)
  readr::write_csv(plot_files, file.path(output_dir, "04_plots", "plot_index.csv"))

  run_log <- tibble::tibble(
    run_time = as.character(Sys.time()),
    workflow_version = config$workflow_version,
    daily_assessment_metric = "CI95 of absolute CM-RM differences in the daily LV window; low-N daily estimates are reported as LOW N, not PASS/FAIL",
    annual_assessment_metric = "2 * Random_y + Bias_y",
    daily_decomposed_metric = "retained for comparison only",
    input_file = normalizePath(input_file, winslash = "/", mustWork = FALSE),
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    scenario = scenario,
    run_mode = run_mode,
    stop_on_rm_fail = stop_on_rm_fail,
    rm_screen_flag = rm$summary$rm_screen_flag[1],
    country_filter = paste(country_filter %||% "ALL", collapse = ";"),
    instrument_filter = paste(instrument_filter %||% "ALL", collapse = ";"),
    size_filter = paste(size_filter %||% "ALL", collapse = ";"),
    campaign_filter = paste(campaign_filter %||% "ALL", collapse = ";"),
    xlsx_results = normalizePath(xlsx_file, winslash = "/", mustWork = FALSE)
  )
  readr::write_csv(run_log, file.path(output_dir, "00_run_log", "run_log.csv"))

  if (isTRUE(render_html)) {
    render_report(output_dir, input_file, scenario, run_mode, config)
  }

  message("Workflow completed. Results written to: ", output_dir)
  invisible(list(
    method_summary = method_summary,
    final_site_assessment = final_site_assessment,
    daily_unit = daily_unit,
    daily_site = daily_site,
    annual_unit = annual_unit,
    annual_site = annual_site,
    rm_screening = rm$summary,
    output_dir = output_dir,
    xlsx_file = xlsx_file
  ))
}

# ---- CLI ---------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- if (length(args) >= 1) args[[1]] else "Equivalence Data Compliation 210426.xlsx"
  output_dir <- if (length(args) >= 2) args[[2]] else "output_equivalence"
  scenario <- if (length(args) >= 3) args[[3]] else "type_testing"
  run_mode <- if (length(args) >= 4) args[[4]] else "full_workflow"
  stop_on_rm_fail <- if (length(args) >= 5) as.logical(args[[5]]) else TRUE
  country_filter <- if (length(args) >= 6 && nzchar(args[[6]])) args[[6]] else NULL
  instrument_filter <- if (length(args) >= 7 && nzchar(args[[7]])) args[[7]] else NULL
  size_filter <- if (length(args) >= 8 && nzchar(args[[8]])) args[[8]] else NULL
  config_file <- if (length(args) >= 9 && nzchar(args[[9]])) args[[9]] else NULL

  run_equivalence_workflow(
    input_file = input_file,
    output_dir = output_dir,
    scenario = scenario,
    run_mode = run_mode,
    stop_on_rm_fail = stop_on_rm_fail,
    render_html = TRUE,
    country_filter = country_filter,
    instrument_filter = instrument_filter,
    size_filter = size_filter,
    config_file = config_file
  )
}
