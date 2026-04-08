# =============================================================================
# 03_lmm_bootstrap_metrics.R
# Elbow ROM Study — Agreement Metrics with BCa Bootstrap CIs (Table 3)
#
# Purpose:
#   Computes MSD, TDI, CP, SEM, MDC, LMM fixed-effect beta,
#   sigma_alpha, and sigma_alpha_beta with BCa bootstrap confidence intervals
#   for each motion × method combination.
#
#   Point estimates are computed from a deterministic LMM fit on the full data;
#   the bootstrap distribution is used ONLY for confidence intervals.  This
#   design ensures reproducible point estimates regardless of RNG state.
#
#   Bootstrap results are cached as RDS files so the expensive bootstrap step
#   is skipped on subsequent runs.  Set FORCE_RERUN <- TRUE to re-run.
#
# Model:
#   rom ~ method + (1 | participant) + (1 | participant:method)
#   "method" recoded to "compare" vs "omc" so coefficients are labeled
#   consistently across all method groups.
#
# Inputs  (data/raw/):
#   flexion_measurements_long.csv
#   extension_measurements_long.csv
#
# Cache   (data/rds/):
#   {motion}_{method}_boot_result.rds   (created on first run)
#
# Outputs (data/agreement/):
#   lmm_{method}_{motion}.csv   — 6 files
#     Rows:    msd, tdi, cp, sem, mdc, beta, sigma_alpha, sigma_alphabeta
#     Columns: metric, point_estimate, bca_lower_ci, bca_upper_ci,
#              [also normal/basic/perc CIs retained for diagnostics],
#              method, delta_limit, coverage, bootstrap_iterations,
#              timestamp, participant_count
#
# Usage:
#   R-4.5.1 -f code/03_lmm_bootstrap_metrics.R
#   (or run interactively from the repository root)
#
# Command-line R versions:
#   00–04 scripts: R-4.5.1
#   redres diagnostics: R-4.3.3
#
# Note:
#   The bootstrap uses parallel = "multicore" which is not available on Windows.
#   On Windows, change to parallel = "snow" or parallel = "no".
# =============================================================================

library(lme4)
library(lmerTest)
library(dplyr)
library(boot)

# --- Configuration ---
FORCE_RERUN          <- FALSE
NUM_BOOTSTRAP        <- 10000
NUM_CPUS             <- 10
P_COVERAGE           <- 0.95
DELTA_LIMIT_FLEXION  <- 10  # degrees
DELTA_LIMIT_EXTENSION <- 5  # degrees

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[1])
} else {
  f_index <- which(args_all == "-f")
  if (length(f_index) > 0 && (f_index[1] + 1) <= length(args_all)) {
    args_all[f_index[1] + 1]
  } else {
    NA_character_
  }
}
SCRIPT_DIR <- if (!is.na(script_path) && nzchar(script_path)) {
  normalizePath(dirname(script_path), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}
PROJECT_ROOT <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)

DATA_DIR      <- file.path(PROJECT_ROOT, "data")
RAW_DIR       <- file.path(DATA_DIR, "raw")
RDS_DIR       <- file.path(DATA_DIR, "rds")
AGREEMENT_DIR <- file.path(DATA_DIR, "agreement")
dir.create(RDS_DIR, showWarnings = FALSE)
dir.create(AGREEMENT_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(1234)

# =============================================================================
# Bootstrap statistic — called once per bootstrap replicate
# =============================================================================
bootstrap_agreement <- function(original_data, indices, p_coverage, delta_limit) {
  unique_participants      <- unique(original_data$participant)
  sampled_participants     <- sample(unique_participants,
                                     size    = length(unique_participants),
                                     replace = TRUE)
  boot_data <- do.call(rbind, lapply(sampled_participants, function(pid) {
    original_data[original_data$participant == pid, ]
  }))

  fit <- tryCatch(
    lmer(rom ~ method + (1 | participant) + (1 | participant:method),
         data    = boot_data,
         control = lmerControl(optimizer = "bobyqa",
                               optCtrl  = list(maxfun = 2e5))),
    error = function(e) NULL
  )
  if (is.null(fit)) return(rep(NA_real_, 8))

  beta          <- fixef(fit)[["methodcompare"]]
  vc            <- as.data.frame(VarCorr(fit))
  var_interact  <- vc[vc$grp == "participant:method", "vcov"]
  var_error     <- sigma(fit)^2
  var_subject   <- vc[vc$grp == "participant",        "vcov"]

  msd <- beta^2 + 2 * (var_interact + var_error)
  tdi <- qnorm((1 + p_coverage) / 2) * sqrt(msd)
  cp  <- 1 - 2 * (1 - pnorm(delta_limit / sqrt(msd)))
  sem <- sqrt(var_error)
  mdc <- 1.96 * sqrt(2) * sem
  sigma_alpha      <- sqrt(var_subject)
  sigma_alphabeta <- sqrt(var_interact)

  c(msd = msd, tdi = tdi, cp = cp, sem = sem, mdc = mdc,
    beta = beta,
    sigma_alpha = sigma_alpha, sigma_alphabeta = sigma_alphabeta)
}

# =============================================================================
# Deterministic point estimates from full-data LMM
# =============================================================================
compute_point_estimates <- function(df, p_coverage, delta_limit) {
  fit <- lmer(rom ~ method + (1 | participant) + (1 | participant:method),
              data    = df,
              control = lmerControl(optimizer = "bobyqa",
                                    optCtrl  = list(maxfun = 2e5)))

  beta         <- fixef(fit)[["methodcompare"]]
  vc           <- as.data.frame(VarCorr(fit))
  var_interact <- vc[vc$grp == "participant:method", "vcov"]
  var_error    <- sigma(fit)^2
  var_subject  <- vc[vc$grp == "participant",        "vcov"]

  msd <- beta^2 + 2 * (var_interact + var_error)
  tdi <- qnorm((1 + p_coverage) / 2) * sqrt(msd)
  cp  <- 1 - 2 * (1 - pnorm(delta_limit / sqrt(msd)))
  sem <- sqrt(var_error)
  mdc <- 1.96 * sqrt(2) * sem
  sigma_alpha      <- sqrt(var_subject)
  sigma_alphabeta <- sqrt(var_interact)

  c(msd = msd, tdi = tdi, cp = cp, sem = sem, mdc = mdc,
    beta = beta,
    sigma_alpha = sigma_alpha, sigma_alphabeta = sigma_alphabeta)
}

extract_ci_bounds <- function(boot_result, metric_index) {
  get_one_type <- function(type_name, slot_name, lower_idx, upper_idx) {
    ci <- tryCatch(
      boot.ci(boot_result, type = type_name, index = metric_index),
      error = function(e) NULL
    )
    if (is.null(ci)) return(c(NA_real_, NA_real_))

    slot <- ci[[slot_name]]
    if (is.null(slot) || length(slot) < upper_idx) {
      return(c(NA_real_, NA_real_))
    }
    c(slot[lower_idx], slot[upper_idx])
  }

  perc_fallback <- function() {
    boot_vals <- boot_result$t[, metric_index]
    boot_vals <- boot_vals[is.finite(boot_vals)]
    if (length(boot_vals) < 2) return(c(NA_real_, NA_real_))
    as.numeric(stats::quantile(boot_vals, probs = c(0.025, 0.975), na.rm = TRUE))
  }

  perc_ci <- get_one_type("perc", "perc", 4, 5)
  if (all(is.na(perc_ci))) {
    perc_ci <- perc_fallback()
  }

  list(
    normal = get_one_type("norm",  "normal", 2, 3),
    basic  = get_one_type("basic", "basic",  4, 5),
    perc   = perc_ci,
    bca    = get_one_type("bca",   "bca",    4, 5)
  )
}

# =============================================================================
# Main analysis function — one motion × method pair
# =============================================================================
run_analysis <- function(long_data, method_name, motion) {
  delta_limit <- if (motion == "flexion") DELTA_LIMIT_FLEXION else DELTA_LIMIT_EXTENSION

  df <- long_data %>%
    filter(method %in% c("omc", method_name)) %>%
    mutate(
      participant = as.factor(participant),
      method      = factor(method),
      method      = relevel(method, ref = "omc"),
      rom         = as.numeric(rom),
      method      = dplyr::recode(method, !!method_name := "compare")
    )

  rds_path <- file.path(RDS_DIR,
    sprintf("%s_%s_boot_result.rds", motion, method_name))

  # --- Bootstrap (cached) ---
  if (!FORCE_RERUN && file.exists(rds_path)) {
    cat(sprintf("  Loading cached bootstrap for %s %s ...\n", motion, method_name))
    boot_result <- readRDS(rds_path)
  } else {
    cat(sprintf("  Running bootstrap for %s %s (%d iterations) ...\n",
                motion, method_name, NUM_BOOTSTRAP))
    boot_result <- boot(
      data      = df,
      statistic = bootstrap_agreement,
      R         = NUM_BOOTSTRAP,
      p_coverage  = P_COVERAGE,
      delta_limit = delta_limit,
      parallel  = "multicore",
      ncpus     = NUM_CPUS
    )
    saveRDS(boot_result, rds_path)
    cat(sprintf("  Cached: %s\n", rds_path))
  }

  # --- Deterministic point estimates (always recomputed) ---
  cat(sprintf("  Computing deterministic point estimates for %s %s ...\n",
              motion, method_name))
  pt <- compute_point_estimates(df, P_COVERAGE, delta_limit)

  # Replace bootstrap t0 with deterministic estimates so CIs are centered
  # on the correct full-data values rather than on a random bootstrap sample.
  boot_result$t0 <- pt

  # --- BCa confidence intervals ---
  metric_names <- c("msd", "tdi", "cp", "sem", "mdc", "beta",
                    "sigma_alpha", "sigma_alphabeta")
  ci_rows <- lapply(seq_along(metric_names), function(i) {
    ci <- extract_ci_bounds(boot_result, i)
    data.frame(
      metric           = metric_names[i],
      point_estimate   = pt[i],
      bca_lower_ci     = ci$bca[1],
      bca_upper_ci     = ci$bca[2],
      normal_lower_ci  = ci$normal[1],
      normal_upper_ci  = ci$normal[2],
      basic_lower_ci   = ci$basic[1],
      basic_upper_ci   = ci$basic[2],
      perc_lower_ci    = ci$perc[1],
      perc_upper_ci    = ci$perc[2],
      stringsAsFactors = FALSE
    )
  })
  results <- do.call(rbind, ci_rows)

  results$method               <- method_name
  results$delta_limit          <- delta_limit
  results$coverage             <- P_COVERAGE
  results$bootstrap_iterations <- NUM_BOOTSTRAP
  results$timestamp            <- Sys.time()
  results$participant_count    <- length(unique(boot_result$data$participant))

  results <- results %>%
    select(metric,
           point_estimate,
           bca_lower_ci,
           bca_upper_ci,
           normal_lower_ci,
           normal_upper_ci,
           basic_lower_ci,
           basic_upper_ci,
           perc_lower_ci,
           perc_upper_ci,
           method,
           delta_limit,
           coverage,
           bootstrap_iterations,
           timestamp,
           participant_count)

  csv_path <- file.path(AGREEMENT_DIR,
    sprintf("lmm_%s_%s.csv", method_name, motion))
  write.csv(results, csv_path, row.names = FALSE)
  cat(sprintf("  Written: %s\n", csv_path))

  invisible(results)
}

# =============================================================================
# Run all 6 combinations
# =============================================================================
flex_data <- read.csv(file.path(RAW_DIR, "flexion_measurements_long.csv"),
                      stringsAsFactors = FALSE)
ext_data  <- read.csv(file.path(RAW_DIR, "extension_measurements_long.csv"),
                      stringsAsFactors = FALSE)

for (mot in c("flexion", "extension")) {
  dat <- if (mot == "flexion") flex_data else ext_data
  for (meth in c("soc", "rtmw", "hsmr")) {
    run_analysis(dat, meth, mot)
  }
}

cat("\nAll LMM agreement CSVs written to data/agreement/\n")
