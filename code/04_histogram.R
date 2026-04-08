# =============================================================================
# 04_histogram.R
# Elbow ROM Study — BCa Histogram Panels from Bootstrap Results
#
# Purpose:
#   Recreates the per-metric histogram panels used in the exploratory notebook
#   workflow, using cached bootstrap results and final agreement-index CSVs.
#
#   For each motion × method combination, this script builds one combined PDF
#   containing histograms for:
#     msd, tdi, cp, sem, mdc, beta
#
#   Vertical lines are overlaid for:
#     - deterministic point estimate (black)
#     - percentile CI (red dashed)
#     - BCa CI (green dashed)
#
# Inputs  (data/):
#   rds/{motion}_{method}_boot_result.rds
#
# Inputs  (data/agreement/):
#   lmm_{method}_{motion}.csv
#
# Outputs (data/plots/):
#   {motion}_{method}_histograms_bca.pdf   — 6 files
#
# Usage:
#   R-4.5.1 -f code/04_histogram.R
#   (or run interactively from the repository root)
#
# Command-line R versions:
#   00–04 scripts: R-4.5.1
#   redres diagnostics: R-4.3.3
# =============================================================================

library(ggplot2)
library(patchwork)

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

DATA_DIR <- file.path(PROJECT_ROOT, "data")
RDS_DIR  <- file.path(DATA_DIR, "rds")
AGREEMENT_DIR <- file.path(DATA_DIR, "agreement")
OUT_DIR  <- file.path(DATA_DIR, "plots")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

metric_labels <- c(
  msd  = "Mean Squared Deviation",
  tdi  = "Total Deviation Index",
  cp   = "Coverage Probability",
  sem  = "Standard Error of Measurement",
  mdc  = "Minimal Detectable Change",
  beta = "Fixed Effect Coefficient"
)

plot_metric_hist <- function(boot_result, ci_df, metric_name, metric_index) {
  hist_data <- data.frame(value = boot_result$t[, metric_index])

  point_val <- ci_df$point_estimate[ci_df$metric == metric_name]
  perc_l    <- ci_df$perc_lower_ci[ci_df$metric == metric_name]
  perc_u    <- ci_df$perc_upper_ci[ci_df$metric == metric_name]
  bca_l     <- ci_df$bca_lower_ci[ci_df$metric == metric_name]
  bca_u     <- ci_df$bca_upper_ci[ci_df$metric == metric_name]

  vline_data <- data.frame(
    xintercept = c(point_val, perc_l, perc_u, bca_l, bca_u),
    label      = c("Point Estimate", "Perc. CI", "Perc. CI", "BCa CI", "BCa CI"),
    linetype   = c("Point Estimate", "Confidence Interval", "Confidence Interval",
                   "Confidence Interval", "Confidence Interval")
  )

  ggplot(hist_data, aes(x = value)) +
    geom_histogram(bins = 50, fill = "lightblue", color = "black", alpha = 0.8) +
    geom_vline(
      data = vline_data,
      aes(xintercept = xintercept, color = label, linetype = linetype),
      linewidth = 1
    ) +
    scale_color_manual(
      values = c("Point Estimate" = "black", "Perc. CI" = "red", "BCa CI" = "green"),
      name = "Legend"
    ) +
    scale_linetype_manual(
      values = c("Point Estimate" = "solid", "Confidence Interval" = "dashed"),
      guide = "none"
    ) +
    labs(
      title = metric_labels[[metric_name]],
      x = metric_labels[[metric_name]],
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

run_one <- function(motion, method_name) {
  rds_path <- file.path(RDS_DIR, sprintf("%s_%s_boot_result.rds", motion, method_name))
  ci_path  <- file.path(AGREEMENT_DIR, sprintf("lmm_%s_%s.csv", method_name, motion))

  if (!file.exists(rds_path)) stop(sprintf("Missing RDS file: %s", rds_path))
  if (!file.exists(ci_path))  stop(sprintf("Missing CI CSV: %s", ci_path))

  boot_result <- readRDS(rds_path)
  ci_df       <- read.csv(ci_path, stringsAsFactors = FALSE)

  metric_order <- c("msd", "tdi", "cp", "sem", "mdc", "beta")
  required_n   <- length(metric_order)
  n_available  <- ncol(boot_result$t)

  if (is.null(n_available) || n_available < required_n) {
    missing_count <- if (is.null(n_available)) required_n else required_n - n_available
    stop(sprintf(
      "RDS file %s has %d/%d required metrics (missing %d).",
      rds_path,
      ifelse(is.null(n_available), 0, n_available),
      required_n,
      missing_count
    ))
  }

  metric_names <- metric_order

  plots <- lapply(seq_along(metric_names), function(i) {
    plot_metric_hist(boot_result, ci_df, metric_names[i], i)
  })

  combined <- wrap_plots(plots, ncol = 3) +
    plot_annotation(title = sprintf("%s %s histograms", tools::toTitleCase(motion), toupper(method_name)))

  out_path <- file.path(OUT_DIR, sprintf("%s_%s_histograms_bca.pdf", motion, method_name))
  ggsave(out_path, plot = combined, width = 12, height = 8)
  cat(sprintf("Written: %s\n", out_path))
}

for (mot in c("flexion", "extension")) {
  for (meth in c("soc", "rtmw", "hsmr")) {
    run_one(mot, meth)
  }
}

cat("\nAll BCa histogram PDFs written to data/plots/\n")
