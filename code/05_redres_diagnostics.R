# =============================================================================
# 05_redres_diagnostics.R
# Elbow ROM Study — redres Residual and Q-Q Diagnostics
#
# Purpose:
#   Fits the same LMM used in the agreement analysis for each motion × method
#   pair and exports combined diagnostic panels using redres:
#     - residuals vs fitted (plot_redres)
#     - residual Q-Q (plot_resqq)
#     - random-effects Q-Q (plot_ranef)
#
#   This script is intended to be run with an older R setup that supports the
#   redres package version used in the project.
#
# Model:
#   rom ~ method + (1 | participant) + (1 | participant:method)
#
# Inputs  (data/raw/):
#   flexion_measurements_long.csv
#   extension_measurements_long.csv
#
# Outputs (data/plots/):
#   {motion}_{method}_redres_diagnostics.pdf   — 6 files
#
# Usage:
#   R-4.3.3 -f code/05_redres_diagnostics.R
#   (or run interactively from the repository root)
# =============================================================================

library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(patchwork)

if (!requireNamespace("redres", quietly = TRUE)) {
  stop("Package 'redres' is required for this script. Run this script in the older R environment where redres is available.")
}

plot_redres <- redres::plot_redres
plot_resqq  <- redres::plot_resqq
plot_ranef  <- redres::plot_ranef

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
RAW_DIR  <- file.path(DATA_DIR, "raw")
OUT_DIR  <- file.path(DATA_DIR, "plots")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

fit_one <- function(long_data, method_name) {
  df <- long_data %>%
    filter(method %in% c("omc", method_name)) %>%
    mutate(
      participant = as.factor(participant),
      method      = as.character(method),
      method      = ifelse(method == method_name, "compare", method),
      method      = factor(method),
      method      = relevel(method, ref = "omc"),
      rom         = as.numeric(rom),
      method      = droplevels(method)
    )

  lmer(
    rom ~ method + (1 | participant) + (1 | participant:method),
    data    = df,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

run_one <- function(motion, method_name, long_data) {
  fit <- fit_one(long_data, method_name)

  p1 <- plot_redres(fit)
  p2 <- plot_resqq(fit)
  p3 <- plot_ranef(fit)

  combined <- p1 + p2 + p3 +
    plot_annotation(
      title = sprintf("%s %s model diagnostics",
                      tools::toTitleCase(motion), toupper(method_name)),
      subtitle = "Residuals vs fitted, residual Q-Q, and random-effects Q-Q"
    )

  out_path <- file.path(OUT_DIR, sprintf("%s_%s_redres_diagnostics.pdf", motion, method_name))
  ggsave(out_path, plot = combined, width = 12, height = 4)
  cat(sprintf("Written: %s\n", out_path))
}

flex_data <- read.csv(file.path(RAW_DIR, "flexion_measurements_long.csv"),
                      stringsAsFactors = FALSE)
ext_data  <- read.csv(file.path(RAW_DIR, "extension_measurements_long.csv"),
                      stringsAsFactors = FALSE)

for (mot in c("flexion", "extension")) {
  dat <- if (mot == "flexion") flex_data else ext_data
  for (meth in c("soc", "rtmw", "hsmr")) {
    run_one(mot, meth, dat)
  }
}

cat("\nAll redres diagnostic PDFs written to data/plots/\n")
