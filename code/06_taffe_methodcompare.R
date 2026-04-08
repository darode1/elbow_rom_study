# =============================================================================
# 06_taffe_methodcompare.R
# Elbow ROM Study — Taffé Method-Comparison Analysis Export
#
# Purpose:
#   Runs MethodCompare::measure_compare for each motion × method combination
#   (soc/rtmw/hsmr vs omc) using paired raw measurements and exports a JSON
#   payload compatible with figures/04_taffe.ipynb.
#
# Inputs  (data/raw/):
#   hsmr_paired_measurements.csv
#   rtmw_paired_measurements.csv
#
# Outputs (data/taffe/):
#   taffe_analysis.json
#
# JSON layout:
#   {
#     flexion:   { hsmr: {...}, rtmw: {...}, soc: {...} },
#     extension: { hsmr: {...}, rtmw: {...}, soc: {...} }
#   }
#
#   Each method object contains:
#     bias, data, models(model_2, model_4), methods, sim_params, nb_simul
#
# Usage:
#   R-4.5.1 -f code/06_taffe_methodcompare.R
#   (or run interactively from the repository root)
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(jsonlite)

if (!requireNamespace("MethodCompare", quietly = TRUE)) {
  stop("Package 'MethodCompare' is required. Install it before running this script.")
}

# --- Configuration ---
NB_SIMUL <- 10000

# --- Paths ---
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
TAFFE_DIR     <- file.path(DATA_DIR, "taffe")
OUT_JSON_PATH <- file.path(TAFFE_DIR, "taffe_analysis.json")
dir.create(TAFFE_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Load raw paired data ---
data_hsmr <- read_csv(file.path(RAW_DIR, "hsmr_paired_measurements.csv"), show_col_types = FALSE)
data_rtmw <- read_csv(file.path(RAW_DIR, "rtmw_paired_measurements.csv"), show_col_types = FALSE)

# =============================================================================
# Build joined full dataset for one motion
# =============================================================================
build_full_data <- function(motion_name) {
  hsmr_motion <- data_hsmr %>%
    filter(motion == motion_name, camera == "straight")

  rtmw_motion <- data_rtmw %>%
    filter(motion == motion_name, camera == "straight")

  omc_soc_hsmr <- hsmr_motion %>%
    select(participant, rep, vicon, soc, hpe) %>%
    rename(
      omc_angle  = vicon,
      soc_angle  = soc,
      hsmr_angle = hpe
    )

  rtmw_only <- rtmw_motion %>%
    select(participant, rep, hpe) %>%
    rename(rtmw_angle = hpe)

  inner_join(omc_soc_hsmr, rtmw_only, by = c("participant", "rep"))
}

# =============================================================================
# Run one MethodCompare model and convert to JSON-friendly list
# =============================================================================
run_methodcompare <- function(df, new_col) {
  analysis_df <- df %>%
    select(participant, omc_angle, all_of(new_col)) %>%
    mutate(participant = as.factor(participant))

  comp <- MethodCompare::measure_compare(
    data = analysis_df,
    new  = new_col,
    ref  = "omc_angle",
    id   = "participant",
    nb_simul = NB_SIMUL
  )

  model_2_coefs <- as.list(comp$models[[2]]$coefficients)
  model_4_coefs <- as.list(comp$models[[4]]$coefficients)

  list(
    bias = comp$bias,
    data = comp$data,
    models = list(
      model_2 = model_2_coefs,
      model_4 = model_4_coefs
    ),
    methods = comp$methods,
    sim_params = comp$sim_params,
    nb_simul = NB_SIMUL
  )
}

# =============================================================================
# Run all 6 combinations
# =============================================================================
flexion_full_data   <- build_full_data("flexion")
extension_full_data <- build_full_data("extension")

cat("Running MethodCompare for flexion ...\n")
flex_hsmr <- run_methodcompare(flexion_full_data, "hsmr_angle")
flex_rtmw <- run_methodcompare(flexion_full_data, "rtmw_angle")
flex_soc  <- run_methodcompare(flexion_full_data, "soc_angle")

cat("Running MethodCompare for extension ...\n")
ext_hsmr <- run_methodcompare(extension_full_data, "hsmr_angle")
ext_rtmw <- run_methodcompare(extension_full_data, "rtmw_angle")
ext_soc  <- run_methodcompare(extension_full_data, "soc_angle")

export_data <- list(
  flexion = list(
    hsmr = flex_hsmr,
    rtmw = flex_rtmw,
    soc  = flex_soc
  ),
  extension = list(
    hsmr = ext_hsmr,
    rtmw = ext_rtmw,
    soc  = ext_soc
  )
)

write_json(export_data, OUT_JSON_PATH, pretty = TRUE, auto_unbox = TRUE, digits = 10)
cat(sprintf("Written: %s\n", OUT_JSON_PATH))
