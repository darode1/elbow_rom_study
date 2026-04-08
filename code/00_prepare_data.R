# =============================================================================
# 00_prepare_data.R
# Elbow ROM Study — Data Preparation
#
# Purpose:
#   Takes raw paired-measurement CSV files (one row per participant/repetition/
#   camera angle) and produces the long-format data files used by downstream
#   analysis scripts.
#
# Inputs  (data/raw/):
#   hsmr_paired_measurements.csv
#   rtmw_paired_measurements.csv
#
# Outputs (data/raw/):
#   flexion_measurements_long.csv   — long format: participant, method, rom
#   extension_measurements_long.csv — long format: participant, method, rom
#
# Usage:
#   R-4.5.1 -f code/00_prepare_data.R
#   (or run interactively from the repository root)
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)

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

DATA_DIR <- file.path(PROJECT_ROOT, "data")
RAW_DIR  <- file.path(DATA_DIR, "raw")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

stale_wide_files <- c(
  file.path(RAW_DIR, "flexion_measurements_wide.csv"),
  file.path(RAW_DIR, "extension_measurements_wide.csv")
)
stale_existing <- stale_wide_files[file.exists(stale_wide_files)]
if (length(stale_existing) > 0) {
  unlink(stale_existing)
  cat("Removed stale wide files:", paste(basename(stale_existing), collapse = ", "), "\n")
}

hsmr_raw <- read_csv(file.path(RAW_DIR, "hsmr_paired_measurements.csv"),
                     show_col_types = FALSE)
rtmw_raw <- read_csv(file.path(RAW_DIR, "rtmw_paired_measurements.csv"),
                     show_col_types = FALSE)

# =============================================================================
# FLEXION
# =============================================================================
hsmr_flex <- hsmr_raw %>% filter(motion == "flexion", camera == "straight")
rtmw_flex <- rtmw_raw %>% filter(motion == "flexion", camera == "straight")

omc_soc_hsmr <- hsmr_flex %>%
  select(participant, rep, vicon, soc, hpe) %>%
  rename(omc = vicon, hsmr = hpe)

rtmw_only <- rtmw_flex %>%
  select(participant, rep, hpe) %>%
  rename(rtmw = hpe)

flexion_wide <- inner_join(omc_soc_hsmr, rtmw_only, by = c("participant", "rep")) %>%
  select(participant, rep, omc, soc, hsmr, rtmw)

flexion_long <- flexion_wide %>%
  pivot_longer(cols = c(omc, soc, hsmr, rtmw),
               names_to  = "method",
               values_to = "rom") %>%
  select(participant, method, rom)

write.csv(flexion_long, file.path(RAW_DIR, "flexion_measurements_long.csv"),
          row.names = FALSE)
cat("Written: flexion_measurements_long.csv\n")

# =============================================================================
# EXTENSION
# =============================================================================
hsmr_ext <- hsmr_raw %>% filter(motion == "extension", camera == "straight")
rtmw_ext <- rtmw_raw %>% filter(motion == "extension", camera == "straight")

omc_soc_hsmr_ext <- hsmr_ext %>%
  select(participant, rep, vicon, soc, hpe) %>%
  rename(omc = vicon, hsmr = hpe)

rtmw_only_ext <- rtmw_ext %>%
  select(participant, rep, hpe) %>%
  rename(rtmw = hpe)

extension_wide <- inner_join(omc_soc_hsmr_ext, rtmw_only_ext,
                             by = c("participant", "rep")) %>%
  select(participant, rep, omc, soc, hsmr, rtmw)

extension_long <- extension_wide %>%
  pivot_longer(cols = c(omc, soc, hsmr, rtmw),
               names_to  = "method",
               values_to = "rom") %>%
  select(participant, method, rom)

write.csv(extension_long, file.path(RAW_DIR, "extension_measurements_long.csv"),
          row.names = FALSE)
cat("Written: extension_measurements_long.csv\n")
