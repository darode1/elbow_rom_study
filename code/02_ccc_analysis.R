# =============================================================================
# 02_ccc_analysis.R
# Elbow ROM Study — Concordance Correlation Coefficient (Table 3, CCC rows)
#
# Purpose:
#   Computes the CCC with BCa bootstrap confidence intervals for each
#   motion × method combination using ccc_vc() from the cccrm package.
#   Bootstrap results are cached as RDS files; the bootstrap is only run if no
#   cached file exists (or if FORCE_RERUN = TRUE below).
#
# Model:
#   ccc_vc(dataset, ry="rom", rind="participant", rmet="method", int=TRUE,
#           cl=0.95, rho=0, boot=TRUE, boot_param=FALSE, nboot=10000,
#           parallel=TRUE, workers=10, future_seed=1234)
#
# Inputs  (data/raw/):
#   flexion_measurements_long.csv
#   extension_measurements_long.csv
#
# Cache   (data/rds/):
#   ccc_{method}_{motion}_bootstrap_10000.rds   (created on first run)
#
# Outputs (data/ccc/):
#   ccc_{method}_{motion}.csv   — 6 files (soc/rtmw/hsmr × flexion/extension)
#     Columns: method, motion, ccc, lower, upper
#
# Usage:
#   R-4.5.1 -f code/02_ccc_analysis.R
#   (or run interactively from the repository root)
#
#
# To force a fresh bootstrap run regardless of cache, set:
#   FORCE_RERUN <- TRUE
# =============================================================================

library(cccrm)
library(dplyr)
library(tidyr)

FORCE_RERUN   <- FALSE
NUM_BOOTSTRAP <- 10000
NUM_WORKERS   <- 10

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
RAW_DIR <- file.path(DATA_DIR, "raw")
RDS_DIR  <- file.path(DATA_DIR, "rds")
CCC_DIR  <- file.path(DATA_DIR, "ccc")
dir.create(RDS_DIR, showWarnings = FALSE)
dir.create(CCC_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Load data ---
flex_data <- read.csv(file.path(RAW_DIR, "flexion_measurements_long.csv"),
                      stringsAsFactors = FALSE)
ext_data  <- read.csv(file.path(RAW_DIR, "extension_measurements_long.csv"),
                      stringsAsFactors = FALSE)

run_ccc <- function(long_data, method_name, motion) {
  df <- long_data %>%
    filter(method %in% c("omc", method_name)) %>%
    mutate(
      participant = as.factor(participant),
      method      = factor(method),
      method      = relevel(method, ref = "omc"),
      rom         = as.numeric(rom)
    )

  rds_path <- file.path(RDS_DIR,
    sprintf("ccc_%s_%s_bootstrap_%d.rds", method_name, motion, NUM_BOOTSTRAP))

  if (!FORCE_RERUN && file.exists(rds_path)) {
    cat(sprintf("  Loading cached CCC for %s %s ...\n", motion, method_name))
    ccc_obj <- readRDS(rds_path)
  } else {
    cat(sprintf("  Running CCC bootstrap for %s %s (%d iterations) ...\n",
                motion, method_name, NUM_BOOTSTRAP))
    ccc_obj <- ccc_vc(dataset     = df,
                      ry          = "rom",
                      rind        = "participant",
                      rmet        = "method",
                      int         = TRUE,
                      cl          = 0.95,
                      rho         = 0,
                      boot        = TRUE,
                      boot_param  = FALSE,
                      nboot       = NUM_BOOTSTRAP,
                      parallel    = TRUE,
                      workers     = NUM_WORKERS,
                      future_seed = 1234)
    saveRDS(ccc_obj, rds_path)
    cat(sprintf("  Cached: %s\n", rds_path))
  }

  out <- data.frame(
    method = method_name,
    motion = motion,
    ccc    = ccc_obj$ccc["CCC"],
    lower  = ccc_obj$ccc["LL CI 95%"],
    upper  = ccc_obj$ccc["UL CI 95%"],
    row.names = NULL
  )

  csv_path <- file.path(CCC_DIR,
    sprintf("ccc_%s_%s.csv", method_name, motion))
  write.csv(out, csv_path, row.names = FALSE)
  cat(sprintf("  Written: %s\n", csv_path))

  invisible(out)
}

# --- Run all 6 combinations ---
for (mot in c("flexion", "extension")) {
  dat <- if (mot == "flexion") flex_data else ext_data
  for (meth in c("soc", "rtmw", "hsmr")) {
    run_ccc(dat, meth, mot)
  }
}

cat("\nAll CCC CSVs written to data/ccc/\n")
