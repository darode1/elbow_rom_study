# =============================================================================
# 01_lmm_fixed_effects.R
# Elbow ROM Study — LMM Fixed Effects (Table 2)
#
# Purpose:
#   Fits a linear mixed model (LMM) for each motion × method combination and
#   extracts the fixed-effect estimates reported in Table 2 of the paper.
#   This script is fully deterministic (no bootstrap) and runs in seconds.
#
# Model:
#   rom ~ method + (1 | participant) + (1 | participant:method)
#   method is releveled to "omc"; the fixed-effect coefficient for "methodcompare"
#   is the mean difference (HPE − OMC) in degrees.
#
# Inputs  (data/raw/):
#   flexion_measurements_long.csv
#   extension_measurements_long.csv
#
# Outputs (data/fixed_effects/):
#   lmm_fixed_effects.csv
#     Columns: motion, method, beta, se, t_value, p_value,
#               sigma_alpha, sigma_alpha_beta, r2_marginal, r2_conditional
#
# Usage:
#   R-4.5.1 -f code/01_lmm_fixed_effects.R
#   (or run interactively from the repository root)
#
# =============================================================================

library(lme4)
library(lmerTest)
library(MuMIn)
library(dplyr)

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

DATA_DIR          <- file.path(PROJECT_ROOT, "data")
RAW_DIR           <- file.path(DATA_DIR, "raw")
FIXED_EFFECTS_DIR <- file.path(DATA_DIR, "fixed_effects")
dir.create(FIXED_EFFECTS_DIR, showWarnings = FALSE, recursive = TRUE)

fit_lmm <- function(long_data, method_name) {
  df <- long_data %>%
    filter(method %in% c("omc", method_name)) %>%
    mutate(
      participant = as.factor(participant),
      method      = factor(method),
      method      = relevel(method, ref = "omc"),
      rom         = as.numeric(rom)
    )

  fit <- lmer(rom ~ method + (1 | participant) + (1 | participant:method),
              data    = df,
              control = lmerControl(optimizer = "bobyqa",
                                    optCtrl   = list(maxfun = 2e5)))

  coef_tab  <- coef(summary(fit))
  method_row <- rownames(coef_tab)[grepl("method", rownames(coef_tab))]

  beta  <- coef_tab[method_row, "Estimate"]
  se    <- coef_tab[method_row, "Std. Error"]
  tval  <- coef_tab[method_row, "t value"]
  pval  <- coef_tab[method_row, "Pr(>|t|)"]

  vc            <- as.data.frame(VarCorr(fit))
  sigma_alpha   <- sqrt(vc[vc$grp == "participant",        "vcov"])
  sigma_ab      <- sqrt(vc[vc$grp == "participant:method", "vcov"])
  sigma_epsilon <- sigma(fit)

  r2 <- r.squaredGLMM(fit)

  data.frame(
    beta             = beta,
    se               = se,
    t_value          = tval,
    p_value          = pval,
    sigma_alpha      = sigma_alpha,
    sigma_alpha_beta = sigma_ab,
    sigma_epsilon    = sigma_epsilon,
    r2_marginal      = r2[1, "R2m"],
    r2_conditional   = r2[1, "R2c"],
    stringsAsFactors = FALSE
  )
}

# Load data
flex_data <- read.csv(file.path(RAW_DIR, "flexion_measurements_long.csv"),
                      stringsAsFactors = FALSE)
ext_data  <- read.csv(file.path(RAW_DIR, "extension_measurements_long.csv"),
                      stringsAsFactors = FALSE)

methods <- c("soc", "hsmr", "rtmw")
rows    <- list()

for (m in methods) {
  cat(sprintf("  Fitting flexion %s ...\n", m))
  r <- fit_lmm(flex_data, m)
  rows[[length(rows) + 1]] <- cbind(motion = "flexion", method = m, r)

  cat(sprintf("  Fitting extension %s ...\n", m))
  r <- fit_lmm(ext_data, m)
  rows[[length(rows) + 1]] <- cbind(motion = "extension", method = m, r)
}

results <- do.call(rbind, rows)
rownames(results) <- NULL

out_path <- file.path(FIXED_EFFECTS_DIR, "lmm_fixed_effects.csv")
write.csv(results, out_path, row.names = FALSE)
cat(sprintf("Written: %s\n", out_path))
print(results)
