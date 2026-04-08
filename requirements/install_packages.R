# Copy/paste one command based on your R version:
# R-4.5.1 -f requirements/install_packages.R --args requirements/r-4.5.1-packages.txt
# R-4.3.3 -f requirements/install_packages.R --args requirements/r-4.3.3-packages.txt

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: R-<version> -f requirements/install_packages.R --args requirements/<package-file>.txt")
}

pkg_file <- args[[1]]
if (!file.exists(pkg_file)) {
  stop(sprintf("Package file not found: %s", pkg_file))
}

pkgs <- readLines(pkg_file, warn = FALSE)
pkgs <- trimws(pkgs)
pkgs <- pkgs[nzchar(pkgs)]
pkgs <- unique(pkgs)

if (length(pkgs) == 0) {
  stop(sprintf("No packages listed in: %s", pkg_file))
}

repos <- "https://cloud.r-project.org"
installed <- rownames(installed.packages())
missing <- setdiff(pkgs, installed)

if (length(missing) == 0) {
  cat("All packages already installed.\n")
} else {
  cat("Installing missing packages:\n")
  cat(paste0(" - ", missing, collapse = "\n"), "\n", sep = "")
  install.packages(missing, repos = repos)
}

cat("\nFinal package status:\n")
for (p in pkgs) {
  cat(sprintf(" - %s: %s\n", p, if (p %in% rownames(installed.packages())) "installed" else "missing"))
}
