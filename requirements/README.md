# R package requirements

This folder is the R equivalent of a `requirements.txt` setup for this project.

## Files

- `r-4.5.1-packages.txt`: packages needed for scripts `00` to `04`
- `r-4.3.3-packages.txt`: packages needed for script `05_redres_diagnostics.R`
- `install_packages.R`: installs any missing packages from one package list

## Install commands

From the repository root:

```bash
R-4.5.1 -f requirements/install_packages.R --args requirements/r-4.5.1-packages.txt
R-4.3.3 -f requirements/install_packages.R --args requirements/r-4.3.3-packages.txt
```

## Notes

- Uses CRAN mirror: `https://cloud.r-project.org`
- Existing packages are not reinstalled.
- If a package is unavailable for your OS/R version, install system dependencies first (common for `lme4`).
