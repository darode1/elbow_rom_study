# Elbow ROM Study

Validation of markerless monocular human pose estimation methods (RTMW, HSMR) and expert visual assessment for clinical measurement of elbow flexion and extension range of motion, using passive-marker optical motion capture (OMC) as the reference standard.

## Repository structure

```
code/          R analysis scripts (run in order, 00–06)
figures/       Figure notebooks (Jupyter) and rendered PDFs
data/          Input CSVs and all script outputs
requirements/  R package lists and installer script
```

## Requirements

Two R versions are needed due to a package compatibility constraint:

| R version | Used for |
|-----------|----------|
| 4.5.1 | Scripts `00`–`04`, `06` |
| 4.3.3 | Script `05` (redres diagnostics) |

Install packages from the repository root:

```bash
R-4.5.1 -f requirements/install_packages.R --args requirements/r-4.5.1-packages.txt
R-4.3.3 -f requirements/install_packages.R --args requirements/r-4.3.3-packages.txt
```

Key packages: `lme4`, `lmerTest`, `cccrm`, `boot`, `methodcompare`, `redres`, `ggplot2`, `patchwork`.

## Running the analysis

All scripts are run from the **repository root** in order. Each step writes its outputs to `data/` for the next step to consume.

```bash
# 1. Reshape raw paired-measurement CSVs into long format
R-4.5.1 -f code/00_prepare_data.R

# 2. Fit LMMs and extract fixed-effect estimates (Table 2)
R-4.5.1 -f code/01_lmm_fixed_effects.R

# 3. Concordance correlation coefficients with BCa bootstrap CIs
#    Results are cached in data/rds/ — rerun is skipped if cache exists
R-4.5.1 -f code/02_ccc_analysis.R

# 4. Agreement metrics (MSD, TDI, CP, SEM, MDC) with BCa bootstrap CIs (Table 3)
#    Takes several minutes; cached in data/rds/
R-4.5.1 -f code/03_lmm_bootstrap_metrics.R

# 5. Bootstrap histogram panels (diagnostic)
R-4.5.1 -f code/04_histogram.R

# 6. LMM residual and Q-Q diagnostic plots — Fig. S2 (requires R 4.3.3)
R-4.3.3 -f code/05_redres_diagnostics.R

# 7. Taffé bias analysis — exports data/taffe/taffe_analysis.json for figures
R-4.5.1 -f code/06_taffe_methodcompare.R
```

## Figures

After running the R scripts, open the Jupyter notebooks in `figures/` to render the manuscript figures:

| Notebook | Output |
|----------|--------|
| `01_boxplots.ipynb` | ROM and difference boxplots |
| `02_ccc.ipynb` | CCC panels |
| `03_mdc.ipynb` | MDC panels |
| `04_taffe.ipynb` | Taffé bias and precision plots |

## Data

Raw paired-measurement CSVs (`data/raw/hsmr_paired_measurements.csv`, `data/raw/rtmw_paired_measurements.csv`) are required to run the pipeline. Contact the corresponding author for access.
