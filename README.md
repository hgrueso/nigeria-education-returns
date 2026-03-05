# Education, Child Marriage, and Heterogeneous Wage Returns in Nigeria

**Authors:** Hernando Grueso, William Rudgard, Ritapriya Bandyopadhyay, Chris Desmond, Brendan Maughan-Brown, Lucie Cluver

**Date:** February 2026

## Overview

This repository contains the replication code for the paper *"Education, Child Marriage, and Heterogeneous Wage Returns in Nigeria"*. Using the 2018–19 Nigeria Living Standards Survey (NLSS), we estimate Mincerian wage models with state fixed effects, Heckman selection correction, and causal forests to analyse returns to education for women, and how child marriage moderates those returns.

### Key Findings

- Each additional year of schooling raises wages by ~15% in both national and Northern Nigeria samples, with **convex** returns.
- Child marriage (before age 18) is associated with a ~15% wage penalty.
- Nationally, the interaction is **significant** (p<0.001): child marriage reduces both wage levels *and* returns to education.
- In Northern Nigeria, child marriage operates as a **pure level shift** (interaction p=0.459), with returns staying ~15% regardless of marriage age.
- Selection into wage employment is significant nationally (Heckman λ=0.822, p<0.05) but not in Northern Nigeria.

## Repository Structure

```
.
├── README.md
├── .gitignore
├── code/
│   └── 03_analysis_NLSS_Final.R          # Main analysis script
├── data/
│   ├── .gitkeep
│   └── NLSS_analysis.dta                 # NLSS microdata (not tracked)
├── outputs/
│   ├── .gitkeep
│   └── (generated files)                 # Created by R script
├── slides_nigeria.qmd                    # Beamer presentation slides
└── tables_and_figures.qmd                # Publication-ready tables & figures PDF
```

## Data Access

The analysis uses the **Nigeria Living Standards Survey (NLSS) 2018–19** microdata. The data file (`data/NLSS_analysis.dta`) is **not included** in this repository due to data use restrictions.

To obtain the data:
1. Visit the [Nigeria National Bureau of Statistics](https://www.nigerianstat.gov.ng/) or the [World Bank Microdata Library](https://microdata.worldbank.org/)
2. Download the NLSS 2018–19 household and individual datasets
3. The pre-processed analysis file (`NLSS_analysis.dta`) will be shared separately with co-authors via a secure link

## Requirements

### Software

- **R** >= 4.2.0
- **Quarto** >= 1.4 (for rendering slides and tables PDF)
- **LaTeX** distribution (e.g., TinyTeX, TeX Live, or MiKTeX) for PDF output

### R Packages

Install all required packages by running:

```r
install.packages(c(
  "tidyverse",
  "haven",
  "modelsummary",
  "kableExtra",
  "sampleSelection",
  "marginaleffects",
  "grf",
  "survey",
  "flextable",
  "officer",
  "patchwork"
))
```

If you don't have a LaTeX distribution, install TinyTeX from R:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Replication Instructions

### 1. Clone the repository

```bash
git clone https://github.com/hgrueso/nigeria-education-returns.git
cd nigeria-education-returns
```

### 2. Add the data

Place `NLSS_analysis.dta` in the `data/` folder (the directory already exists in the repo):

```bash
cp /path/to/NLSS_analysis.dta data/
```

### 3. Run the analysis

The R script runs all models (OLS, Heckman, probit, causal forest) for both National and Northern Nigeria samples, and saves results to `outputs/analysis_results.RData`:

```bash
Rscript code/03_analysis_NLSS_Final.R
```

This takes approximately 5-10 minutes depending on your machine (the causal forest grows 4,000 trees per sample).

### 4. Render the outputs

**Tables and Figures PDF** (for paper submission):
```bash
quarto render tables_and_figures.qmd
```
Produces `tables_and_figures.pdf` with 23 tables and 10 figures, one per page.

**Presentation slides** (beamer PDF):
```bash
quarto render slides_nigeria.qmd
```
Produces `slides_nigeria.pdf`, a slide deck summarising the main results.

### Full pipeline (one command)

```bash
Rscript code/03_analysis_NLSS_Final.R && \
  quarto render tables_and_figures.qmd && \
  quarto render slides_nigeria.qmd
```

## Methods Summary

| Method | Purpose |
|---|---|
| Quadratic Mincer | Returns to education with convexity, child marriage penalty, interaction |
| Linear Mincer | Robustness check with linear schooling term |
| Education levels | Returns by primary/secondary/higher (vs. none) |
| Literacy | Binary literacy premium |
| Heckman selection | Two-step correction for selection into wage employment |
| Probit + AME | First-stage selection model with average marginal effects |
| Causal forest | Heterogeneous treatment effects of education on wages |

All wage models include state fixed effects, urban/rural controls, and survey weights. The probit additionally controls for parental education (factor variables).

## Variable Coding

Key variable definitions used in the analysis:

| Variable | Coding | Source |
|---|---|---|
| `sector` | 0 = Rural, 1 = Urban | NLSS original |
| `is_rural` | 1 if sector == 0, else 0 | Derived |
| `child_marriage` | 1 if married before age 18 | Derived from age at first marriage |
| `religion` | 1 = Islam, 2 = Christianity, 3+ = Other | NLSS original |
| `father_edu_cat` / `mother_edu_cat` | 0 = None, 1-2 = Primary, 3+ = Secondary+ | NLSS original |

## Output Files

| File | Description |
|---|---|
| `tables_and_figures.pdf` | All tables (1-23) and figures (1-10) for paper |
| `slides_nigeria.pdf` | Presentation slides |
| `outputs/analysis_results.RData` | All model objects, data frames, plots |
| `outputs/Table_1_*.docx` | Descriptive stats (wage earners) -- Word format |
| `outputs/Table_A1_*.docx` | Descriptive stats (full sample) -- Word format |

## Troubleshooting

**`cannot open the connection` when rendering QMD files:**
You must run the R script first (`Rscript code/03_analysis_NLSS_Final.R`). The QMD files depend on `outputs/analysis_results.RData` which is generated by the script. Always run in the order: R script -> tables QMD -> slides QMD.

**`.docx` export warnings (`Requested Pandoc binary`):**
This means Pandoc is not installed system-wide. Quarto bundles its own Pandoc, but the R script calls `modelsummary()` which looks for a system Pandoc. These warnings are harmless -- the `.html` tables and `.RData` are still saved correctly.

**Quarto cross-reference warnings (`Raw LaTeX table`):**
These are cosmetic. They appear because we use `kableExtra`/`modelsummary` to generate LaTeX tables directly, bypassing Quarto's native cross-referencing. Tables still render correctly with proper numbering.

## License

This code is shared for replication purposes among co-authors. Please do not distribute without permission.

## Contact

For questions about the code or data, contact Hernando Grueso at hernando@aimpactlab.com.
