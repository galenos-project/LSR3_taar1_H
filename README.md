# LSR3 findings from human studies - TAAR1 agonists in psychosis

Systematic literature review and meta-analysis examining the efficacy and safety of TAAR1 agonists (ulotaront and ralmitaront) for treating psychosis in humans. This project uses random-effects meta-analysis to compare TAAR1 agonists against placebo and current antipsychotics across outcomes including symptom reduction, functioning, cognition, and adverse events.

This project is licensed under the terms of the [Creative Commons Attribution 4.0 International license (CC-BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

## Project structure

```
LSR3_taar1_H/
├── DESCRIPTION                # Project metadata and dependencies
├── renv.lock                  # Exact package versions for reproducibility
├── LSR3_taar1_H.Rmd           # R Markdown report
├── LSR3_taar1_H.html          # Rendered HTML report
├── data/                      # Raw and cleaned data
│   ├── clean_data.R           # Data cleaning script
│   ├── LSR3_H_2024-01-22.xlsx # Raw data from EPPI-Reviewer
│   └── ...                    # Additional data files
├── util/                      # Analysis utilities
│   ├── analysis.R             # Meta-analysis script
│   └── util.R                 # Helper functions
├── lsr3_references.bib        # Literature references
└── grateful-refs.bib          # R package citations
```

## Setup

### Prerequisites

- [R](https://cran.r-project.org/) (>= 4.0)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)
- [CMake](https://cmake.org/download/) — required to build some R package dependencies
  - **macOS**: download the `.dmg` installer from the link above, or run `brew install cmake` in Terminal
  - **Windows**: download the `.msi` installer from the link above and select "Add CMake to the system PATH" during installation

### Clone the repository

```bash
git clone <repository-url>
cd LSR3_taar1_H
```

### Install R packages

This project uses [`renv`](https://rstudio.github.io/renv/) for dependency management. The `renv.lock` file records the exact package versions used to produce the published results, and the `DESCRIPTION` file declares the project's direct dependencies.

```r
# Install renv if you don't have it:
install.packages("renv")

# Restore all packages from the lock file:
renv::restore()
```

This will install every dependency at the correct version, including packages from GitHub.

## Running the analysis

The R Markdown file orchestrates the entire pipeline. When knitted, it automatically:

1. Runs `clean_data.R` to read and clean the raw Excel data.
2. Runs `analysis.R` to perform all meta-analyses, generate forest plots, and compute effect sizes.
3. Renders the full report as an HTML file with all tables, figures, and results inline.

### Option A: RStudio (recommended)

1. Open `LSR3_taar1_H.Rproj` in RStudio.
2. Open `LSR3_taar1_H.Rmd`.
3. Click **Knit** (or press `Ctrl+Shift+K` / `Cmd+Shift+K`).

### Option B: Command line

```r
# From within R, with the working directory set to the project root:
rmarkdown::render("LSR3_taar1_H.Rmd")
```

Or from the terminal:

```bash
Rscript -e 'rmarkdown::render("LSR3_taar1_H.Rmd")'
```

#LSR3 updates

- The code has had to be amended slightly, and for each update the files - and folders - have a postscript '_u[n]', where 'n' is the iteration of the update.
- Changes compared to the previous versions are reported in the first section of the R markdown files.

The current _u1 versions represent the first update of the review.
