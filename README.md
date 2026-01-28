# Alamar Biomarker QC and Analysis

R code for quality control and analysis of Alamar biomarker data.

## Repository Structure

```
├── Primary_QC/           # Primary quality control analyses
├── Replicate_Analysis/   # Replicate sample concordance and analysis
├── U19_Analysis/         # U19 grant-specific analyses
└── Utilities/            # Shared scripts for data merging and filtering
```

## Projects

### Primary_QC
Primary quality control pipeline for biomarker data. Open `Primary_QC/Primary_QC.Rproj` in RStudio.

### Replicate_Analysis
Analysis of replicate samples for assessing reproducibility. Open `Replicate_Analysis/Replicate_Analysis.Rproj` in RStudio.

### U19_Analysis
Analyses specific to the U19 grant objectives. Open `U19_Analysis/U19_Analysis.Rproj` in RStudio.

### Utilities
Shared R scripts used across projects:
- **File_Merge.qmd** - Merges post-QC NPQ files with metadata, generates exclusion reports for duplicates and implausible values
- **Apply_Filters.qmd** - Applies sample exclusion filters to merged datasets

## Getting Started

1. Clone this repository
2. Open the relevant `.Rproj` file in RStudio for the analysis you want to run
3. Each project folder has its own `input_files/` and `output_files/` directories (not tracked in git)

## Data Files

Input and output data files are excluded from version control (see `.gitignore`). Each project expects:
- `input_files/` - Place your input data here
- `output_files/` - Generated outputs will be saved here

## Requirements

- R (>= 4.0)
- RStudio (recommended)
- Required packages: tidyverse, knitr, kableExtra
