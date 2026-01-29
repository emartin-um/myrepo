# Alamar Biomarker QC and Analysis

R code for quality control and analysis of Alamar biomarker data for the U19 Alzheimer's Disease research project.

## Repository Structure

```
├── Primary_QC/           # Primary quality control analyses
├── Replicate_Analysis/   # Replicate sample concordance and CV analysis
├── U19_Analysis/         # U19 grant-specific association analyses
└── Utilities/            # Shared functions and helper scripts
```

## Projects

### Primary_QC
Primary quality control pipeline for biomarker data. Filters samples and biomarkers based on read counts, detectability, and LOD criteria.

**Scripts:**
- `QC_Pipeline_Primary_ALZ123_12_20_25.Rmd` - Main QC pipeline for ALZ1/2/3 data

Open `Primary_QC/Primary_QC.Rproj` in RStudio.

### Replicate_Analysis
Analysis of replicate samples for assessing assay reproducibility and concordance.

**Scripts:**
- `QC_Pipeline_Replicates_ALZ_12_29_25.Rmd` - CV analysis and replicate concordance

Open `Replicate_Analysis/Replicate_Analysis.Rproj` in RStudio.

### U19_Analysis
Statistical analyses specific to the U19 grant objectives, including CDX associations and interaction testing.

**Scripts:**
- `Assoc_Analysis_univariate_HighBiomarkers_12_19_25.Rmd` - Univariate association tests
- `Assoc_Analysis_Interaction_HighBiomarkers_12_31_25_1.Rmd` - CDX interaction analyses

Open `U19_Analysis/U19_Analysis.Rproj` in RStudio.

### Utilities
Shared R functions used across projects.

**Scripts:**
- `shared_functions.R` - Common utility functions (summarization, outlier detection, CV calculation, plotting helpers)

## Workflow

The recommended analysis order is:

```
1. Primary_QC
   └── Produces: NPQ_post_QC.csv, biomarker/sample summaries

2. Replicate_Analysis (optional, for QC assessment)
   └── Produces: CV statistics, concordance reports

3. U19_Analysis
   a. Univariate analysis first
      └── Produces: biomarker_groups.csv, meta_plus_race_dx.csv
   b. Interaction analysis second
      └── Produces: association results, interaction plots
```

## Getting Started

1. Clone this repository
2. Open the relevant `.Rproj` file in RStudio for the analysis you want to run
3. Create `input_files/` directory in the project folder if it doesn't exist
4. Place your input data in `input_files/`
5. Run the Rmd scripts; outputs will be saved to `output_files/`

## Data Files

Input and output data files are excluded from version control (see `.gitignore`). Each project expects:
- `input_files/` - Place your input data here
- `output_files/` - Generated outputs will be saved here

## Requirements

### R Version
- R >= 4.0
- RStudio (recommended)

### Required Packages

Install all required packages with:

```r
install.packages(c(
  # Core data manipulation
  "tidyverse",
  "readr",
  "dplyr",
  "tidyr",
  "purrr",


  # Statistical analysis
  "broom",
  "emmeans",
  "car",
  "caret",

  # Visualization
  "ggplot2",
  "pheatmap",
  "patchwork",
  "gridExtra",

  # Tables and reporting
  "knitr",
  "kableExtra",

  # Other utilities
  "poibin",
  "irlba"
))
```

### Package Summary by Project

| Package | Primary_QC | Replicate | U19 | Description |
|---------|:----------:|:---------:|:---:|-------------|
| tidyverse | x | x | x | Core data manipulation |
| pheatmap | x | | x | Heatmap visualization |
| caret | x | | x | ML utilities (for clustering) |
| kableExtra | x | x | x | HTML table formatting |
| emmeans | | | x | Estimated marginal means |
| car | | | x | Type II ANOVA |
| broom | | | x | Model tidying |
| patchwork | | x | x | Plot composition |
| gridExtra | | | x | Arrange multiple plots |
| poibin | x | | | Poisson binomial distribution |
| irlba | x | | | Fast SVD/PCA |

## Using Shared Functions

To use the shared utility functions in your Rmd files, add this to your setup chunk:

```r
source("../Utilities/shared_functions.R")
```

Available functions include:
- `summarize_counts()` - Count samples by Run/Bay
- `summarize_df_pretty()` - Create summary tables
- `protect_name()` - Safely use variable names in formulas
- `identify_outliers()` - Detect outliers (IQR/MAD/zscore)
- `cv()` - Calculate coefficient of variation
- `calculate_cv_stats()` - CV statistics for replicates
- `plot_biomarker_violins()` - Violin plots for distributions

See `Utilities/README.md` for full documentation.

## Contributing

When adding new scripts:
1. Use the shared functions from `Utilities/shared_functions.R` where possible
2. Document parameters at the top of each Rmd file
3. Update the relevant README with script descriptions
4. Keep input/output file paths relative to the project folder
