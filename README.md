# Alamar Biomarker QC and Analysis

R code for quality control and analysis of Alamar biomarker data for the U19 Alzheimer's Disease research project.

## Repository Structure

```
├── Primary_QC/           # Primary quality control analyses
├── Metadata_Merge/       # Merge biomarker data with metadata
├── Replicate_Analysis/   # Replicate sample concordance and CV analysis
├── Hemolysis_Check/      # Hemolysis marker analysis by site
├── U19_Analysis/         # U19 grant-specific association analyses
├── Clustering/           # PCA and clustering analyses
└── Utilities/            # Shared functions and helper scripts
```

## Projects

### Primary_QC
Primary quality control pipeline for biomarker data. Filters samples and biomarkers based on read counts, detectability, and LOD criteria.

**Scripts:**
- `QC_Pipeline_Primary_Alamar.Rmd` - Main QC pipeline

Open `Primary_QC/Primary_QC.Rproj` in RStudio.

### Metadata_Merge
Merges post-QC biomarker data with sample metadata. Identifies and handles duplicates and implausible values.

**Scripts:**
- `Metadata_Merge_Pipeline.qmd` - Main pipeline (generates HTML report)

Open `Metadata_Merge/Metadata_Merge.Rproj` in RStudio and render the qmd file.

### Replicate_Analysis
Analysis of replicate samples for assessing assay reproducibility and concordance.

**Scripts:**
- `QC_Pipeline_Replicates.Rmd` - CV analysis and replicate concordance

Open `Replicate_Analysis/Replicate_Analysis.Rproj` in RStudio. Input files are auto-copied from Primary_QC if not present.

### Hemolysis_Check
Analysis of hemolysis markers (HBA1, PGK1, MDH1, SOD1, ENO2) to identify pre-analytical sample handling issues vs biological hemolysis effects.

**Scripts:**
- `Hemolysis_by_Site.Rmd` - Comprehensive hemolysis analysis including:
  - Hemolysis index by site
  - Sex differences (G6PD-related effects)
  - Covariate associations
  - Outlier detection
  - Technical replicate baseline variance comparison

Open `Hemolysis_Check/Hemolysis_Check.Rproj` in RStudio. Data auto-loads from Metadata_Merge output if not in input_files/.

### U19_Analysis
Statistical analyses specific to the U19 grant objectives, including CDX associations and interaction testing.

**Scripts:**
- `Assoc_Analysis_univariate_HighBiomarkers_12_19_25.Rmd` - Univariate association tests
- `Assoc_Analysis_Interaction_HighBiomarkers_12_31_25_1.Rmd` - CDX interaction analyses

Open `U19_Analysis/U19_Analysis.Rproj` in RStudio.

### Clustering
PCA and clustering analyses for dimensionality reduction, metadata associations, and outlier detection.

**Scripts:**
- `biomarker_pca_analysis.Rmd` - Comprehensive PCA with metadata association testing

Open `Clustering/Clustering.Rproj` in RStudio.

### Utilities
Shared R functions used across projects.

**Scripts:**
- `shared_functions.R` - Common utility functions (summarization, outlier detection, CV calculation, plotting helpers)

## Workflow

The recommended analysis order is:

```
1. Primary_QC
   └── Produces: NPQ_post_QC.csv, biomarker/sample summaries
   │
   ├── 2a. Replicate_Analysis (optional, can run after Primary_QC)
   │       └── Produces: CV statistics, concordance reports
   │
   └── 2b. Metadata_Merge
           └── Produces: merged datasets, exclusion report, filtered analysis-ready data

3. Hemolysis_Check (optional, recommended before association analyses)
   └── Produces: hemolysis index, outlier flags, biomarker hemolysis assessment

4. U19_Analysis
   a. Univariate analysis first
      └── Produces: biomarker_groups.csv, meta_plus_race_dx.csv
   b. Interaction analysis second
      └── Produces: association results, interaction plots

5. Clustering (optional, for exploratory analysis)
   └── Produces: PCA scores, loadings, metadata associations, outliers
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
  "irlba",

  # PCA/Clustering
  "FactoMineR",
  "factoextra",
  "scales"
))
```

### Package Summary by Project

| Package | Primary_QC | Replicate | U19 | Clustering | Description |
|---------|:----------:|:---------:|:---:|:----------:|-------------|
| tidyverse | x | x | x | x | Core data manipulation |
| pheatmap | x | | x | | Heatmap visualization |
| caret | x | | x | | ML utilities (for clustering) |
| kableExtra | x | x | x | x | HTML table formatting |
| emmeans | | | x | | Estimated marginal means |
| car | | | x | | Type II ANOVA |
| broom | | | x | | Model tidying |
| patchwork | | x | x | | Plot composition |
| gridExtra | | | x | x | Arrange multiple plots |
| poibin | x | | | | Poisson binomial distribution |
| irlba | x | | | | Fast SVD/PCA |
| FactoMineR | | | | x | PCA analysis |
| factoextra | | | | x | PCA visualization |
| scales | | | | x | Plot formatting |

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
