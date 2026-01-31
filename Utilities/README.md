# Utilities

Shared R functions used across multiple projects in this repository.

> **Note:** The data merge pipeline (File_Merge.qmd, Apply_Filters.qmd) has moved to `Metadata_Merge/`. See `../Metadata_Merge/README.md` for documentation.

## Files

### shared_functions.R

Common utility functions that can be sourced by any Rmd file in the repository.

**Usage:**
```r
source("../Utilities/shared_functions.R")
```

## Function Reference

### Data Summarization

#### `summarize_counts(df, type_name = "n")`
Summarize sample counts by Run and Bay.

```r
sample_counts <- summarize_counts(my_data, "Sample_Count")
```

#### `summarize_df_pretty(df, exclude_cols = NULL)`
Create a pretty summary table for data frame columns. Returns summary statistics for numeric columns (min, Q1, median, Q3, max, mean, sd) and frequency counts for character/factor columns.

```r
summary_table <- summarize_df_pretty(my_data, exclude_cols = c("SampleID"))
knitr::kable(summary_table)
```

### Name Protection

#### `protect_name(name)`
Wrap variable names containing special characters in backticks for safe use in R formulas.

```r
formula_str <- paste(protect_name("pTau-217"), "~ CDX")
# Returns: "`pTau-217` ~ CDX"
```

### Outlier Detection

#### `identify_outliers(data, biomarker_groups, method = "iqr", threshold = 3, sample_id_col = "SampleID")`
Identify outliers in biomarker data using IQR, MAD, or z-score methods.

**Parameters:**
- `method`: "iqr" (Tukey's fences), "mad" (Median Absolute Deviation), or "zscore"
- `threshold`: Multiplier for IQR/MAD, or number of SDs for zscore

**Returns:** List with:
- `outliers`: Named list of row indices by biomarker
- `summary`: Data frame with outlier counts
- `details`: Data frame with outlier values and sample info

```r
results <- identify_outliers(my_data, biomarker_groups,
                             method = "iqr", threshold = 3)
print(results$summary)
```

### CV Calculations

#### `cv(x, na.rm = TRUE)`
Calculate coefficient of variation (SD / mean).

#### `calculate_cv_stats(data, sample_col, biomarker_col, value_col)`
Calculate CV statistics for replicate samples. Expects data in long format.

**Returns:** Data frame with CV statistics by biomarker:
- n_samples, mean_cv, median_cv, sd_cv, min_cv, max_cv, pct_cv_under_20

### Visualization

#### `plot_biomarker_violins(data, biomarkers, group_var = NULL, ncol = 3)`
Create violin plots with overlaid boxplots for biomarker distributions.

```r
plot_biomarker_violins(my_data,
                       biomarkers = c("GFAP", "NEFL", "pTau-217"),
                       group_var = "CDX")
```

### Statistical Helpers

#### `is_variable(data, var)`
Check if a variable has sufficient variability for analysis (more than 1 unique value).

#### `check_min_group_size(data, group_var, min_n = 5)`
Check if all groups have at least `min_n` samples.

### File I/O

#### `ensure_dir(dir_path)`
Create directory if it doesn't exist.

#### `read_data(file_path, ...)`
Wrapper around `readr::read_csv` with `show_col_types = FALSE`.

### Reporting Helpers

#### `print_section(title, level = 2)`
Print a markdown section header for Rmd output.

#### `print_param(key, value)`
Print a key-value pair in bold markdown format.

### Association Testing

#### `test_covariate_associations(data, biomarker_groups, covariates, ...)`
Test biomarker associations with covariates by group. Performs univariate tests (t-test, ANOVA, or linear regression) with FDR correction and visualization.

**Parameters:**
- `data`: Data frame containing biomarkers and covariates
- `biomarker_groups`: Named list of biomarker vectors grouped by cluster
- `covariates`: Character vector of covariate names to test
- `fdr_threshold`: FDR threshold for significance (default 0.05)
- `print_tables`: Whether to print result tables (default FALSE)

**Returns:** List with:
- `omnibus`: Combined results from all tests
- `pairwise`: Pairwise comparisons for significant ANOVA results

```r
results <- test_covariate_associations(
  data = my_data,
  biomarker_groups = list(group1 = c("BM1", "BM2"), group2 = c("BM3")),
  covariates = c("sex", "age_at_sample", "CDX"),
  fdr_threshold = 0.05,
  print_tables = TRUE
)
```

#### `test_covariate_associations_specific(data, biomarkers, covariates, color_by, ...)`
Similar to `test_covariate_associations` but for a specific list of biomarkers rather than grouped biomarkers. Supports coloring plots by additional variables.

```r
results <- test_covariate_associations_specific(
  data = my_data,
  biomarkers = c("GFAP", "NEFL", "pTau-217"),
  covariates = c("Country/State"),
  color_by = "CDX",
  fdr_threshold = 0.001
)
```

#### `plot_group_heatmaps(df, biomarker_groups, cor_mat, ...)`
Create correlation heatmaps and optional boxplots for each group of biomarkers.

**Parameters:**
- `df`: Data frame containing biomarker values
- `biomarker_groups`: Named list of biomarker vectors
- `cor_mat`: Optional pre-computed correlation matrix
- `show_numbers`: Whether to display correlation values
- `add_boxplots`: Whether to add distribution boxplots (default TRUE)

```r
plot_group_heatmaps(my_data, biomarker_groups,
                    show_numbers = TRUE,
                    add_boxplots = TRUE)
```

## Additional Templates

### Flex_Assoc.Rmd

A flexible template for general association testing with covariates. Uses Type III ANOVA with proper handling of unbalanced designs.

**Features:**
- Filter data by CDX, sex, race, etc.
- Test multiple covariates
- FDR correction
- Automatic visualization of significant results
- emmeans post-hoc tests

## Dependencies

The shared functions require these packages:
- `dplyr`, `tidyr`, `readr`, `ggplot2` (tidyverse)
- `purrr`, `broom`, `tibble` (for association testing)
- `knitr`, `kableExtra` (for table output)
- `gridExtra` (for plot arrangement)
- `pheatmap` (for heatmaps)
- `emmeans`, `car` (for Flex_Assoc.Rmd)
