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

## Dependencies

The shared functions require these packages (all part of tidyverse):
- `dplyr`
- `tidyr`
- `readr`
- `ggplot2`
