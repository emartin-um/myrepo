# Clustering

Principal Component Analysis (PCA) and clustering analyses for biomarker data exploration, dimensionality reduction, and metadata association testing.

## Usage

1. Open `Clustering.Rproj` in RStudio
2. Place input data files in `input_files/`
3. Run analysis scripts
4. Outputs will be saved to `output_files/`

## Scripts

### biomarker_pca_analysis.Rmd

Comprehensive PCA analysis with metadata associations and outlier detection.

**Purpose:** Performs dimensionality reduction and identifies:
- Principal components capturing biomarker variation
- Associations between PCs and metadata variables (CDX, Race/Ethnicity, Site, etc.)
- Biomarker loadings driving each PC
- Outlier samples in PC space
- Metadata correlations (confounding assessment)

**Key Configuration Parameters:**
| Parameter | Default | Description |
|-----------|---------|-------------|
| `SUBSET_DATA` | TRUE | Whether to subset data before analysis |
| `SUBSET_VARIABLE` | "CDX" | Variable to subset by |
| `SUBSET_VALUES` | c("NCI") | Values to include in subset |
| `MIN_GROUP_SIZE` | 5 | Minimum samples per group for testing |
| `N_PCS_TO_TEST` | 15 | Number of PCs to test for associations |
| `VARS_TO_TEST` | c("CDX", "Race_Ethnicity", ...) | Metadata variables to test |
| `PLOT_P_THRESHOLD` | 0.05 | Significance threshold for plotting |
| `N_TOP_LOADINGS` | 15 | Top biomarkers to show per PC |
| `N_PCS_LOADINGS` | 10 | Number of PCs for loading analysis |

**Required Input Files:**
- `meta_plus_race_dx.csv` - Merged biomarker + metadata from U19_Analysis

**Output Files:**
- `pca_scores_with_metadata.csv` - PC scores with sample metadata
- `pca_loadings_all.csv` - Biomarker loadings for all PCs
- `pca_metadata_associations.csv` - Association test results
- `pca_variance_partitioning.csv` - Variance explained by each variable
- `metadata_correlations.csv` - Pairwise metadata associations
- `pca_outliers.csv` - Identified outlier samples

**Analysis Sections:**

1. **Data Preparation**
   - Creates AB42/AB40 ratio biomarker
   - Subsetting by metadata variables
   - Missing value handling

2. **PCA Visualization**
   - Scree plots (variance explained)
   - Biplot loadings (PC1-PC4)
   - Sample plots colored by CDX, Race/Ethnicity, Site, Sex, Age, Run, Bay

3. **Metadata Analysis**
   - Correlation/association matrix between metadata variables
   - Cramér's V for categorical pairs
   - Eta-squared for mixed pairs
   - Pearson r for continuous pairs

4. **Association Testing**
   - Kruskal-Wallis tests (categorical variables)
   - Correlation tests (continuous variables)
   - Bonferroni correction for multiple testing
   - Effect size (R²/η²) calculation

5. **Variance Partitioning**
   - How much variance in each PC is explained by each metadata variable
   - Identifies confounders and batch effects

6. **Biomarker Loadings**
   - Top positive and negative loadings for each PC
   - Biological interpretation of PC axes

7. **Outlier Detection**
   - Mahalanobis distance (multivariate)
   - SD threshold (univariate)
   - Outlier distribution by metadata groups

## Input Files

Place your input data in `input_files/` (not tracked in git):
- `meta_plus_race_dx.csv` from `U19_Analysis/output_files/`

## Output Files

Generated outputs are saved to `output_files/` (not tracked in git):
- PCA results (scores, loadings)
- Association test tables
- Outlier reports

## Dependencies

See main repository README for full package list. Key packages:
- `tidyverse` - Data manipulation
- `FactoMineR`, `factoextra` - PCA analysis
- `ggplot2` - Visualization
- `gridExtra` - Multi-panel plots
- `scales` - Plot formatting

## Workflow

This analysis typically follows U19_Analysis:

```
U19_Analysis/output_files/
    |
    +-- meta_plus_race_dx.csv
    |
    v
Clustering/input_files/
    |
    v
[Run biomarker_pca_analysis.Rmd]
    |
    v
Clustering/output_files/
    +-- pca_scores_with_metadata.csv
    +-- pca_loadings_all.csv
    +-- pca_metadata_associations.csv
    +-- pca_outliers.csv
```

## Key Findings

The script includes notes on specific findings:
- HTT and TARDBP separate AFDC (TANZ, ABTH) from AA populations
- PGK1, ANZA5 are low in Tanzania samples
- AB42 is low in Uganda samples (in non-MCI analysis)

## Customization

Edit the Configuration section at the top of the Rmd to:
- Change subset criteria (e.g., analyze only AD samples, or only specific sites)
- Adjust significance thresholds
- Modify which metadata variables to test
- Change the number of PCs to analyze
