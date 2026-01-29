# Replicate Analysis

Analysis of replicate samples for assessing assay reproducibility and concordance across plates and runs.

## Usage

1. Open `Replicate_Analysis.Rproj` in RStudio
2. Place input data files in `input_files/`
3. Run analysis scripts
4. Outputs will be saved to `output_files/`

## Scripts

### QC_Pipeline_Replicates_ALZ_12_29_25.Rmd

Comprehensive replicate analysis for assessing assay reproducibility.

**Purpose:** Analyzes replicate samples to assess:
- Intra-plate and inter-plate coefficient of variation (CV)
- Concordance between technical replicates
- Plate/batch effects on replicate agreement
- Biomarker-specific reproducibility metrics

**Key Parameters:**
| Parameter | Default | Description |
|-----------|---------|-------------|
| `cv_threshold` | 0.20 | Maximum acceptable CV (20%) |
| `min_replicates` | 2 | Minimum replicates required per sample |

**Required Input Files:**
- Post-QC NPQ data from Primary_QC (e.g., `NPQ_[date]_post_QC.csv`)
- Sample metadata identifying replicate pairs

**Output Files:**
- `cv_summary.csv` - CV statistics by biomarker
- `replicate_concordance.csv` - Pairwise replicate comparisons
- CV distribution plots
- Bland-Altman plots for replicate agreement

**Key Functions:**
- CV calculation and visualization
- Replicate pair identification
- Concordance correlation coefficient (CCC) analysis

## Input Files

Place your input data in `input_files/` (not tracked in git):
- Post-QC NPQ data from `Primary_QC/output_files/`
- Replicate sample mapping files

## Output Files

Generated outputs are saved to `output_files/` (not tracked in git):
- CV summary statistics
- Concordance reports
- Reproducibility plots

## Dependencies

See main repository README for full package list. Key packages:
- `tidyverse` - Data manipulation
- `ggplot2` - Visualization
- `patchwork` - Plot composition
- `kableExtra` - Table formatting

## Workflow

This analysis typically follows Primary_QC:

```
Primary_QC/output_files/NPQ_post_QC.csv
            |
            v
Replicate_Analysis/input_files/
            |
            v
    [Run QC_Pipeline_Replicates.Rmd]
            |
            v
Replicate_Analysis/output_files/
```
