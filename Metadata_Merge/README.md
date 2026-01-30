# Metadata Merge

Merges post-QC biomarker data with sample metadata and applies exclusion filters to create analysis-ready datasets.

## Usage

### Automated Pipeline (Recommended)

```bash
cd Metadata_Merge/

# Full pipeline - prompts before copying files
Rscript run_metadata_merge.R

# Auto-confirm copy (non-interactive)
Rscript run_metadata_merge.R --yes

# Use existing input files (skip copy step)
Rscript run_metadata_merge.R --no-copy

# Re-run filtering only (after editing exclusion report)
Rscript run_metadata_merge.R --filter-only
```

### Manual Steps

1. Place files in `input_files/`:
   - `NPQ_post_QC.csv` - Standard post-QC biomarker data
   - `NPQ_Low_post_QC.csv` - Low post-QC biomarker data
   - A metadata file with "metadata" in the name (e.g., `U19_Alamar_metadata.csv`)

2. Run `File_Merge.qmd` in RStudio

3. Review `output_files/sample_exclusion_report.csv`

4. Run `Apply_Filters.qmd` in RStudio

## Scripts

### run_metadata_merge.R

Automated pipeline script with options:
- `--yes` or `-y`: Auto-confirm file copy (non-interactive)
- `--no-copy`: Skip copy step, use existing files in input_files/
- `--filter-only`: Skip steps 1-2, only re-run filtering

### File_Merge.qmd

Merges biomarker data with metadata. Generates:
- Merged datasets (standard, low, combined)
- Sample exclusion report identifying:
  - Duplicate samples (keeps most recent run)
  - Illogical race/ethnicity combinations
  - Implausible values (age, BMI, weight, etc.)
- Metadata quality control summaries

### Apply_Filters.qmd

Applies exclusion filters to create final datasets:
- Removes flagged samples based on exclusion report
- Outputs filtered datasets ready for analysis
- Provides before/after filtering summaries

## Input Files

Place in `input_files/` (not tracked in git):

| File | Description | Source |
|------|-------------|--------|
| `NPQ_post_QC.csv` | Standard post-QC biomarker data | Primary_QC output |
| `NPQ_Low_post_QC.csv` | Low detectability biomarker data | Primary_QC output |
| `*metadata*.csv` | Sample metadata (auto-detected by name) | External |

## Output Files

Generated in `output_files/` (not tracked in git):

| File | Description |
|------|-------------|
| `merged_standard_post_QC.csv` | Merged standard biomarkers + metadata |
| `merged_low_post_QC.csv` | Merged low biomarkers + metadata |
| `merged_combined_post_QC.csv` | All biomarkers + metadata |
| `sample_exclusion_report.csv` | Exclusion flags and reasons |
| `filtered/*.csv` | Final filtered datasets |

## Workflow

```
Primary_QC/output_files/
    ├── NPQ_[date]_post_QC.csv
    └── NPQ_[date]_Low_post_QC.csv
            │
            ▼ (copy to Metadata_Merge/input_files/)

Metadata_Merge/input_files/
    ├── NPQ_post_QC.csv
    ├── NPQ_Low_post_QC.csv
    └── *metadata*.csv  (auto-detected)
            │
            ▼ File_Merge.qmd

Metadata_Merge/output_files/
    ├── merged_*.csv
    └── sample_exclusion_report.csv  ← Review/edit if needed
            │
            ▼ Apply_Filters.qmd

Metadata_Merge/output_files/filtered/
    ├── filtered_standard_post_QC.csv
    ├── filtered_low_post_QC.csv
    └── filtered_combined_post_QC.csv  ← Analysis-ready
```

## Manual Exclusion Editing

To modify which samples are excluded:

1. Run the full pipeline first
2. Open `output_files/sample_exclusion_report.csv`
3. Change `exclude` column from `TRUE` to `FALSE` for samples you want to keep
4. Run: `Rscript run_metadata_merge.R --filter-only`

## Dependencies

- tidyverse
- knitr
- kableExtra
