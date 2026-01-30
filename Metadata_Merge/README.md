# Metadata Merge

Merges post-QC biomarker data with sample metadata and applies exclusion filters to create analysis-ready datasets.

## Usage

### Recommended: Run the Pipeline in RStudio

Open `Metadata_Merge_Pipeline.qmd` in RStudio and click **Render** (or press Ctrl/Cmd+Shift+K).

This generates an HTML report with:
- Input file verification
- Merge summaries
- Quality control checks (duplicates, implausible values, illogical race/ethnicity)
- Exclusion report
- Final filtered dataset summaries

### Filter-Only Mode

To re-run filtering after editing the exclusion report:

```r
quarto::quarto_render("Metadata_Merge_Pipeline.qmd",
                      execute_params = list(filter_only = TRUE))
```

Or edit the YAML header to set `filter_only: true` and re-render.

## Scripts

### Metadata_Merge_Pipeline.qmd

Main pipeline document that generates an HTML report. Includes:
- Input file verification and auto-detection
- Merge biomarker data with metadata
- Quality control: duplicates, implausible values, illogical combinations
- Exclusion report generation
- Filter application
- Output summaries

### File_Merge.qmd / Apply_Filters.qmd

Legacy individual scripts (kept for compatibility). The pipeline document above is preferred.

## Input Files

Place in `input_files/` (not tracked in git):

| File | Description | Source |
|------|-------------|--------|
| `NPQ_post_QC.csv` | Standard post-QC biomarker data | Primary_QC output |
| `NPQ_Low_post_QC.csv` | Low detectability biomarker data | Primary_QC output |
| `*metadata*.csv` | Sample metadata (auto-detected by name) | External |

If no NPQ files exist in `input_files/`, the pipeline will automatically copy them from `Primary_QC/output_files/`.

## Output Files

Generated in `output_files/` (not tracked in git):

| File | Description |
|------|-------------|
| `merged_standard_post_QC.csv` | Merged standard biomarkers + metadata |
| `merged_low_post_QC.csv` | Merged low biomarkers + metadata |
| `merged_combined_post_QC.csv` | All biomarkers + metadata |
| `sample_exclusion_report.csv` | Exclusion flags and reasons |
| `samples_not_in_metadata.csv` | NPQ samples missing from metadata (if any) |
| `filtered/*.csv` | Final filtered datasets |

## Workflow

```
Primary_QC/output_files/
    ├── NPQ_[date]_post_QC.csv
    └── NPQ_[date]_Low_post_QC.csv
            │
            ▼ (auto-copied if input_files/ is empty)

Metadata_Merge/input_files/
    ├── NPQ_post_QC.csv
    ├── NPQ_Low_post_QC.csv
    └── *metadata*.csv  (auto-detected)
            │
            ▼ Metadata_Merge_Pipeline.qmd

Metadata_Merge/output_files/
    ├── merged_*.csv
    └── sample_exclusion_report.csv  ← Review/edit if needed
            │
            ▼ (filter-only mode)

Metadata_Merge/output_files/filtered/
    ├── filtered_standard_post_QC.csv
    ├── filtered_low_post_QC.csv
    └── filtered_combined_post_QC.csv  ← Analysis-ready
```

## Exclusion Logic

### Auto-Excluded
- **Duplicate samples**: Older runs excluded, most recent kept
- **Implausible values**: Age, BMI, weight, height outside plausible ranges

### Flagged for Review (Not Auto-Excluded)
- **Illogical Race/Ethnicity combinations**: Flagged with note, manually decide to exclude

## Manual Exclusion Editing

To modify which samples are excluded:

1. Run the full pipeline first
2. Open `output_files/sample_exclusion_report.csv`
3. Change `exclude` column:
   - `TRUE` → `FALSE` to keep a sample
   - `FALSE` → `TRUE` to exclude a sample
4. Re-render with `filter_only: true`

## Dependencies

- tidyverse
- knitr
- kableExtra
