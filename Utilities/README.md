# Utilities

Shared R scripts for data merging and filtering, used across multiple projects.

## Scripts

### File_Merge.qmd
Merges post-QC NPQ biomarker files with metadata. Generates:
- Merged datasets (standard, low, combined)
- Sample exclusion report identifying duplicates and implausible values
- Metadata quality control summaries

### Apply_Filters.qmd
Applies exclusion filters from File_Merge.qmd to the merged datasets:
- Removes flagged samples (duplicates, implausible values)
- Outputs filtered datasets ready for analysis
- Provides before/after filtering summaries

## Usage

These scripts are designed to be run in sequence:
1. Run `File_Merge.qmd` first to merge files and generate exclusion report
2. Run `Apply_Filters.qmd` to apply filters and create final datasets

## Input/Output

Scripts expect:
- `input_files/` - Source data files
- `output_files/` - Merged and filtered outputs
