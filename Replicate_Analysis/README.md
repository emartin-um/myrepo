# Replicate Analysis

Analysis of replicate samples for assessing assay reproducibility and concordance across plates and runs.

## Usage

1. Open `Replicate_Analysis.Rproj` in RStudio
2. Place input data files in `input_files/` (or let the pipeline auto-copy from Primary_QC)
3. Knit `QC_Pipeline_Replicates.Rmd`
4. Outputs will be saved to `output_files/`

## Scripts

### QC_Pipeline_Replicates.Rmd

Comprehensive replicate analysis for assessing assay reproducibility.

**Purpose:** Analyzes replicate samples to assess:
- Intra-plate and inter-plate coefficient of variation (CV)
- Concordance between technical replicates
- Plate/batch effects on replicate agreement
- Biomarker-specific reproducibility metrics

**Key Parameters:**
| Parameter | Default | Description |
|-----------|---------|-------------|
| `cv_threshold` | 25 | Maximum acceptable CV (%) |

**Required Input Files:**

| File | Description | Source |
|------|-------------|--------|
| `NPQ_*.csv` | Post-QC NPQ data | Primary_QC output (auto-copied if missing) |
| `Raw_counts.csv` | Raw read counts | Primary_QC output (auto-copied if missing) |
| `Sample_QC.csv` | Sample QC metadata | Primary_QC output (auto-copied if missing) |
| `Replicate_list.csv` | HIHG replicate sample IDs | Created by analyst |

If NPQ, Raw_counts, or Sample_QC files are not found in `input_files/`, the pipeline will automatically copy them from `Primary_QC/output_files/`.

**Note:** `Replicate_list.csv` must be created manually by the analyst - it contains the list of HIHG replicate sample IDs.

**Output Files:**
- `hihg_reps_NPQ.csv`, `sc_reps_NPQ.csv`, `nc_reps_NPQ.csv`, `ipc_reps_NPQ.csv` - NPQ values for each replicate type
- `hihg_reps_READS.csv`, `sc_reps_READS.csv`, `nc_reps_READS.csv`, `ipc_reps_READS.csv` - Raw reads for each replicate type
- HTML report with CV analysis and visualizations

## Input Files

Place your input data in `input_files/` (not tracked in git).

If the required files from Primary_QC are not present, they will be automatically copied from `Primary_QC/output_files/` when you run the pipeline.

The only file you must create manually is `Replicate_list.csv` containing the HIHG replicate sample IDs.

## Output Files

Generated outputs are saved to `output_files/` (not tracked in git):
- CV summary statistics
- Concordance reports
- Reproducibility plots

## Dataset Notes

To add dataset-specific notes that appear in the HTML report but aren't tracked in git:

1. Create a file named `notes.md` in this directory
2. Add your notes in markdown format
3. The notes will appear at the end of the HTML report

The `notes.md` file is gitignored, so each dataset can have its own notes.

## Dependencies

See main repository README for full package list. Key packages:
- `tidyverse` - Data manipulation
- `ggplot2` - Visualization
- `kableExtra` - Table formatting

## Workflow

This analysis can run directly after Primary_QC (does not require Metadata_Merge):

```
Primary_QC/output_files/
    ├── NPQ_*.csv
    ├── Raw_counts.csv
    └── Sample_QC.csv
            │
            ▼ (auto-copied if input_files/ is empty)

Replicate_Analysis/input_files/
    ├── NPQ_*.csv
    ├── Raw_counts.csv
    ├── Sample_QC.csv
    └── Replicate_list.csv  (create manually)
            │
            ▼ QC_Pipeline_Replicates.Rmd

Replicate_Analysis/output_files/
    ├── *_reps_NPQ.csv
    ├── *_reps_READS.csv
    └── QC_Pipeline_Replicates.html
```
