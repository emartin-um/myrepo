# Primary QC

Primary quality control analyses for Alamar biomarker data. This pipeline processes raw NPQ data, applies QC filters, and produces cleaned datasets for downstream analysis.

## Usage

1. Open `Primary_QC.Rproj` in RStudio
2. Place input data files in `input_files/`
3. Run analysis scripts
4. Outputs will be saved to `output_files/`

## Scripts

### QC_Pipeline_Primary_ALZ123_12_20_25.Rmd

Main QC pipeline for the combined ALZ1, ALZ2, and ALZ3 datasets.

**Purpose:** Performs comprehensive quality control including:
- Sample and biomarker filtering based on read counts and detectability
- Identification of high-read/high-detectability biomarker subsets
- LOD (Limit of Detection) analysis
- Plate/batch effect visualization
- NPQ data normalization and export

**Key Parameters:**
| Parameter | Default | Description |
|-----------|---------|-------------|
| `read_threshold` | 300 | Minimum read count for sample inclusion |
| `detect_threshold` | 0.8 | Minimum detectability rate for biomarkers |
| `lod_method` | "half" | Method for handling values below LOD |

**Required Input Files:**
- Raw NPQ data files (CSV format) from Alamar platform
- Sample metadata with Run/Bay information

**Output Files:**
- `NPQ_[date]_post_QC.csv` - Cleaned NPQ data for high-quality samples/biomarkers
- `biomarker_summary.csv` - QC statistics for each biomarker
- `sample_summary.csv` - QC statistics for each sample
- Various QC plots (PNG/PDF)

## Input Files

Place your input data in `input_files/` (not tracked in git):
- Raw NPQ data from Alamar runs
- Plate layout files
- Sample manifests

## Output Files

Generated outputs are saved to `output_files/` (not tracked in git):
- Filtered NPQ datasets
- QC summary tables
- Diagnostic plots

## Dependencies

See main repository README for full package list. Key packages:
- `tidyverse`, `readr` - Data manipulation
- `pheatmap` - Heatmap visualization
- `kableExtra` - Table formatting
