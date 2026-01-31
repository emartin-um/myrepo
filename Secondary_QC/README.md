# Secondary QC

Secondary quality control analyses for Alamar biomarker data. These analyses are optional but recommended for comprehensive QC assessment.

## Contents

### Replicate_Analysis/
Analysis of technical replicate samples for assessing assay reproducibility and concordance.

**Scripts:**
- `QC_Pipeline_Replicates.Rmd` - CV analysis and replicate concordance

**Key outputs:**
- CV statistics by biomarker
- Concordance reports
- Replicate pair comparisons

### Hemolysis_Check/
Analysis of hemolysis markers (HBA1, PGK1, MDH1, SOD1, ENO2) to identify pre-analytical sample handling issues vs biological hemolysis effects.

**Scripts:**
- `Hemolysis_by_Site.Rmd` - Comprehensive hemolysis analysis

**Key outputs:**
- Hemolysis index by site
- Sex differences (G6PD-related effects)
- Covariate associations
- Outlier detection flags

## Workflow

These analyses can be run after Primary_QC:

```
Primary_QC
├── Replicate_Analysis (can run directly after Primary_QC)
└── Metadata_Merge
    └── Hemolysis_Check (requires merged data)
```

## Getting Started

1. Open the relevant `.Rproj` file in RStudio
2. Input files are auto-copied from upstream outputs if not present
3. Run the Rmd scripts; outputs will be saved to `output_files/`
