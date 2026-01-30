#!/usr/bin/env Rscript
# =============================================================================
# Data Merge Pipeline
# =============================================================================
# This script automates the data merge workflow:
# 1. Copies post-QC files from Primary_QC/output_files to Utilities/input_files
# 2. Runs File_Merge.qmd to merge biomarker data with metadata
# 3. Runs Apply_Filters.qmd to apply exclusion filters
#
# Usage:
#   Rscript run_data_merge_pipeline.R
#   OR source from RStudio with working directory set to Utilities/
#
# Prerequisites:
#   - Primary_QC pipeline must have been run first
#   - U19_Alamar_metadata.csv must be in Utilities/input_files/
# =============================================================================

library(tidyverse)
library(knitr)

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

# File naming pattern from Primary_QC (adjust date as needed)
# Set to NULL to auto-detect the most recent files
NPQ_DATE_PATTERN <- NULL  # e.g., "20251220" or NULL for auto-detect

# Paths (relative to Utilities folder)
PRIMARY_QC_OUTPUT <- "../Primary_QC/output_files"
UTILITIES_INPUT <- "input_files"
UTILITIES_OUTPUT <- "output_files"

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

find_latest_npq_files <- function(output_dir) {
  # Find all post_QC files (not triage, not Low)
  standard_files <- list.files(output_dir, pattern = "NPQ_.*_post_QC\\.csv$", full.names = TRUE)
  standard_files <- standard_files[!grepl("Low|triage", standard_files)]

  # Find all Low_post_QC files (not triage)
  low_files <- list.files(output_dir, pattern = "NPQ_.*_Low_post_QC\\.csv$", full.names = TRUE)
  low_files <- low_files[!grepl("triage", low_files)]

  if (length(standard_files) == 0 || length(low_files) == 0) {
    stop("Could not find NPQ post-QC files in ", output_dir)
  }

  # Get most recent by file modification time
  standard_file <- standard_files[which.max(file.mtime(standard_files))]
  low_file <- low_files[which.max(file.mtime(low_files))]

  list(standard = standard_file, low = low_file)
}

run_qmd_as_r <- function(qmd_file) {
  # Extract R code from qmd and run it
  temp_r <- tempfile(fileext = ".R")
  knitr::purl(qmd_file, output = temp_r, quiet = TRUE)
  source(temp_r, local = FALSE)
  unlink(temp_r)
}

# -----------------------------------------------------------------------------
# Main Pipeline
# -----------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 72), "\n")
cat("  DATA MERGE PIPELINE\n")
cat(strrep("=", 72), "\n\n")

# Verify we're in the right directory
if (!file.exists("File_Merge.qmd")) {
  stop("This script must be run from the Utilities/ directory")
}

# Step 1: Copy files from Primary_QC
cat("STEP 1: Copying post-QC files from Primary_QC...\n")
cat(strrep("-", 52), "\n")

if (!is.null(NPQ_DATE_PATTERN)) {
  # Use specified date pattern
  standard_src <- file.path(PRIMARY_QC_OUTPUT, paste0("NPQ_", NPQ_DATE_PATTERN, "_post_QC.csv"))
  low_src <- file.path(PRIMARY_QC_OUTPUT, paste0("NPQ_", NPQ_DATE_PATTERN, "_Low_post_QC.csv"))

  if (!file.exists(standard_src) || !file.exists(low_src)) {
    stop("Specified NPQ files not found for date: ", NPQ_DATE_PATTERN)
  }
} else {
  # Auto-detect latest files
  npq_files <- find_latest_npq_files(PRIMARY_QC_OUTPUT)
  standard_src <- npq_files$standard
  low_src <- npq_files$low
}

cat("  Source (standard):", basename(standard_src), "\n")
cat("  Source (low):     ", basename(low_src), "\n")

# Copy files
file.copy(standard_src, file.path(UTILITIES_INPUT, "NPQ_post_QC.csv"), overwrite = TRUE)
file.copy(low_src, file.path(UTILITIES_INPUT, "NPQ_Low_post_QC.csv"), overwrite = TRUE)

cat("  Copied to:", UTILITIES_INPUT, "\n\n")

# Verify metadata exists
metadata_file <- file.path(UTILITIES_INPUT, "U19_Alamar_metadata.csv")
if (!file.exists(metadata_file)) {
  stop("Metadata file not found: ", metadata_file)
}
cat("  Metadata file found:", basename(metadata_file), "\n\n")

# Step 2: Run File_Merge.qmd
cat("STEP 2: Running File_Merge.qmd...\n")
cat(strrep("-", 52), "\n")

run_qmd_as_r("File_Merge.qmd")

cat("\n  Merge complete. Output files:\n")
merge_outputs <- list.files(UTILITIES_OUTPUT, pattern = "^merged_.*\\.csv$")
for (f in merge_outputs) {
  cat("    -", f, "\n")
}

# Show exclusion summary
exclusion_report <- read_csv(file.path(UTILITIES_OUTPUT, "sample_exclusion_report.csv"),
                             show_col_types = FALSE)
n_exclude <- sum(exclusion_report$exclude)
n_keep <- sum(!exclusion_report$exclude)

cat("\n  Exclusion Report Summary:\n")
cat("    Total samples:", nrow(exclusion_report), "\n")
cat("    To EXCLUDE:   ", n_exclude, "\n")
cat("    To KEEP:      ", n_keep, "\n\n")

if (n_exclude > 0) {
  cat("  Exclusion reasons:\n")
  reasons <- exclusion_report %>%
    filter(exclude) %>%
    count(exclude_reason, sort = TRUE)
  for (i in 1:nrow(reasons)) {
    cat("    ", sprintf("%3d", reasons$n[i]), " - ",
        str_trunc(reasons$exclude_reason[i], 60), "\n", sep = "")
  }
  cat("\n")
}

# Step 3: Run Apply_Filters.qmd
cat("STEP 3: Running Apply_Filters.qmd...\n")
cat(strrep("-", 52), "\n")

run_qmd_as_r("Apply_Filters.qmd")

cat("\n  Filtering complete. Final datasets:\n")
filtered_outputs <- list.files(file.path(UTILITIES_OUTPUT, "filtered"), pattern = "\\.csv$")
for (f in filtered_outputs) {
  fpath <- file.path(UTILITIES_OUTPUT, "filtered", f)
  df <- read_csv(fpath, show_col_types = FALSE)
  cat("    -", f, "(", nrow(df), "samples x", ncol(df), "columns )\n")
}

cat("\n")
cat(strrep("=", 72), "\n")
cat("  PIPELINE COMPLETE\n")
cat(strrep("=", 72), "\n")
cat("\nFiltered datasets ready for analysis in:\n")
cat("  ", file.path(UTILITIES_OUTPUT, "filtered"), "\n\n")
