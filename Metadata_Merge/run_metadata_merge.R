#!/usr/bin/env Rscript
# =============================================================================
# Metadata Merge Pipeline
# =============================================================================
# This script automates the data merge workflow:
# 1. Optionally copies post-QC files from Primary_QC/output_files
# 2. Runs File_Merge.qmd to merge biomarker data with metadata
# 3. Runs Apply_Filters.qmd to apply exclusion filters
#
# Usage:
#   Full pipeline (will prompt before copying):
#     Rscript run_metadata_merge.R
#
#   Skip copy step (use existing files in input_files/):
#     Rscript run_metadata_merge.R --no-copy
#
#   Re-run filtering only (after manually editing exclusion report):
#     Rscript run_metadata_merge.R --filter-only
#
#   Non-interactive mode (auto-confirm copy):
#     Rscript run_metadata_merge.R --yes
#
# Prerequisites:
#   - Primary_QC pipeline must have been run first (if copying)
#   - Metadata file must be in input_files/
# =============================================================================

library(tidyverse)
library(knitr)

# -----------------------------------------------------------------------------
# Parse command line arguments (or use pre-set variables in RStudio)
# -----------------------------------------------------------------------------
# In RStudio, you can set these before source():
#   FILTER_ONLY <- TRUE
#   source("run_metadata_merge.R")

args <- commandArgs(trailingOnly = TRUE)
if (!exists("FILTER_ONLY")) FILTER_ONLY <- "--filter-only" %in% args
if (!exists("NO_COPY")) NO_COPY <- "--no-copy" %in% args

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

# File naming pattern from Primary_QC (adjust date as needed)
# Set to NULL to auto-detect the most recent files
NPQ_DATE_PATTERN <- NULL  # e.g., "20251220" or NULL for auto-detect

# Paths (relative to Metadata_Merge folder)
PRIMARY_QC_OUTPUT <- "../Primary_QC/output_files"
INPUT_DIR <- "input_files"
OUTPUT_DIR <- "output_files"

# Expected input file names
NPQ_STANDARD_FILE <- "NPQ_post_QC.csv"
NPQ_LOW_FILE <- "NPQ_Low_post_QC.csv"
# METADATA_FILE is auto-detected (any file with "metadata" in the name)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

find_metadata_file <- function(input_dir) {
  # Find any file with "metadata" in the name (case-insensitive)
  all_files <- list.files(input_dir, full.names = TRUE)
  metadata_files <- all_files[grepl("metadata", basename(all_files), ignore.case = TRUE)]

  if (length(metadata_files) == 0) {
    stop("No metadata file found in ", input_dir, "\n",
         "  Expected: a file with 'metadata' in the name (e.g., U19_Alamar_metadata.csv)")
  }

  if (length(metadata_files) > 1) {
    # If multiple, take the most recently modified
    cat("  Note: Found multiple metadata files, using most recent:\n")
    for (f in metadata_files) {
      cat("    ", basename(f), "\n")
    }
    metadata_files <- metadata_files[which.max(file.mtime(metadata_files))]
  }

  return(metadata_files)
}

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

show_exclusion_summary <- function() {
  exclusion_report <- read_csv(file.path(OUTPUT_DIR, "sample_exclusion_report.csv"),
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
}

# -----------------------------------------------------------------------------
# Main Pipeline
# -----------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 72), "\n")
if (FILTER_ONLY) {
  cat("  METADATA MERGE PIPELINE - FILTER ONLY MODE\n")
} else {
  cat("  METADATA MERGE PIPELINE\n")
}
cat(strrep("=", 72), "\n\n")

# Verify we're in the right directory
if (!file.exists("File_Merge.qmd")) {
  stop("This script must be run from the Metadata_Merge/ directory")
}

if (!FILTER_ONLY) {
  # -----------------------------------------------------------------------------
  # Step 1: Copy files from Primary_QC (only if no NPQ files exist)
  # -----------------------------------------------------------------------------

  # Check if NPQ files already exist in input_files/
  existing_npq <- list.files(INPUT_DIR, pattern = "NPQ", ignore.case = TRUE)

  if (!NO_COPY && length(existing_npq) == 0) {
    cat("STEP 1: Copy post-QC files from Primary_QC\n")
    cat(strrep("-", 52), "\n")
    cat("  No NPQ files found in input_files/ - copying from Primary_QC...\n\n")

    # Find source files
    if (!is.null(NPQ_DATE_PATTERN)) {
      standard_src <- file.path(PRIMARY_QC_OUTPUT, paste0("NPQ_", NPQ_DATE_PATTERN, "_post_QC.csv"))
      low_src <- file.path(PRIMARY_QC_OUTPUT, paste0("NPQ_", NPQ_DATE_PATTERN, "_Low_post_QC.csv"))

      if (!file.exists(standard_src) || !file.exists(low_src)) {
        stop("Specified NPQ files not found for date: ", NPQ_DATE_PATTERN)
      }
    } else {
      npq_files <- find_latest_npq_files(PRIMARY_QC_OUTPUT)
      standard_src <- npq_files$standard
      low_src <- npq_files$low
    }

    cat("  Found in Primary_QC/output_files/:\n")
    cat("    Standard: ", basename(standard_src), "\n")
    cat("    Low:      ", basename(low_src), "\n\n")

    standard_dest <- file.path(INPUT_DIR, NPQ_STANDARD_FILE)
    low_dest <- file.path(INPUT_DIR, NPQ_LOW_FILE)

    file.copy(standard_src, standard_dest, overwrite = TRUE)
    file.copy(low_src, low_dest, overwrite = TRUE)
    cat("  Files copied successfully.\n\n")

  } else {
    cat("STEP 1: Using existing NPQ files in input_files/\n")
    cat(strrep("-", 52), "\n")
    if (NO_COPY) {
      cat("  --no-copy flag specified.\n")
    } else {
      cat("  Found existing NPQ files:\n")
      for (f in existing_npq) {
        cat("    -", f, "\n")
      }
    }
    cat("\n")
  }

  # Verify required files exist
  standard_file <- file.path(INPUT_DIR, NPQ_STANDARD_FILE)
  low_file <- file.path(INPUT_DIR, NPQ_LOW_FILE)

  missing_files <- c()
  if (!file.exists(standard_file)) missing_files <- c(missing_files, NPQ_STANDARD_FILE)
  if (!file.exists(low_file)) missing_files <- c(missing_files, NPQ_LOW_FILE)

  if (length(missing_files) > 0) {
    stop("Missing required files in input_files/:\n  - ", paste(missing_files, collapse = "\n  - "))
  }

  # Auto-detect metadata file
  metadata_file <- find_metadata_file(INPUT_DIR)

  cat("  Input files verified:\n")
  cat("    -", NPQ_STANDARD_FILE, "\n")
  cat("    -", NPQ_LOW_FILE, "\n")
  cat("    -", basename(metadata_file), "(auto-detected)\n\n")

  # Set environment variable so File_Merge.qmd can find it
  Sys.setenv(METADATA_FILE = metadata_file)

  # -----------------------------------------------------------------------------
  # Step 2: Run File_Merge.qmd
  # -----------------------------------------------------------------------------
  cat("STEP 2: Running File_Merge.qmd...\n")
  cat(strrep("-", 52), "\n")

  run_qmd_as_r("File_Merge.qmd")

  cat("\n  Merge complete. Output files:\n")
  merge_outputs <- list.files(OUTPUT_DIR, pattern = "^merged_.*\\.csv$")
  for (f in merge_outputs) {
    cat("    -", f, "\n")
  }

  show_exclusion_summary()

} else {
  # -----------------------------------------------------------------------------
  # Filter-only mode: verify exclusion report exists
  # -----------------------------------------------------------------------------
  cat("FILTER-ONLY MODE: Skipping Steps 1-2\n")
  cat(strrep("-", 52), "\n")

  exclusion_file <- file.path(OUTPUT_DIR, "sample_exclusion_report.csv")
  if (!file.exists(exclusion_file)) {
    stop("Exclusion report not found. Run full pipeline first (without --filter-only)")
  }

  merged_file <- file.path(OUTPUT_DIR, "merged_combined_post_QC.csv")
  if (!file.exists(merged_file)) {
    stop("Merged files not found. Run full pipeline first (without --filter-only)")
  }

  cat("  Using existing exclusion report:\n")
  cat("    ", exclusion_file, "\n")
  cat("    Modified:", format(file.mtime(exclusion_file), "%Y-%m-%d %H:%M:%S"), "\n")

  show_exclusion_summary()
}

# -----------------------------------------------------------------------------
# Step 3: Run Apply_Filters.qmd
# -----------------------------------------------------------------------------
if (FILTER_ONLY) {
  cat("STEP 3: Re-running Apply_Filters.qmd with modified exclusions...\n")
} else {
  cat("STEP 3: Running Apply_Filters.qmd...\n")
}
cat(strrep("-", 52), "\n")

# Create filtered output directory if needed
if (!dir.exists(file.path(OUTPUT_DIR, "filtered"))) {
  dir.create(file.path(OUTPUT_DIR, "filtered"))
}

run_qmd_as_r("Apply_Filters.qmd")

cat("\n  Filtering complete. Final datasets:\n")
filtered_outputs <- list.files(file.path(OUTPUT_DIR, "filtered"), pattern = "\\.csv$")
for (f in filtered_outputs) {
  fpath <- file.path(OUTPUT_DIR, "filtered", f)
  df <- read_csv(fpath, show_col_types = FALSE)
  cat("    -", f, "(", nrow(df), "samples x", ncol(df), "columns )\n")
}

cat("\n")
cat(strrep("=", 72), "\n")
cat("  PIPELINE COMPLETE\n")
cat(strrep("=", 72), "\n")
cat("\nFiltered datasets ready for analysis in:\n")
cat("  ", file.path(OUTPUT_DIR, "filtered"), "\n")

if (!FILTER_ONLY) {
  cat("\nTo manually edit exclusions and re-filter:\n")
  cat("  1. Edit output_files/sample_exclusion_report.csv\n")
  cat("     (change 'exclude' column from TRUE to FALSE to keep samples)\n")
  cat("  2. Run: Rscript run_metadata_merge.R --filter-only\n")
}
cat("\n")
