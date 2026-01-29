#' Shared Utility Functions for Alamar Biomarker QC
#'
#' This file contains common functions used across multiple analysis scripts.
#' Source this file at the beginning of your Rmd files:
#'   source("../Utilities/shared_functions.R")
#'
#' @author ERM
#' @date 2025-01-28

# =============================================================================
# DATA SUMMARIZATION FUNCTIONS
# =============================================================================

#' Summarize sample counts by Run and Bay
#'
#' @param df Data frame containing Run and Bay columns
#' @param type_name Name for the count column
#' @return Data frame with counts by Run x Bay
summarize_counts <- function(df, type_name = "n") {
  df %>%
    dplyr::group_by(Run, Bay) %>%
    dplyr::summarise(!!type_name := dplyr::n(), .groups = "drop")
}

#' Create pretty summary table for data frame columns
#'
#' Generates summary statistics for numeric columns (min, Q1, median, Q3, max,
#' mean, sd) and frequency counts for character/factor columns.
#'
#' @param df Data frame to summarize
#' @param exclude_cols Character vector of column names to exclude
#' @return Data frame with Column and Summary columns
summarize_df_pretty <- function(df, exclude_cols = NULL) {

  # Exclude columns if specified
  if (!is.null(exclude_cols)) {
    df <- df %>% dplyr::select(-dplyr::all_of(exclude_cols))
  }


  # --- Numeric summary ---
  num_summary <- df %>%
    dplyr::select(where(is.numeric)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Column", values_to = "Value") %>%
    dplyr::group_by(Column) %>%
    dplyr::summarise(
      Summary = paste0(
        "min=", round(min(Value, na.rm = TRUE), 2),
        ", Q1=", round(quantile(Value, 0.25, na.rm = TRUE), 2),
        ", median=", round(median(Value, na.rm = TRUE), 2),
        ", Q3=", round(quantile(Value, 0.75, na.rm = TRUE), 2),
        ", max=", round(max(Value, na.rm = TRUE), 2),
        ", mean=", round(mean(Value, na.rm = TRUE), 2),
        ", sd=", round(sd(Value, na.rm = TRUE), 2)
      ),
      .groups = "drop"
    )

  # --- Character/factor summary ---
  char_summary <- df %>%
    dplyr::select(where(~is.character(.) || is.factor(.))) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Column", values_to = "Value") %>%
    dplyr::group_by(Column, Value) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(Column, dplyr::desc(Count)) %>%
    dplyr::group_by(Column) %>%
    dplyr::summarise(Summary = paste0(Value, " (", Count, ")", collapse = "; "), .groups = "drop")

  # --- Combine numeric and character summaries ---
  dplyr::bind_rows(num_summary, char_summary) %>%
    dplyr::arrange(Column)
}

# =============================================================================
# NAME PROTECTION FUNCTIONS
# =============================================================================

#' Protect variable names for use in formulas
#'
#' Wraps variable names containing special characters in backticks so they
#' can be used safely in R formulas.
#'
#' @param name Character string of variable name
#' @return Character string, wrapped in backticks if needed
protect_name <- function(name) {
  if (grepl("[^a-zA-Z0-9_.]", name)) {
    return(paste0("`", name, "`"))
  }
  return(name)
}

# =============================================================================
# OUTLIER DETECTION FUNCTIONS
# =============================================================================

#' Identify outliers in biomarker data
#'
#' Detects outliers using IQR, MAD, or z-score methods.
#'
#' @param data Data frame containing biomarker values
#' @param biomarker_groups List of biomarker name vectors grouped by cluster
#' @param method Detection method: "iqr", "mad", or "zscore"
#' @param threshold Multiplier for IQR/MAD or number of SDs for zscore
#' @param sample_id_col Name of the sample ID column
#' @return List with outliers (indices), summary (counts), and details (values)
identify_outliers <- function(data, biomarker_groups, method = "iqr", threshold = 3,
                              sample_id_col = "SampleID") {

  # Get all biomarker columns

all_biomarkers <- unlist(biomarker_groups)

  outlier_list <- list()
  outlier_summary <- data.frame()
  outlier_details <- list()

  for (biomarker in all_biomarkers) {
    if (!biomarker %in% names(data)) next

    values <- data[[biomarker]]

    if (method == "iqr") {
      # IQR method (Tukey's fences)
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_fence <- Q1 - threshold * IQR
      upper_fence <- Q3 + threshold * IQR
      outliers <- which(values < lower_fence | values > upper_fence)

    } else if (method == "mad") {
      # Median Absolute Deviation (more robust)
      med <- median(values, na.rm = TRUE)
      mad <- median(abs(values - med), na.rm = TRUE)
      lower_fence <- med - threshold * mad
      upper_fence <- med + threshold * mad
      outliers <- which(values < lower_fence | values > upper_fence)

    } else if (method == "zscore") {
      # Z-score method
      mean_val <- mean(values, na.rm = TRUE)
      sd_val <- sd(values, na.rm = TRUE)
      z_scores <- abs((values - mean_val) / sd_val)
      outliers <- which(z_scores > threshold)
    }

    if (length(outliers) > 0) {
      outlier_list[[biomarker]] <- outliers

      # Get sample IDs and values for outliers with additional info
      outlier_df <- data.frame(
        Biomarker = biomarker,
        Value = values[outliers]
      )

      if (sample_id_col %in% names(data)) {
        outlier_df$SampleID <- data[[sample_id_col]][outliers]
      }

      if ("CDX" %in% names(data)) {
        outlier_df$CDX <- data[["CDX"]][outliers]
      }

      if ("age_at_sample" %in% names(data)) {
        outlier_df$age_at_sample <- data[["age_at_sample"]][outliers]
      }

      if ("Group2" %in% names(data)) {
        outlier_df$Group2 <- data[["Group2"]][outliers]
      }

      if ("Country/State" %in% names(data)) {
        outlier_df$`Country/State` <- data[["Country/State"]][outliers]
      }

      # Sort by Value (highest to lowest)
      outlier_df <- outlier_df %>% dplyr::arrange(dplyr::desc(Value))

      outlier_details[[biomarker]] <- outlier_df

      # Add to summary
      outlier_summary <- dplyr::bind_rows(
        outlier_summary,
        data.frame(
          Biomarker = biomarker,
          N_outliers = length(outliers),
          Pct_outliers = round(100 * length(outliers) / length(values), 2),
          Method = method,
          Threshold = threshold
        )
      )
    }
  }

  return(list(outliers = outlier_list,
              summary = outlier_summary,
              details = outlier_details))
}

# =============================================================================
# CV CALCULATION FUNCTIONS
# =============================================================================

#' Calculate coefficient of variation
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values
#' @return CV as a proportion (not percentage)
cv <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' Calculate CV statistics for replicate samples
#'
#' @param data Data frame in long format with SampleID, Biomarker, Value columns
#' @param sample_col Name of sample ID column
#' @param biomarker_col Name of biomarker column
#' @param value_col Name of value column
#' @return Data frame with CV statistics by biomarker
calculate_cv_stats <- function(data, sample_col = "SampleID",
                               biomarker_col = "Biomarker",
                               value_col = "Value") {

  data %>%
    dplyr::group_by(.data[[biomarker_col]], .data[[sample_col]]) %>%
    dplyr::summarise(
      n_reps = dplyr::n(),
      mean_val = mean(.data[[value_col]], na.rm = TRUE),
      sd_val = sd(.data[[value_col]], na.rm = TRUE),
      cv = sd_val / mean_val,
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data[[biomarker_col]]) %>%
    dplyr::summarise(
      n_samples = dplyr::n(),
      mean_cv = mean(cv, na.rm = TRUE),
      median_cv = median(cv, na.rm = TRUE),
      sd_cv = sd(cv, na.rm = TRUE),
      min_cv = min(cv, na.rm = TRUE),
      max_cv = max(cv, na.rm = TRUE),
      pct_cv_under_20 = mean(cv < 0.20, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

# =============================================================================
# VISUALIZATION HELPER FUNCTIONS
# =============================================================================

#' Create violin plots for biomarker distributions
#'
#' @param data Data frame with biomarker values
#' @param biomarkers Character vector of biomarker column names
#' @param group_var Optional grouping variable for coloring
#' @param ncol Number of columns in facet wrap
#' @return ggplot object
plot_biomarker_violins <- function(data, biomarkers, group_var = NULL, ncol = 3) {

  # Convert to long format
  plot_data <- data %>%
    dplyr::select(dplyr::all_of(c(biomarkers, group_var))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(biomarkers),
      names_to = "Biomarker",
      values_to = "Value"
    )

  if (!is.null(group_var)) {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[group_var]], y = Value,
                                                  fill = .data[[group_var]])) +
      ggplot2::geom_violin(alpha = 0.7) +
      ggplot2::geom_boxplot(width = 0.2, alpha = 0.8)
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = Value)) +
      ggplot2::geom_violin(fill = "steelblue", alpha = 0.7) +
      ggplot2::geom_boxplot(width = 0.2, fill = "white", alpha = 0.8)
  }

  p +
    ggplot2::facet_wrap(~ Biomarker, scales = "free_y", ncol = ncol) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    ggplot2::labs(x = NULL, y = "Value")
}

# =============================================================================
# STATISTICAL TEST HELPER FUNCTIONS
# =============================================================================

#' Check if a variable has sufficient variability for analysis
#'
#' @param data Data frame
#' @param var Variable name to check
#' @return Logical, TRUE if variable has more than 1 unique value
is_variable <- function(data, var) {
  if (!var %in% names(data)) return(FALSE)
  n_unique <- length(unique(na.omit(data[[var]])))
  return(n_unique > 1)
}

#' Check minimum sample size per group
#'
#' @param data Data frame
#' @param group_var Grouping variable name
#' @param min_n Minimum required sample size
#' @return Logical, TRUE if all groups have >= min_n samples
check_min_group_size <- function(data, group_var, min_n = 5) {
  counts <- table(data[[group_var]])
  all(counts >= min_n)
}

# =============================================================================
# FILE I/O HELPER FUNCTIONS
# =============================================================================

#' Ensure output directory exists
#'
#' Creates the directory if it doesn't exist.
#'
#' @param dir_path Path to directory
#' @return Invisible NULL
ensure_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }
  invisible(NULL)
}

#' Read CSV with standard options
#'
#' Wrapper around read_csv with commonly used options.
#'
#' @param file_path Path to CSV file
#' @param ... Additional arguments passed to read_csv
#' @return Data frame
read_data <- function(file_path, ...) {
  readr::read_csv(file_path, show_col_types = FALSE, ...)
}

# =============================================================================
# MESSAGE/LOGGING FUNCTIONS
# =============================================================================

#' Print a section header for Rmd output
#'
#' @param title Section title
#' @param level Header level (1-5)
print_section <- function(title, level = 2) {
  cat(paste0("\n", strrep("#", level), " ", title, "\n\n"))
}

#' Print a key-value pair for reports
#'
#' @param key Parameter name
#' @param value Parameter value
print_param <- function(key, value) {
  cat(paste0("**", key, ":** ", value, "\n\n"))
}
