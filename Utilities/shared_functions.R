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

# =============================================================================
# ASSOCIATION TESTING FUNCTIONS
# =============================================================================

#' Test biomarker associations with covariates by group
#'
#' Performs univariate association tests (t-test, ANOVA, or linear regression)
#' between biomarkers and covariates, with FDR correction and visualization.
#'
#' @param data Data frame containing biomarkers and covariates
#' @param biomarker_groups Named list of biomarker vectors grouped by cluster
#' @param covariates Character vector of covariate names to test
#' @param fdr_threshold FDR threshold for significance (default 0.05)
#' @param max_plots_per_page Maximum plots per page (default 6)
#' @param min_group_n Minimum samples per group for categorical tests (default 5)
#' @param print_tables Whether to print result tables (default FALSE)
#' @param biomarkers_to_test Optional subset of biomarkers to test
#' @return List with omnibus results and pairwise comparisons (invisible)
#' @examples
#' results <- test_covariate_associations(
#'   data = my_data,
#'   biomarker_groups = list(group1 = c("BM1", "BM2"), group2 = c("BM3")),
#'   covariates = c("sex", "age_at_sample", "CDX")
#' )
test_covariate_associations <- function(data, biomarker_groups,
                                        covariates = c("sex", "age_at_sample"),
                                        fdr_threshold = 0.05,
                                        max_plots_per_page = 6,
                                        min_group_n = 5,
                                        print_tables = FALSE,
                                        biomarkers_to_test = NULL) {

  # Required packages
  require(dplyr)
  require(purrr)
  require(broom)
  require(knitr)
  require(kableExtra)
  require(ggplot2)
  require(tidyr)
  require(gridExtra)

  # Filter biomarker_groups if specific biomarkers requested
  if (!is.null(biomarkers_to_test)) {
    biomarker_groups <- lapply(biomarker_groups, function(group) {
      intersect(group, biomarkers_to_test)
    })
    # Remove empty groups
    biomarker_groups <- biomarker_groups[sapply(biomarker_groups, length) > 0]

    cat("**Testing", length(unlist(biomarker_groups)), "specified biomarkers**\n\n")
  }

  # Initialize results storage
  all_results <- list()
  pairwise_results <- list()

  # Loop through each group
  for (i in seq_along(biomarker_groups)) {
    group_biomarkers <- biomarker_groups[[i]]

    cat("\n#### Group", i, "\n\n")

    # Get ID column and any other essential metadata
    essential_cols <- c("SampleID", covariates)
    select_cols <- unique(c(essential_cols, group_biomarkers))

    group_data <- data %>%
      dplyr::select(dplyr::all_of(select_cols))
    biomarkers <- group_biomarkers

    # Loop through each covariate
    for (cov in covariates) {
      cat("\n##### Association with", cov, "\n\n")

      # Filter for complete cases on this covariate
      group_data_cov <- group_data %>%
        dplyr::filter(!is.na(.data[[cov]]))

      # Convert to factor if character
      if (is.character(group_data_cov[[cov]])) {
        group_data_cov[[cov]] <- factor(group_data_cov[[cov]])
      }

      # Determine test type based on covariate type
      is_categorical <- is.factor(group_data_cov[[cov]]) ||
        is.character(group_data_cov[[cov]])

      if (is_categorical) {
        # Filter groups by minimum sample size
        group_counts <- table(group_data_cov[[cov]])
        valid_groups <- names(group_counts[group_counts >= min_group_n])

        if (length(valid_groups) < length(group_counts)) {
          excluded <- setdiff(names(group_counts), valid_groups)
          cat("*Excluding groups with n <", min_group_n, ":",
              paste(paste0(excluded, " (n=", group_counts[excluded], ")"), collapse=", "), "*\n\n")
        }

        group_data_cov <- group_data_cov %>% dplyr::filter(.data[[cov]] %in% valid_groups)
        group_data_cov[[cov]] <- droplevels(group_data_cov[[cov]])

        n_levels <- length(unique(group_data_cov[[cov]]))

        if (n_levels < 2) {
          cat("*Skipping", cov, "- fewer than 2 valid groups*\n\n")
          next
        }

        # Print group sample sizes
        cat("**Group sizes:**", paste(names(group_counts[valid_groups]), "=",
                                      group_counts[valid_groups], collapse=", "), "\n\n")

        # Run tests
        results_cov <- purrr::map_df(
          biomarkers,
          ~ {
            tryCatch({
              protected_biomarker <- protect_name(.x)
              protected_cov <- protect_name(cov)
              formula_str <- paste(protected_biomarker, "~", protected_cov)

              if (n_levels == 2) {
                # T-test for binary variables
                tb <- t.test(as.formula(formula_str), data = group_data_cov)

                # Get group means
                group_means <- group_data_cov %>%
                  dplyr::group_by(!!sym(cov)) %>%
                  dplyr::summarise(mean = mean(!!sym(.x), na.rm = TRUE), .groups = "drop")

                tibble::tibble(
                  Biomarker = .x,
                  statistic = tb$statistic,
                  p.value = tb$p.value,
                  method = "t-test",
                  mean_diff = diff(group_means$mean),
                  group_info = paste(group_means[[cov]], "=",
                                     round(group_means$mean, 3), collapse = "; ")
                )
              } else {
                # ANOVA for 3+ levels
                lm_fit <- lm(as.formula(formula_str), data = group_data_cov)
                aov_result <- anova(lm_fit)

                # Get group means
                group_means <- group_data_cov %>%
                  dplyr::group_by(!!sym(cov)) %>%
                  dplyr::summarise(mean = mean(!!sym(.x), na.rm = TRUE),
                                   sd = sd(!!sym(.x), na.rm = TRUE),
                                   .groups = "drop")

                tibble::tibble(
                  Biomarker = .x,
                  F_statistic = aov_result[1, "F value"],
                  p.value = aov_result[1, "Pr(>F)"],
                  method = "ANOVA",
                  group_info = paste(group_means[[cov]], "=",
                                     round(group_means$mean, 3), collapse = "; ")
                )
              }
            }, error = function(e) {
              tibble::tibble(Biomarker = .x, statistic = NA, p.value = NA,
                             method = "error", group_info = NA)
            })
          }
        )

        # Pairwise comparisons for significant ANOVA results
        if (n_levels > 2) {
          sig_biomarkers_anova <- results_cov %>%
            dplyr::mutate(FDR_temp = p.adjust(p.value, method = "fdr")) %>%
            dplyr::filter(FDR_temp < fdr_threshold) %>%
            dplyr::pull(Biomarker)

          if (length(sig_biomarkers_anova) > 0) {
            pairwise_list <- purrr::map_df(sig_biomarkers_anova, ~ {
              tryCatch({
                pw <- pairwise.t.test(group_data_cov[[.x]], group_data_cov[[cov]],
                                      p.adjust.method = "bonferroni")

                # Convert to tidy format
                pw_tidy <- as.data.frame(pw$p.value) %>%
                  tibble::rownames_to_column("group1") %>%
                  tidyr::pivot_longer(-group1, names_to = "group2", values_to = "p.value") %>%
                  dplyr::filter(!is.na(p.value)) %>%
                  dplyr::mutate(
                    Biomarker = .x,
                    Group = i,
                    Covariate = cov
                  )

                pw_tidy
              }, error = function(e) {
                tibble::tibble(
                  Biomarker = .x,
                  group1 = NA_character_,
                  group2 = NA_character_,
                  p.value = NA_real_,
                  Group = i,
                  Covariate = cov
                )
              })
            })

            pairwise_results[[paste0("Group", i, "_", cov)]] <- pairwise_list
          }
        }

      } else {
        # Continuous covariate - use linear regression
        results_cov <- purrr::map_df(
          biomarkers,
          ~ {
            tryCatch({
              protected_biomarker <- protect_name(.x)
              protected_cov <- protect_name(cov)
              formula_str <- paste(protected_biomarker, "~", protected_cov)

              lm_fit <- lm(as.formula(formula_str), data = group_data_cov)
              broom::tidy(lm_fit) %>%
                dplyr::filter(term == cov | term == protected_cov) %>%
                dplyr::mutate(Biomarker = .x, method = "Linear Regression") %>%
                dplyr::select(Biomarker, estimate, statistic, p.value, method)
            }, error = function(e) {
              tibble::tibble(Biomarker = .x, estimate = NA, statistic = NA,
                             p.value = NA, method = "error")
            })
          }
        )
      }

      # Add FDR correction
      results_cov <- results_cov %>%
        dplyr::arrange(p.value) %>%
        dplyr::mutate(FDR = p.adjust(p.value, method = "fdr"))

      # Store results
      all_results[[paste0("Group", i, "_", cov)]] <- results_cov %>%
        dplyr::mutate(Group = i, Covariate = cov)

      if (print_tables == TRUE) {
        # Display table
        if (is_categorical && n_levels == 2) {
          header_text <- paste("T-test for association with", cov)
          display_cols <- c("Biomarker", "statistic", "p.value", "FDR", "mean_diff", "group_info")
        } else if (is_categorical) {
          header_text <- paste("ANOVA for association with", cov)
          display_cols <- c("Biomarker", "F_statistic", "p.value", "FDR", "group_info")
        } else {
          header_text <- paste("Linear Regression for", cov, "association")
          display_cols <- c("Biomarker", "estimate", "statistic", "p.value", "FDR")
        }

        results_display <- results_cov %>% dplyr::select(dplyr::any_of(display_cols))

        print(
          results_display %>%
            kableExtra::kbl(format = "html", digits = 4) %>%
            kableExtra::add_header_above(setNames(ncol(results_display), header_text)) %>%
            kableExtra::kable_styling(
              bootstrap_options = c("striped", "hover", "condensed"),
              full_width = FALSE
            )
        )

        cat("\n\n")

        # Print pairwise comparisons for significant ANOVA results
        if (is_categorical && n_levels > 2 && paste0("Group", i, "_", cov) %in% names(pairwise_results)) {
          cat("\n**Pairwise comparisons (Bonferroni-adjusted) for significant biomarkers:**\n\n")

          pw_display <- pairwise_results[[paste0("Group", i, "_", cov)]] %>%
            dplyr::select(Biomarker, group1, group2, p.value) %>%
            dplyr::arrange(Biomarker, p.value)

          print(
            pw_display %>%
              kableExtra::kbl(format = "html", digits = 4) %>%
              kableExtra::kable_styling(
                bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE
              )
          )

          cat("\n\n")
        }
      }

      # Plots for significant associations
      sig_biomarkers <- results_cov %>%
        dplyr::filter(FDR < fdr_threshold) %>%
        dplyr::pull(Biomarker)

      if (length(sig_biomarkers) > 0) {
        cat("\n**Significant", cov, "Associations (FDR <", fdr_threshold, "):** ",
            length(sig_biomarkers), "biomarkers\n\n")

        # Split into pages
        n_pages <- ceiling(length(sig_biomarkers) / max_plots_per_page)

        for (page in 1:n_pages) {
          start_idx <- (page - 1) * max_plots_per_page + 1
          end_idx <- min(page * max_plots_per_page, length(sig_biomarkers))
          page_biomarkers <- sig_biomarkers[start_idx:end_idx]

          if (n_pages > 1) {
            cat("\n*Page", page, "of", n_pages, "*\n\n")
          }

          if (is_categorical) {
            # Create statistics labels
            stat_labels <- results_cov %>%
              dplyr::filter(Biomarker %in% page_biomarkers)

            # Check which columns exist
            if ("statistic" %in% names(stat_labels)) {
              stat_labels <- stat_labels %>%
                dplyr::mutate(label = sprintf("t=%.2f, p=%.2e", statistic, p.value))
            } else if ("F_statistic" %in% names(stat_labels)) {
              stat_labels <- stat_labels %>%
                dplyr::mutate(label = sprintf("F=%.2f, p=%.2e", F_statistic, p.value))
            }

            stat_labels <- stat_labels %>% dplyr::select(Biomarker, label)

            # Boxplots for categorical covariates
            plot_data <- group_data_cov %>%
              dplyr::select(dplyr::all_of(c(cov, page_biomarkers))) %>%
              tidyr::pivot_longer(cols = dplyr::all_of(page_biomarkers),
                                  names_to = "Biomarker",
                                  values_to = "Value")

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[cov]], y = Value, fill = .data[[cov]])) +
              ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1) +
              ggplot2::geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
              ggplot2::geom_text(data = stat_labels,
                                 ggplot2::aes(x = Inf, y = Inf, label = label),
                                 hjust = 1.05, vjust = 1.5,
                                 size = 2.5, inherit.aes = FALSE) +
              ggplot2::facet_wrap(~ Biomarker, scales = "free_y", ncol = 3) +
              ggplot2::theme_minimal() +
              ggplot2::theme(
                strip.text = ggplot2::element_text(face = "bold", size = 9),
                axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
                legend.position = "bottom"
              ) +
              ggplot2::labs(
                title = paste("Group", i, "-", cov, "Associations (FDR <", fdr_threshold, ")"),
                x = cov,
                y = "Biomarker Value"
              )

            # Add custom colors for common covariates
            if (cov == "sex" && all(levels(group_data_cov[[cov]]) %in% c("F", "M"))) {
              p <- p + ggplot2::scale_fill_manual(values = c("F" = "#E69F00", "M" = "#56B4E9"))
            }

            print(p)
            cat("\n\n")

          } else {
            # Scatter plots for continuous covariates
            plots <- lapply(page_biomarkers, function(biom) {
              plot_data <- group_data_cov %>%
                dplyr::select(dplyr::all_of(c(cov, biom)))

              names(plot_data) <- c("x_var", "y_var")

              # Get statistics
              stat_row <- results_cov %>% dplyr::filter(Biomarker == biom)

              ggplot2::ggplot(plot_data, ggplot2::aes(x = x_var, y = y_var)) +
                ggplot2::geom_point(alpha = 0.5, color = "steelblue", size = 1.5) +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "red",
                                     fill = "pink", alpha = 0.3) +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                  title = biom,
                  subtitle = sprintf("β=%.3f, t=%.2f, p=%.2e, FDR=%.2e",
                                     stat_row$estimate, stat_row$statistic,
                                     stat_row$p.value, stat_row$FDR),
                  x = cov,
                  y = "Value"
                ) +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(face = "bold", size = 9),
                  plot.subtitle = ggplot2::element_text(size = 7),
                  axis.title = ggplot2::element_text(size = 8),
                  axis.text = ggplot2::element_text(size = 7)
                )
            })

            n_cols <- min(3, length(plots))
            do.call(gridExtra::grid.arrange, c(plots, ncol = n_cols,
                                               top = paste("Group", i, "-", cov,
                                                           "Associations (FDR <", fdr_threshold,
                                                           ") - Page", page)))
            cat("\n\n")
          }
        }
      } else {
        cat("\n*No significant", cov, "associations at FDR <", fdr_threshold, "*\n\n")
      }
    }

    cat("\n---\n\n")
  }

  # Combine all results
  if (length(all_results) > 0) {
    combined_results <- dplyr::bind_rows(all_results)
    pairwise_combined <- if(length(pairwise_results) > 0) {
      dplyr::bind_rows(pairwise_results)
    } else {
      NULL
    }

    return(invisible(list(
      omnibus = combined_results,
      pairwise = pairwise_combined
    )))
  } else {
    return(invisible(NULL))
  }
}


#' Test biomarker associations with covariates for specific biomarkers
#'
#' Similar to test_covariate_associations but for a specific list of biomarkers
#' rather than grouped biomarkers. Supports coloring by additional variables.
#'
#' @param data Data frame containing biomarkers and covariates
#' @param biomarkers Character vector of biomarker names to test
#' @param covariates Character vector of covariate names to test
#' @param color_by Optional variable name for coloring plots
#' @param fdr_threshold FDR threshold for significance (default 0.05)
#' @param max_plots_per_page Maximum plots per page (default 6)
#' @param min_group_n Minimum samples per group for categorical tests (default 5)
#' @return List with omnibus results and pairwise comparisons (invisible)
test_covariate_associations_specific <- function(data,
                                                  biomarkers = NULL,
                                                  covariates = c("sex", "age_at_sample"),
                                                  color_by = NULL,
                                                  fdr_threshold = 0.05,
                                                  max_plots_per_page = 6,
                                                  min_group_n = 5) {

  # Required packages
  require(dplyr)
  require(purrr)
  require(broom)
  require(knitr)
  require(kableExtra)
  require(ggplot2)
  require(tidyr)
  require(gridExtra)

  # Check if biomarkers exist in data
  missing_biomarkers <- setdiff(biomarkers, names(data))
  if (length(missing_biomarkers) > 0) {
    warning("The following biomarkers not found in data: ",
            paste(missing_biomarkers, collapse = ", "))
    biomarkers <- intersect(biomarkers, names(data))
  }

  if (length(biomarkers) == 0) {
    stop("No valid biomarkers specified")
  }

  # Select necessary columns
  essential_cols <- unique(c("SampleID", covariates, color_by, biomarkers))
  data_subset <- data %>%
    dplyr::select(dplyr::all_of(essential_cols))

  # Initialize results storage
  all_results <- list()
  pairwise_results <- list()

  # Loop through each covariate
  for (cov in covariates) {
    cat("\n### Association with", cov, "\n\n")

    # Check if covariate exists
    if (!cov %in% names(data_subset)) {
      cat("*Skipping", cov, "- column not found in data*\n\n")
      next
    }

    # Filter for complete cases on this covariate
    data_cov <- data_subset %>%
      dplyr::filter(!is.na(!!sym(cov)))

    # Check if we have any data left
    if (nrow(data_cov) == 0) {
      cat("*Skipping", cov, "- no complete cases*\n\n")
      next
    }

    # Convert to factor if character
    if (is.character(data_cov[[cov]])) {
      data_cov[[cov]] <- factor(data_cov[[cov]])
    }

    # Determine test type based on covariate type
    is_categorical <- is.factor(data_cov[[cov]]) ||
      is.character(data_cov[[cov]])

    if (is_categorical) {
      # Filter groups by minimum sample size
      group_counts <- table(data_cov[[cov]])
      valid_groups <- names(group_counts[group_counts >= min_group_n])

      if (length(valid_groups) < length(group_counts)) {
        excluded <- setdiff(names(group_counts), valid_groups)
        cat("*Excluding groups with n <", min_group_n, ":",
            paste(paste0(excluded, " (n=", group_counts[excluded], ")"), collapse=", "), "*\n\n")
      }

      data_cov <- data_cov %>% dplyr::filter(.data[[cov]] %in% valid_groups)
      data_cov[[cov]] <- droplevels(data_cov[[cov]])

      n_levels <- length(unique(data_cov[[cov]]))

      if (n_levels < 2) {
        cat("*Skipping", cov, "- fewer than 2 valid groups*\n\n")
        next
      }

      # Print group sample sizes
      cat("**Group sizes:**", paste(names(group_counts[valid_groups]), "=",
                                    group_counts[valid_groups], collapse=", "), "\n\n")

      # Run tests
      results_cov <- purrr::map_df(
        biomarkers,
        ~ {
          tryCatch({
            protected_biomarker <- protect_name(.x)
            protected_cov <- protect_name(cov)
            formula_str <- paste(protected_biomarker, "~", protected_cov)

            if (n_levels == 2) {
              # T-test for binary variables
              tb <- t.test(as.formula(formula_str), data = data_cov)

              # Get group means
              group_means <- data_cov %>%
                dplyr::group_by(!!sym(cov)) %>%
                dplyr::summarise(mean = mean(!!sym(.x), na.rm = TRUE), .groups = "drop")

              tibble::tibble(
                Biomarker = .x,
                statistic = tb$statistic,
                p.value = tb$p.value,
                method = "t-test",
                mean_diff = diff(group_means$mean),
                group_info = paste(group_means[[cov]], "=",
                                   round(group_means$mean, 3), collapse = "; ")
              )
            } else {
              # ANOVA for 3+ levels
              lm_fit <- lm(as.formula(formula_str), data = data_cov)
              aov_result <- anova(lm_fit)

              # Get group means
              group_means <- data_cov %>%
                dplyr::group_by(!!sym(cov)) %>%
                dplyr::summarise(mean = mean(!!sym(.x), na.rm = TRUE),
                                 sd = sd(!!sym(.x), na.rm = TRUE),
                                 .groups = "drop")

              tibble::tibble(
                Biomarker = .x,
                F_statistic = aov_result[1, "F value"],
                p.value = aov_result[1, "Pr(>F)"],
                method = "ANOVA",
                group_info = paste(group_means[[cov]], "=",
                                   round(group_means$mean, 3), collapse = "; ")
              )
            }
          }, error = function(e) {
            tibble::tibble(Biomarker = .x, statistic = NA, p.value = NA,
                           method = "error", group_info = NA)
          })
        }
      )

      # Pairwise comparisons for significant ANOVA results
      if (n_levels > 2) {
        sig_biomarkers_anova <- results_cov %>%
          dplyr::mutate(FDR_temp = p.adjust(p.value, method = "fdr")) %>%
          dplyr::filter(FDR_temp < fdr_threshold) %>%
          dplyr::pull(Biomarker)

        if (length(sig_biomarkers_anova) > 0) {
          pairwise_list <- purrr::map(sig_biomarkers_anova, ~ {
            protected_biomarker <- protect_name(.x)
            protected_cov <- protect_name(cov)
            formula_str <- paste(protected_biomarker, "~", protected_cov)

            pw <- pairwise.t.test(data_cov[[.x]], data_cov[[cov]],
                                  p.adjust.method = "bonferroni")

            # Convert to tidy format
            pw_tidy <- as.data.frame(pw$p.value) %>%
              tibble::rownames_to_column("group1") %>%
              tidyr::pivot_longer(-group1, names_to = "group2", values_to = "p.value") %>%
              dplyr::filter(!is.na(p.value)) %>%
              dplyr::mutate(Biomarker = .x)

            pw_tidy
          })

          pairwise_results[[cov]] <- dplyr::bind_rows(pairwise_list)
        }
      }

    } else {
      # Continuous covariate - use linear regression
      results_cov <- purrr::map_df(
        biomarkers,
        ~ {
          tryCatch({
            protected_biomarker <- protect_name(.x)
            protected_cov <- protect_name(cov)
            formula_str <- paste(protected_biomarker, "~", protected_cov)

            lm_fit <- lm(as.formula(formula_str), data = data_cov)
            broom::tidy(lm_fit) %>%
              dplyr::filter(term == cov | term == protected_cov) %>%
              dplyr::mutate(Biomarker = .x, method = "Linear Regression") %>%
              dplyr::select(Biomarker, estimate, statistic, p.value, method)
          }, error = function(e) {
            tibble::tibble(Biomarker = .x, estimate = NA, statistic = NA,
                           p.value = NA, method = "error")
          })
        }
      )
    }

    # Add FDR correction
    results_cov <- results_cov %>%
      dplyr::arrange(p.value) %>%
      dplyr::mutate(FDR = p.adjust(p.value, method = "fdr"))

    # Store results
    all_results[[cov]] <- results_cov %>%
      dplyr::mutate(Covariate = cov)

    # Display table
    if (is_categorical && n_levels == 2) {
      header_text <- paste("T-test for association with", cov)
      display_cols <- c("Biomarker", "statistic", "p.value", "FDR", "mean_diff")
    } else if (is_categorical) {
      header_text <- paste("ANOVA for association with", cov)
      display_cols <- c("Biomarker", "F_statistic", "p.value", "FDR")
    } else {
      header_text <- paste("Linear Regression for", cov, "association")
      display_cols <- c("Biomarker", "estimate", "statistic", "p.value", "FDR")
    }

    results_display <- results_cov %>% dplyr::select(dplyr::any_of(display_cols))

    print(
      results_display %>%
        kableExtra::kbl(format = "html", digits = 4) %>%
        kableExtra::add_header_above(setNames(ncol(results_display), header_text)) %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed"),
          full_width = FALSE
        )
    )

    cat("\n\n")

    # Print pairwise comparisons for significant ANOVA results
    if (is_categorical && n_levels > 2 && cov %in% names(pairwise_results)) {
      cat("\n**Pairwise comparisons (Bonferroni-adjusted) for significant biomarkers:**\n\n")

      pw_display <- pairwise_results[[cov]] %>%
        dplyr::arrange(Biomarker, p.value)

      print(
        pw_display %>%
          kableExtra::kbl(format = "html", digits = 4) %>%
          kableExtra::kable_styling(
            bootstrap_options = c("striped", "hover", "condensed"),
            full_width = FALSE
          )
      )

      cat("\n\n")
    }

    # Plots for significant associations
    sig_biomarkers <- results_cov %>%
      dplyr::filter(FDR < fdr_threshold) %>%
      dplyr::pull(Biomarker)

    if (length(sig_biomarkers) > 0) {
      cat("\n**Significant", cov, "Associations (FDR <", fdr_threshold, "):** ",
          length(sig_biomarkers), "biomarkers\n\n")

      # Split into pages
      n_pages <- ceiling(length(sig_biomarkers) / max_plots_per_page)

      for (page in 1:n_pages) {
        start_idx <- (page - 1) * max_plots_per_page + 1
        end_idx <- min(page * max_plots_per_page, length(sig_biomarkers))
        page_biomarkers <- sig_biomarkers[start_idx:end_idx]

        if (n_pages > 1) {
          cat("\n*Page", page, "of", n_pages, "*\n\n")
        }

        if (is_categorical) {
          # Boxplots for categorical covariates
          select_cols <- c(cov, page_biomarkers)
          if (!is.null(color_by) && color_by %in% names(data_cov)) {
            select_cols <- c(select_cols, color_by)
          }

          plot_data <- data_cov %>%
            dplyr::select(dplyr::all_of(select_cols)) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(page_biomarkers),
                                names_to = "Biomarker",
                                values_to = "Value")

          # Create base plot
          if (!is.null(color_by) && color_by %in% names(plot_data)) {
            if (is.character(plot_data[[color_by]])) {
              plot_data[[color_by]] <- factor(plot_data[[color_by]])
            }

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = .data[[cov]], x = Value, fill = .data[[color_by]])) +
              ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1,
                                    position = ggplot2::position_dodge(0.8)) +
              ggplot2::geom_point(ggplot2::aes(color = .data[[color_by]]),
                                  position = ggplot2::position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
                                  alpha = 0.4, size = 1) +
              ggplot2::labs(fill = color_by, color = color_by)
          } else {
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[cov]], y = Value, fill = .data[[cov]])) +
              ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1) +
              ggplot2::geom_jitter(width = 0.2, alpha = 0.3, size = 1)
          }

          p <- p +
            ggplot2::facet_wrap(~ Biomarker, scales = "free_y", ncol = 3) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              strip.text = ggplot2::element_text(face = "bold", size = 9),
              axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
              legend.position = "bottom"
            ) +
            ggplot2::labs(
              title = paste(cov, "Associations (FDR <", fdr_threshold, ")"),
              x = cov,
              y = "Biomarker Value"
            )

          if (is.null(color_by) || color_by == cov) {
            if (cov == "sex" && all(levels(data_cov[[cov]]) %in% c("F", "M"))) {
              p <- p + ggplot2::scale_fill_manual(values = c("F" = "#E69F00", "M" = "#56B4E9"))
            } else if (cov == "CDX_grouped") {
              p <- p + ggplot2::scale_fill_manual(
                values = c("NCI" = "#2ECC71", "MCI" = "#F39C12", "AD" = "#E74C3C")
              )
            }
          }

          print(p)
          cat("\n\n")

        } else {
          # Histograms for continuous covariates
          plots <- lapply(page_biomarkers, function(biom) {
            select_cols <- c(cov, biom)
            if (!is.null(color_by) && color_by %in% names(data_cov)) {
              select_cols <- c(select_cols, color_by)
            }

            plot_data <- data_cov %>%
              dplyr::select(dplyr::all_of(select_cols))

            names(plot_data)[names(plot_data) == cov] <- "x_var"
            names(plot_data)[names(plot_data) == biom] <- "y_var"

            fdr_val <- results_cov %>%
              dplyr::filter(Biomarker == biom) %>%
              dplyr::pull(FDR)

            if (!is.null(color_by) && color_by %in% names(plot_data)) {
              if (is.character(plot_data[[color_by]])) {
                plot_data[[color_by]] <- factor(plot_data[[color_by]])
              }

              p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = y_var, fill = .data[[color_by]])) +
                ggplot2::geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
                ggplot2::labs(fill = color_by)

              if (color_by == "sex") {
                p <- p + ggplot2::scale_fill_manual(values = c("F" = "#E69F00", "M" = "#56B4E9"))
              } else if (color_by == "CDX_grouped") {
                p <- p + ggplot2::scale_fill_manual(
                  values = c("NCI" = "#2ECC71", "MCI" = "#F39C12", "AD" = "#E74C3C")
                )
              }
            } else {
              p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = y_var)) +
                ggplot2::geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30)
            }

            p <- p +
              ggplot2::theme_minimal() +
              ggplot2::labs(
                title = biom,
                subtitle = paste0("Association with ", cov,
                                  ", FDR = ", format(fdr_val, digits = 3, scientific = TRUE)),
                x = "Biomarker Value",
                y = "Count"
              ) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 9),
                plot.subtitle = ggplot2::element_text(size = 7),
                axis.title = ggplot2::element_text(size = 8),
                axis.text = ggplot2::element_text(size = 7),
                legend.position = "bottom",
                legend.title = ggplot2::element_text(size = 7),
                legend.text = ggplot2::element_text(size = 6)
              )

            return(p)
          })

          n_cols <- min(3, length(plots))
          do.call(gridExtra::grid.arrange, c(plots, ncol = n_cols,
                                             top = paste(cov, "Associations (FDR <",
                                                         fdr_threshold, ") - Page", page)))
          cat("\n\n")
        }
      }
    } else {
      cat("\n*No significant", cov, "associations at FDR <", fdr_threshold, "*\n\n")
    }

    cat("\n---\n\n")
  }

  # Combine all results
  if (length(all_results) > 0) {
    combined_results <- dplyr::bind_rows(all_results)
    return(invisible(list(
      omnibus = combined_results,
      pairwise = if(length(pairwise_results) > 0) dplyr::bind_rows(pairwise_results, .id = "Covariate") else NULL
    )))
  } else {
    return(invisible(NULL))
  }
}


#' Plot correlation heatmaps and distributions for biomarker groups
#'
#' Creates correlation heatmaps and optional boxplots for each group of biomarkers.
#'
#' @param df Data frame containing biomarker values
#' @param biomarker_groups Named list of biomarker vectors grouped by cluster
#' @param cor_mat Optional pre-computed correlation matrix
#' @param show_numbers Whether to display correlation values on heatmap
#' @param min_group_size Minimum group size for visualization (default 2)
#' @param fontsize Base font size for heatmap labels (default 8)
#' @param add_boxplots Whether to add distribution boxplots (default TRUE)
#' @return NULL (invisible), produces plots as side effects
plot_group_heatmaps <- function(df, biomarker_groups, cor_mat = NULL,
                                show_numbers = FALSE,
                                min_group_size = 2,
                                fontsize = 8,
                                add_boxplots = TRUE) {

  require(pheatmap)
  require(ggplot2)
  require(tidyr)
  require(dplyr)

  # If correlation matrix not provided, compute it
  if (is.null(cor_mat)) {
    num_cols <- sapply(df, is.numeric)
    samp_num <- df[, num_cols]
    cor_mat <- cor(samp_num, use = "pairwise.complete.obs")
  }

  # Get numeric data for boxplots
  num_cols <- sapply(df, is.numeric)
  samp_num <- df[, num_cols]

  # Summary statistics
  cat("**Total groups:** ", length(biomarker_groups), "\n\n")
  cat("**Total biomarkers:** ", sum(sapply(biomarker_groups, length)), "\n\n")

  # Group size distribution
  group_sizes <- sapply(biomarker_groups, length)
  cat("**Group sizes:** Min =", min(group_sizes),
      ", Max =", max(group_sizes),
      ", Mean =", round(mean(group_sizes), 1), "\n\n")
  cat("---\n\n")

  # Loop through each group
  for (i in seq_along(biomarker_groups)) {
    group_biomarkers <- biomarker_groups[[i]]

    # Print header
    cat("\n### Group", i, "\n")
    cat("**Size:** ", length(group_biomarkers), " biomarker(s)\n\n")

    # Skip if group is too small
    if (length(group_biomarkers) < min_group_size) {
      cat("*Group has fewer than", min_group_size, "biomarkers - skipping visualizations.*\n\n")
      cat("**Biomarkers:**", paste(group_biomarkers, collapse = ", "), "\n\n")
      next
    }

    # Show biomarker list
    cat("**Biomarkers:**\n\n")
    cat(paste("-", group_biomarkers, collapse = "\n\n"), "\n\n")

    # Subset correlation matrix for this group
    group_cor <- cor_mat[group_biomarkers, group_biomarkers]

    # Summary statistics for this group
    upper_tri <- group_cor[upper.tri(group_cor)]
    cat("**Correlation summary:**\n\n")
    cat("- Mean correlation:", round(mean(upper_tri), 3), "\n\n")
    cat("- Median correlation:", round(median(upper_tri), 3), "\n\n")
    cat("- Range:", round(min(upper_tri), 3), "to", round(max(upper_tri), 3), "\n\n")

    # Create heatmap
    cat("#### Correlation Heatmap\n")
    pheat_obj <- NULL
    tryCatch({
      # Adjust font size based on group size
      auto_fontsize <- max(4, min(fontsize, 12 - length(group_biomarkers) / 5))

      pheat_obj <- pheatmap::pheatmap(group_cor,
                                      main = paste("Group", i, "Correlation Heatmap"),
                                      fontsize_row = auto_fontsize,
                                      fontsize_col = auto_fontsize,
                                      display_numbers = show_numbers,
                                      number_color = "black",
                                      border_color = "grey60",
                                      cellwidth = max(10, 200 / length(group_biomarkers)),
                                      cellheight = max(10, 200 / length(group_biomarkers)))
    }, error = function(e) {
      cat("*Error creating heatmap for this group.*\n\n")
    })

    cat("\n")

    # Add boxplots if requested
    if (add_boxplots && !is.null(pheat_obj)) {
      cat("\n#### Distribution Boxplots\n")

      tryCatch({
        # Subset data for this group
        group_data <- samp_num[, group_biomarkers, drop = FALSE]

        # Extract the row order from pheatmap
        biomarker_order <- rownames(group_cor)[pheat_obj$tree_row$order]

        # Convert to long format for ggplot
        group_long <- group_data %>%
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "Biomarker",
                              values_to = "Value") %>%
          dplyr::mutate(Biomarker = factor(Biomarker, levels = biomarker_order))

        # Create boxplot
        p <- ggplot2::ggplot(group_long, ggplot2::aes(x = Biomarker, y = Value)) +
          ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = auto_fontsize),
                         axis.text.y = ggplot2::element_text(size = 8),
                         plot.title = ggplot2::element_text(size = 12, face = "bold")) +
          ggplot2::labs(title = paste("Group", i, "- Biomarker Distributions"),
                        x = "Biomarker",
                        y = "Value") +
          ggplot2::coord_flip()

        print(p)

      }, error = function(e) {
        cat("*Error creating boxplots for this group.*\n\n")
      })

      cat("\n\n")
    }
  }

  invisible(NULL)
}
