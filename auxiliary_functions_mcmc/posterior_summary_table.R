library(coda)
library(kableExtra)

# Function to compute and display the posterior summary table
posterior_summary_table <- function(samples) {
  # Compute summary statistics
  summary_stats <- summary(samples)

  # Compute probability that the posterior is bounded away from 0
  posterior_matrix <- as.matrix(samples)
  pd <- apply(posterior_matrix, 2, function(samples) {
    max(mean(samples > 0), mean(samples < 0))
  })

  # Extract relevant statistics
  means <- summary_stats$statistics[, "Mean"]
  lower <- summary_stats$quantiles[, "2.5%"]
  upper <- summary_stats$quantiles[, "97.5%"]

  # Create the simplified regression table
  regression_table <- data.frame(
    Variable = rownames(summary_stats$statistics),
    Mean_with_Stars = paste0(
      round(means, 3),
      ifelse(
        pd > 0.999, "***",
        ifelse(
          pd > 0.99, "**",
          ifelse(pd > 0.95, "*", "")
        )
      )
    ),
    Credible_Interval = paste0("[", round(lower, 3), ", ", round(upper, 3), "]")
  )

  # Rename columns in the regression_table
  names(regression_table) <- c("Variable", "Posterior Mean", "95% Credible Interval")

  # Render the table using kableExtra
  regression_table %>%
    kbl(
      caption = "Simplified Summary of Regression Coefficients",
      align = "c"
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  # Return the regression table (for further processing if needed)
  return(regression_table)
}

# Example usage
# library(coda)
# samples <- readRDS("path_to_your_chains.rds")
# posterior_summary_table(samples)
