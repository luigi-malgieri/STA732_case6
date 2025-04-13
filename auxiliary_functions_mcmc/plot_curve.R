# plot_implied_curves.R

# Function to clean and convert posterior mean values
extract_numeric <- function(value) {
  as.numeric(gsub("\\*+", "", value))  # Remove any asterisks and convert to numeric
}

# Function to generate the implied curve
generate_function <- function(x, beta_1, beta_2, beta_3) {
  result <- beta_1 * 1 / (1 + exp(beta_3 * (x - beta_2)))
  return(result)
}

# Extract beta coefficients and plot implied curves for EE and ZM
plot_implied_curves <- function(posterior_table) {
  # Extract beta_EE values
  beta_EE1 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_EE1"])
  beta_EE2 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_EE2"])
  beta_EE3 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_EE3"])
  
  # Extract beta_ZM values
  beta_ZM1 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_ZM1"])
  beta_ZM2 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_ZM2"])
  beta_ZM3 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_ZM3"])
  
  # Set the range for EE and ZM
  EE_values <- seq(0, 10, length.out = 100)
  ZM_values <- seq(0, 1, length.out = 100)
  
  # Generate function values for EE
  result_ee <- generate_function(EE_values, beta_EE1, beta_EE2, beta_EE3)
  
  # Generate function values for ZM
  result_zm <- generate_function(ZM_values, beta_ZM1, beta_ZM2, beta_ZM3)
  
  # Plot for EE
  
  par(mfrow = c(1, 2))
  
  plot(EE_values, result_ee, type = "l", col = "blue",
       xlab = "EE", ylab = "Function Value",
       main = "Implied Plot for EE")
  
  # Plot for ZM
  plot(ZM_values, result_zm, type = "l", col = "green",
       xlab = "ZM", ylab = "Function Value",
       main = "Implied Plot for ZM")
}