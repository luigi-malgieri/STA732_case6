library(ggplot2)

# Function to clean and convert posterior mean values
extract_numeric <- function(value) {
  as.numeric(gsub("\\*+", "", value))  # Remove any asterisks and convert to numeric
}


# Function to generate the implied curve
generate_function <- function(x, beta_0, beta_1, beta_2, beta_3) {
  result <- beta_0 + beta_1 * 1 / (1 + exp(beta_3 * (x - beta_2)))
  return(result)
}

# Adjusted function to plot implied curves for multiple labs using ggplot2
plot_implied_curves <- function(posterior_table, nlabs = 1, lab_names = NULL) {
  
  # Set the range for EE and ZM
  EE_values <- seq(0, 10, length.out = 100)
  ZM_values <- seq(0, 1, length.out = 100)
  
  # Prepare an empty data frame to store results for ggplot
  plot_data_ee <- data.frame(EE = EE_values, Value = NA, Lab = factor(0))
  
  if (nlabs == 1) {
    # Extract beta_EE values for a single lab
    beta_0 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_0"])
    beta_EE1 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_EE1"])
    beta_EE2 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_EE2"])
    beta_EE3 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_EE3"])
    
    # Generate function values for EE
    result_ee <- generate_function(EE_values, beta_0, beta_EE1, beta_EE2, beta_EE3)
    
    # Add to data frame for ggplot
    plot_data_ee$Value <- result_ee
    plot_data_ee$Lab <- "Lab 1"
    

    
    plot_data_ZM <- data.frame(ZM = ZM_values, Value = NA, Lab = factor(0))
    
    beta_0 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_0"])
    beta_ZM1 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_ZM1"])
    beta_ZM2 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_ZM2"])
    beta_ZM3 <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == "beta_ZM3"])
    
    # Generate function values for ZM
    result_ZM <- generate_function(ZM_values, beta_0, beta_ZM1, beta_ZM2, beta_ZM3)
    
    # Add to data frame for ggplot
    plot_data_ZM$Value <- result_ZM
    plot_data_ZM$Lab <- "Lab 1"
    
    par(mfrow = c(1, 2))
    
    # Plot for EE using ggplot
    ggplot(plot_data_ee, aes(x = EE, y = Value, color = Lab)) +
      geom_line(size = 1.5) +
      labs(x = "EE", y = "Function Value", title = "Implied Plot for EE") +
      theme_minimal()
    
    # Plot for ZM using ggplot
    ggplot(plot_data_ZM, aes(x = ZM, y = Value, color = Lab)) +
      geom_line(size = 1.5) +
      labs(x = "ZM", y = "Function Value", title = "Implied Plot for ZM") +
      theme_minimal()
    
  } else {
    # Prepare an empty data frame for plotting multiple labs
    plot_data_ee <- data.frame(EE = rep(EE_values, nlabs), Value = NA, Lab = factor(rep(1:nlabs, each = length(EE_values))))
    
    colors <- rainbow(nlabs)  
    for (l in 1:nlabs) {
      beta_0_l <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == paste0("beta_lab[", l, "]")])
      beta_EE1_l <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == paste0("beta_EE1[", l, "]")])
      beta_EE2_l <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == paste0("beta_EE2[", l, "]")])
      beta_EE3_l <- extract_numeric(posterior_table$`Posterior Mean`[posterior_table$Variable == paste0("beta_EE3[", l, "]")])
      
      result_ee_l <- generate_function(EE_values,beta_0_l, beta_EE1_l, beta_EE2_l, beta_EE3_l)
      
      # Update the Value column for the lab
      plot_data_ee$Value[plot_data_ee$Lab == l] <- result_ee_l
    }
    plot_data_ee$Lab <- factor(rep(lab_names, each = length(EE_values)))
    
    # Plot for EE using ggplot with multiple labs
    ggplot(plot_data_ee, aes(x = EE, y = Value, color = Lab)) +
      geom_line(size = 0.7) +
      labs(x = "EE", y = "b_l + bEE1_l /( 1 + exp(bEE3_l (EE -  bEE2_l) ) )", title = "Point estimates of response curve, by lab") +
      scale_color_manual(values = colors) +
      theme_minimal() +
      theme(legend.title = element_blank())
  }
}




##################################################################################

add_points <- function(points) {
  # Ensure the points are sorted
  points <- sort(points)
  
  # Initialize an empty vector for the result
  new_points <- c()
  
  # Iterate through intervals and add 5 equally spaced points
  for (i in seq_along(points)[-length(points)]) {
    interval_points <- seq(points[i], points[i + 1], length.out = 7)
    # Exclude the endpoint to avoid duplication
    new_points <- c(new_points, interval_points[-7])
  }
  
  # Add the last point back
  unique(c(new_points, points[length(points)]))
}

plot_f_plain <- function(f_matrix, x_points) {
  # Compute mean curve
  mean_curve <- colMeans(f_matrix)
  
  # Compute pointwise quantiles
  quantile_curve_lower <- apply(f_matrix, 2, quantile, probs = 0.025)
  quantile_curve_upper <- apply(f_matrix, 2, quantile, probs = 0.975)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    x = rep(x_points, times = nrow(f_matrix)),
    y = as.vector(t(f_matrix)),
    curve_type = rep("Sample Curves", each = length(x_points))
  )
  
  mean_data <- data.frame(x = x_points, y = mean_curve, curve_type = "Mean Curve")
  lower_quantile_data <- data.frame(x = x_points, y = quantile_curve_lower, curve_type = "Lower Quantile")
  upper_quantile_data <- data.frame(x = x_points, y = quantile_curve_upper, curve_type = "Upper Quantile")
  
  # Plotting
  ggplot() +
    # Very pale sample curves
    geom_line(data = plot_data, aes(x = x, y = y, group = rep(1:nrow(f_matrix), each = length(x_points))),
              color = "gray80", alpha = 0.3) +
    # Mean curve
    geom_line(data = mean_data, aes(x = x, y = y, color = curve_type), size = 1.2) +
    # Quantile curves
    geom_line(data = lower_quantile_data, aes(x = x, y = y, color = curve_type), size = 1.1, linetype = "dashed") +
    geom_line(data = upper_quantile_data, aes(x = x, y = y, color = curve_type), size = 1.1, linetype = "dashed") +
    scale_color_manual(values = c("Mean Curve" = "blue", 
                                  "Lower Quantile" = "red", 
                                  "Upper Quantile" = "red")) +
    labs(title = "Mean and Quantile Curves with Sample Functions",
         x = "Design Points", y = "Function Value") +
    theme_minimal()
}

plot_f_array <- function(f_array, x_points, centered = FALSE) {
  Nlab <- dim(f_array)[1]
  Ndesignpoint <- dim(f_array)[2]
  Nsamples <- dim(f_array)[3]
  
  plot_data <- data.frame()
  
  # Colors for labs
  lab_colors <- scales::hue_pal()(Nlab)
  
  for (lab_idx in 1:Nlab) {
    # Extract the slice for the current lab and transpose
    lab_slice <- t(f_array[lab_idx, , ])
    if (centered == TRUE){
      lab_slice <- lab_slice - as.vector(lab_slice[,1])
    } 
    
    # Compute mean curve and quantiles
    mean_curve <- colMeans(lab_slice)
    quantile_curve_lower <- apply(lab_slice, 2, quantile, probs = 0.025)
    quantile_curve_upper <- apply(lab_slice, 2, quantile, probs = 0.975)
    
    # Prepare the data for ggplot
    plot_data <- rbind(plot_data,
                       data.frame(x = x_points, y = mean_curve, lab = lab_idx, curve_type = "Mean"),
                       data.frame(x = x_points, y = quantile_curve_lower, lab = lab_idx, curve_type = "Lower Quantile"),
                       data.frame(x = x_points, y = quantile_curve_upper, lab = lab_idx, curve_type = "Upper Quantile")
    )
  }
  
  # Plot
  ggplot(plot_data, aes(x = x, y = y, color = factor(lab), linetype = curve_type)) +
    geom_line(size = 1.2) +
    scale_linetype_manual(values = c("Mean" = "solid", "Lower Quantile" = "dashed", "Upper Quantile" = "dashed")) +
    scale_color_manual(values = lab_colors) +
    labs(title = "Mean and Quantile Curves for Labs",
         x = "Design Points", y = "Function Value",
         color = "Lab") +
    theme_minimal()
}

effect_threshold <- function(f_matrix, x_points, alpha = 0.025) {
  # Shift each row so that the first design point value is 0
  shifted_matrix <- f_matrix - f_matrix[, 1]
  
  # Compute pointwise quantiles of the shifted matrix
  lower_quantile <- apply(shifted_matrix, 2, quantile, probs = alpha)
  
  # Determine the smallest column index where the lower quantile is above 0
  smallest_f_index <- which(lower_quantile > 0)[1]
  
  # Return the corresponding design point if the condition is met, or NA
  if (!is.na(smallest_f_index)) {
    return(x_points[smallest_f_index])
  } else {
    return(NA) # No column found satisfying the condition
  }
} 