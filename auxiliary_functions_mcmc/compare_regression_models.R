compare_regression_models <- function(y, n, yhat1, p1, yhat2, p2, yhat3, p3) {
  
  # Function to calculate R-squared
  calculate_r_squared <- function(y, yhat) {
    rss <- sum((y - yhat)^2)
    tss <- sum((y - mean(y))^2)
    r_squared <- 1 - (rss / tss)
    return(r_squared)
  }
  
  # Function to calculate Adjusted R-squared
  calculate_adjusted_r_squared <- function(r_squared, n, p) {
    adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
    return(adjusted_r_squared)
  }
  
  # Function to calculate RMSE
  calculate_rmse <- function(y, yhat) {
    rmse <- sqrt(mean((y - yhat)^2))
    return(rmse)
  }
  
  # Function to calculate AIC
  calculate_aic <- function(y, yhat, n, p) {
    rss <- sum((y - yhat)^2)
    aic <- log(rss / n) + 2 * p
    return(aic)
  }
  
  # Function to calculate BIC
  calculate_bic <- function(y, yhat, n, p) {
    rss <- sum((y - yhat)^2)
    bic <- log(rss / n) + p * log(n)
    return(bic)
  }
  
  
  # Calculate metrics for each model
  metrics <- data.frame(
    R2 = c(calculate_r_squared(y, yhat1), calculate_r_squared(y, yhat2), calculate_r_squared(y, yhat3)),
    Adjusted_R2 = c(calculate_adjusted_r_squared(calculate_r_squared(y, yhat1), n, p1),
                    calculate_adjusted_r_squared(calculate_r_squared(y, yhat2), n, p2),
                    calculate_adjusted_r_squared(calculate_r_squared(y, yhat3), n, p3)),
    RMSE = c(calculate_rmse(y, yhat1), calculate_rmse(y, yhat2), calculate_rmse(y, yhat3)),
    AIC = c(calculate_aic(y, yhat1, n, p1), calculate_aic(y, yhat2, n, p2), calculate_aic(y, yhat3, n, p3)),
    BIC = c(calculate_bic(y, yhat1, n, p1), calculate_bic(y, yhat2, n, p2), calculate_bic(y, yhat3, n, p3))
  )
  
  # Set row names for each metric
  rownames(metrics) <- c("Model 1", "Model 2", "Model 3")
  
  return(t(metrics))
}
