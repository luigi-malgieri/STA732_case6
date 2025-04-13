extract_posterior_samples <- function(posterior_samples, nppsamps) {
  num_chains <- length(posterior_samples)
  
  # Extract and trim the last `nppsamps` samples from each chain
  y_pp_list <- lapply(posterior_samples, function(chain) {
    y_pp <- chain[, grep("y_pp", colnames(chain))]
    y_pp[(nrow(y_pp) - nppsamps + 1):nrow(y_pp), , drop = FALSE]
  })
  
  # Interleave samples from all chains
  y_pp_interleaved <- do.call(cbind, y_pp_list)  # Stack all chains side by side
  y_pp_interleaved <- as.vector(t(y_pp_interleaved))  # Transpose and unroll by row
  
  # Reshape into matrix form
  y_pp_matrix <- matrix(y_pp_interleaved, nrow = nppsamps * num_chains, byrow = TRUE)
  
  return(y_pp_matrix)
}