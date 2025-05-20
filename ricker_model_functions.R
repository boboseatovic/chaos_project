# --- Core Model Functions ---
# Contains functions needed by multiple figure scripts.

# The Ricker map with constant emigration/harvesting
# X_next = X * exp(R * (1 - X)) - L
ricker_map <- function(x, R, L) {
  if (x <= 0) {
    return(0) # Population is extinct
  }
  x_next <- x * exp(R * (1 - x)) - L
  # Ensure extinction if result is non-positive
  return(max(0, x_next))
}

# Function to simulate one trajectory
# Returns points after the transient period
simulate_ricker <- function(R, L, x0, n_total, n_transient) {
  if (n_total <= n_transient) stop("n_total must be greater than n_transient")
  x <- numeric(n_total)
  x[1] <- x0
  
  for (i in 1:(n_total - 1)) {
    x[i+1] <- ricker_map(x[i], R, L)
    # Stop if extinct
    if (x[i+1] <= 0) {
      # Keep the remaining values at 0 for accurate attractor representation
      x[(i+2):n_total] <- 0
      break
    }
  }
  
  # Return the points after the transient period
  return(x[(n_transient + 1):n_total])
}

cat("Core Ricker model functions loaded.\n")