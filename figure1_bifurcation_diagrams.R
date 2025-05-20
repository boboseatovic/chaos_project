# --- Figure 1: Bifurcation Diagrams (R vs X) ---

# Load the core model functions
source("ricker_model_functions.R") 

# Function specific to generating R-based bifurcation diagrams
generate_bifurcation_R <- function(L, R_vals, x0 = 0.3, n_total = 1000, n_transient = 800, ...) {
  # Calculate number of points to plot per R
  n_plot <- n_total - n_transient
  if (n_plot <= 0) stop("n_total must be greater than n_transient")
  
  # Prepare storage for results
  results_R <- numeric(0)
  results_X <- numeric(0)
  
  cat("Generating bifurcation for L =", L, "\nProgress:")
  total_R <- length(R_vals)
  pb_step <- max(1, floor(total_R / 20)) # Progress update step
  
  for (i in 1:total_R) {
    R <- R_vals[i]
    # Simulate
    x_attractor <- simulate_ricker(R, L, x0, n_total, n_transient)
    # Keep only non-extinct points for plotting
    x_plot <- x_attractor[x_attractor > 1e-9] 
    
    if (length(x_plot) > 0) {
      results_R <- c(results_R, rep(R, length(x_plot)))
      results_X <- c(results_X, x_plot)
    }
    
    if (i %% pb_step == 0 || i == total_R) {
      cat(".")
    }
  }
  cat(" Done.\n")
  
  # Plotting
  plot(results_R, results_X, pch = '.', cex = 1.5,
       xlab = "R", ylab = "X", ylim = c(0, 5.0), ...)
}

# Parameters from Fig 1 caption
R_values <- seq(1.8, 4.0, length.out = 300) # Increased points for smoother plot
X0_fig1 <- 0.3 # Starting point of the population
N_TOTAL_FIG1 <- 1000
# Caption says last 200 points were plotted, so transient is 1000 - 200 = 800
N_TRANSIENT_FIG1 <- N_TOTAL_FIG1 - 200 

# --- Generate the Plots ---

# Generate Figure 1a in a new window
cat("\nGenerating Figure 1a...\n")
dev.new(width=6, height=5)
generate_bifurcation_R(L = 0, R_vals = R_values, x0 = X0_fig1,
                       n_total = N_TOTAL_FIG1, n_transient = N_TRANSIENT_FIG1,
                       main = "Bifurcation diagram \n Figure 1a (L=0)")

# Generate Figure 1b in another new window
cat("\nGenerating Figure 1b...\n")
dev.new(width=6, height=5)
generate_bifurcation_R(L = 0.06, R_vals = R_values, x0 = X0_fig1,
                       n_total = N_TOTAL_FIG1, n_transient = N_TRANSIENT_FIG1,
                       main = "Bifurcation diagram \n Figure 1b (L=0.06)")

cat("\nFigure 1 generation complete.\n")