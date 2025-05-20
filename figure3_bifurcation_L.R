# --- Figure 3: Bifurcation Diagram (L vs X) ---

# Load the core model functions
source("ricker_model_functions.R") 

# Function specific to generating L-based bifurcation diagrams
generate_bifurcation_L <- function(R, L_vals, x0 = 0.3, n_total = 1000, n_transient = 800, ...) {
  # Calculate number of points to plot per L
  n_plot <- n_total - n_transient
  if (n_plot <= 0) stop("n_total must be greater than n_transient")
  
  # Prepare storage for results
  results_L <- numeric(0)
  results_X <- numeric(0)
  
  cat("Generating bifurcation for R =", R, "\nProgress:")
  total_L <- length(L_vals)
  pb_step <- max(1, floor(total_L / 20)) # Progress update step
  
  for (i in 1:total_L) {
    L <- L_vals[i]
    # Simulate
    x_attractor <- simulate_ricker(R, L, x0, n_total, n_transient)
    # Keep only non-extinct points for plotting (avoid plotting exact zero)
    x_plot <- x_attractor[x_attractor > 1e-9] 
    
    if (length(x_plot) > 0) {
      results_L <- c(results_L, rep(L, length(x_plot)))
      results_X <- c(results_X, x_plot)
    }
    if (i %% pb_step == 0 || i == total_L) {
      cat(".")
    }
  }
  cat(" Done.\n")
  
  # Plotting
  plot(results_L, results_X, pch = '.', cex = 1.5,
       xlab = "L", ylab = "X", ylim = c(0, 2.5), ...)
}

# --- Generate the Plot ---

# Parameters for Fig 3
R_fig3 <- 2.6
L_values_fig3 <- seq(0, 1.6, length.out = 300) # Increased points for smoother plot
X0_fig3 <- 0.3 # Assuming same as Fig 1
N_TOTAL_FIG3 <- 1000 # Assuming same as Fig 1
N_TRANSIENT_FIG3 <- N_TOTAL_FIG3 - 200 # Assuming same as Fig 1

cat("\nGenerating Figure 3...\n")
dev.new(width=6, height=5)
generate_bifurcation_L(R = R_fig3, L_vals = L_values_fig3, x0 = X0_fig3,
                       n_total = N_TOTAL_FIG3, n_transient = N_TRANSIENT_FIG3,
                       main = "Figure 3 (R=2.6)")

cat("Figure 3 generation complete.\n")