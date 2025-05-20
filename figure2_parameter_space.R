# --- Figure 2: Parameter Space (R vs L) ---

# Load the core model functions
source("ricker_model_functions.R") 

# Function to classify dynamics for a given (R, L) pair
classify_dynamics <- function(R, L, x0 = 0.3, n_iter = 1000) {
  x <- numeric(n_iter)
  x[1] <- x0
  first_extinction_step <- -1
  
  # Check immediate extinction after first step
  x[2] <- ricker_map(x[1], R, L)
  if (x[2] <= 0) {
    return("immediate") # Extinct at step 1
  }
  
  for (i in 2:(n_iter - 1)) {
    x[i+1] <- ricker_map(x[i], R, L)
    if (x[i+1] <= 0) {
      first_extinction_step <- i + 1
      break
    }
  }
  
  if (first_extinction_step == -1) {
    # Check if it's non-zero steady state or oscillation
    if(any(x[round(n_iter*0.8):n_iter] > 1e-6)) { # Check last 20% for activity
      return("survival") # Survived all iterations with positive values
    } else {
      return("eventual") # Collapsed to zero eventually
    }
  } else {
    return("eventual") # Extinct after step 1 but within n_iter
  }
}

# --- Generate the Data ---

# Define the grid of R and L values
R_grid <- seq(1.8, 4.0, length.out = 100) 
L_grid <- seq(0, 2.0, length.out = 100)   
param_grid <- expand.grid(R = R_grid, L = L_grid)

# Classify dynamics for each point in the grid
cat("Generating parameter space data for Figure 2...\n")
results_status <- apply(param_grid, 1, function(row) {
  classify_dynamics(R = row['R'], L = row['L'], n_iter = 1000)
})

cat("Done classifying.\n")

param_grid$status <- results_status

# --- Plotting Figure 2 ---
cat("Plotting Figure 2...\n")
dev.new(width=7, height=6)
plot(param_grid$R, param_grid$L, type = 'n', # Set up plot area
     xlab = "R", ylab = "L",
     main = "Figure 2: Survival Regions (Simulated)",
     xlim = range(R_grid), ylim = range(L_grid))

# Blank region (survival): Using light grey background points for visibility
points(param_grid$R[param_grid$status == "survival"],
       param_grid$L[param_grid$status == "survival"],
       pch = 15, col = "lightgrey", cex = 0.7) # Small squares

# Crossed region (eventual extinction): Using 'x' symbol
points(param_grid$R[param_grid$status == "eventual"],
       param_grid$L[param_grid$status == "eventual"],
       pch = 4, col = "black", cex = 0.6) 

# Dotted region (immediate extinction): Using '+' symbol
points(param_grid$R[param_grid$status == "immediate"],
       param_grid$L[param_grid$status == "immediate"],
       pch = 3, col = "blue", cex = 0.6) 

legend("topright", legend = c("Survival", "Eventual Extinction", "Immediate Extinction"),
       pch = c(15, 4, 3), col = c("lightgrey", "black", "blue"), 
       pt.cex = c(1.0, 0.6, 0.6), bg="white", title="Simulation Result")

cat("Figure 2 generation complete.\n")