# --- Figure 4: Cobweb Plots ---

# Load the core model functions
source("ricker_model_functions.R") 

# --- Modified Cobweb Plotting Function ---
plot_cobweb <- function(R, L, x0, n_steps, 
                        xlim = c(0, 1.5), ylim = c(0, 1.5), 
                        panel_title = "", ...) {
  
  # Generate the trajectory
  x_traj <- numeric(n_steps + 1)
  x_traj[1] <- x0
  for (i in 1:n_steps) {
    x_traj[i+1] <- ricker_map(x_traj[i], R, L)
    # Stop if extinct or below a tiny threshold to avoid plotting issues
    if (x_traj[i+1] <= 1e-9) { 
      x_traj[(i+1):length(x_traj)] <- NA 
      break
    }
  }
  # Keep only non-NA points for plotting segments
  valid_indices <- which(!is.na(x_traj))
  x_traj_plot <- x_traj[valid_indices]
  
  # Sequence for plotting the map function F(X) = X*exp(R(1-X)) - L
  x_func <- seq(min(xlim) - 0.1, max(xlim) + 0.1, length.out = 300) 
  y_func <- sapply(x_func, function(x) ricker_map(x, R, L))
  
  # --- Plotting ---
  # Set up plot area
  plot(NA, type = 'n', # Create empty plot first
       xlab = expression(X[i]), ylab = expression(X[i+1]),
       xlim = xlim, ylim = ylim, ...)
  
  # Add the map function F(X) - Solid black line, slightly thicker
  lines(x_func, y_func, col = "black", lwd = 1.5, lty = 1) 
  
  # Add the y=x diagonal line - Dashed grey line
  abline(0, 1, col = "grey50", lty = 2) 
  
  # Add cobweb lines for the valid (non-extinct) part of the trajectory
  # Black lines with arrows in the middle 
  if (length(x_traj_plot) > 1) {
    for (i in 1:(length(x_traj_plot))) {
      if (i == 1) {
      # Vertical line from (Xi, 0) to (Xi, Xi + 1) for the frist time
      arrows(x_traj_plot[i], 0, x_traj_plot[i], (x_traj_plot[i+1]/2),
               col = "black", length = 0.03)
      segments(x_traj_plot[i], (x_traj_plot[i+1]/2), x_traj_plot[i], x_traj_plot[i+1],
                 col = "black")
      } else if (i == length(x_traj_plot)) {
      # Vertical line from (Xi, Xi) to (Xi, 0) for the last time
      arrows(x_traj_plot[i], x_traj_plot[i], x_traj_plot[i], (x_traj_plot[i]/2),
               col = "black", length = 0.03)
      segments(x_traj_plot[i], (x_traj_plot[i]/2), x_traj_plot[i], 0,
                 col = "black")
      }  
      else {
      # Vertical line from (Xi, Xi) to (Xi, Xi+1) for every other time
        if(x_traj_plot[i] < x_traj_plot[i+1]) {
          arrows(x_traj_plot[i], x_traj_plot[i], x_traj_plot[i], (x_traj_plot[i] + ((x_traj_plot[i+1]-x_traj_plot[i])/2)), 
                    col = "black", length = 0.03)
          segments(x_traj_plot[i], (x_traj_plot[i] + ((x_traj_plot[i+1]-x_traj_plot[i])/2)), x_traj_plot[i], x_traj_plot[i+1], 
                    col = "black")
        }
        else {
          arrows(x_traj_plot[i], x_traj_plot[i], x_traj_plot[i], (x_traj_plot[i] - ((x_traj_plot[i]-x_traj_plot[i+1])/2)), 
                 col = "black", length = 0.03)
          segments(x_traj_plot[i], (x_traj_plot[i] - ((x_traj_plot[i]-x_traj_plot[i+1])/2)), x_traj_plot[i], x_traj_plot[i+1], 
                   col = "black")
        }
      }
      # Horizontal line from (Xi, Xi+1) to (Xi+1, Xi+1)
      arrows(x_traj_plot[i], x_traj_plot[i+1], (x_traj_plot[i] + (x_traj_plot[i+1]-x_traj_plot[i])/2), x_traj_plot[i+1], 
               col = "black", length = 0.03)
      segments((x_traj_plot[i] + (x_traj_plot[i+1]-x_traj_plot[i])/2), x_traj_plot[i+1], x_traj_plot[i+1], x_traj_plot[i+1],
               col = "black")
    }
  } 
  
  # Add panel title (e.g., "a")
  title(main = panel_title, adj = 0.05, line = 0.5, cex.main = 1) # Place title inside top-left
  
  # Add box around plot area
  box() 
}

# --- Generate the Plots ---

# Parameters from Fig 4 caption
X0_fig4 <- 0.3
N_STEPS_FIG4 <- 15 

cat("\nGenerating Figure 4 panels...\n")
# Set up plot window and layout for 2x2 grid
dev.new(width=7, height=7)
par(mfrow = c(2, 2), mar=c(4, 4, 1.5, 1), oma=c(0,0,2,0)) 

# --- Panel-Specific Calls with Adjusted Limits ---

# Fig 4a: R=1.8, L=0.77 (Decay to zero/low value)
# Estimated axis limits from paper: X ~ 0-1.2, Y ~ 0-0.5
plot_cobweb(R = 1.8, L = 0.77, x0 = X0_fig4, n_steps = N_STEPS_FIG4, 
            xlim=c(0, 1.2), ylim=c(0, 0.5), panel_title = "a") 

# Fig 4b: R=1.8, L=0.81 (Faster decay/extinction)
# Estimated axis limits from paper: X ~ 0-1.2, Y ~ 0-0.5
plot_cobweb(R = 1.8, L = 0.81, x0 = X0_fig4, n_steps = N_STEPS_FIG4, 
            xlim=c(0, 1.2), ylim=c(0, 0.5), panel_title = "b")

# Fig 4c: R=2.6, L=0.8 (Chaotic-like expansion/oscillation)
# Estimated axis limits from paper: X ~ 0-1.5, Y ~ 0-1.5
plot_cobweb(R = 2.6, L = 0.8, x0 = X0_fig4, n_steps = N_STEPS_FIG4, 
            xlim=c(0, 1.5), ylim=c(0, 1.5), panel_title = "c")

# Fig 4d: R=2.6, L=1.58 (Decay to stable point / near extinction?)
# Estimated axis limits from paper: X ~ 0-1.5, Y ~ 0-0.5 (Function peak is high, but dynamics are low)
plot_cobweb(R = 2.6, L = 1.58, x0 = X0_fig4, n_steps = N_STEPS_FIG4, 
            xlim=c(0, 1.5), ylim=c(0, 0.5), panel_title = "d")

mtext("Figure 4: Cobweb Plots", outer = TRUE, cex = 1.2, line=0.5)

# Reset plotting layout
par(mfrow = c(1, 1), mar=c(5.1, 4.1, 4.1, 2.1), oma=c(0,0,0,0)) 

cat("Figure 4 generation complete.\n")