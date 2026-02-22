# =========================== R Code Fragment ===========================
# 1. Data Simulation using Acceptance-Rejection Method
# Define the target joint density function f(x, y)
f_xy <- function(x, y) {
  # The density is 4 * (x + y^2) inside the region D, and 0 otherwise.
  # Region D: 0 <= x, 0 <= y, x + y < 1
  is_in_D <- (x >= 0) & (y >= 0) & (x + y < 1)
  density_val <- ifelse(is_in_D, 4 * (x + y^2), 0)
  return(density_val)
}

# The maximum value M for the A-R method on the bounding box [0, 1] x [0, 1]
M <- 4

# Function to generate N samples
generate_samples <- function(N) {
  samples_X <- numeric(N)
  samples_Y <- numeric(N)
  i <- 1
  
  # Loop until N valid samples are accepted
  while (i <= N) {
    # Propose (x_prop, y_prop) from the uniform distribution U([0, 1] x [0, 1])
    x_prop <- runif(1, 0, 1)
    y_prop <- runif(1, 0, 1)
    
    # Check if (x_prop, y_prop) is in the region D (x + y < 1)
    if (x_prop + y_prop < 1) {
      # Acceptance step: Check if U < f(x, y) / (M * g(x, y))
      # Since g(x, y) = 1 and M = 4, the condition is U < f(x, y) / 4
      u <- runif(1, 0, 1)
      if (u < f_xy(x_prop, y_prop) / M) {
        samples_X[i] <- x_prop
        samples_Y[i] <- y_prop
        i <- i + 1
      }
    }
  }
  return(data.frame(X = samples_X, Y = samples_Y))
}

# Define sample sizes for simulation
N_list <- c(50, 100, 500, 1000, 5000) # Extend to 500, 1000, 5000 for better results

# Generate all required datasets
datasets <- lapply(N_list, generate_samples)
names(datasets) <- paste0("N_", N_list)
# =======================================================================

# =========================== R Code Fragment ===========================
# 2. OLS Estimation and Results Collection

# Store the theoretical results
beta0_true <- 2/3
beta1_true <- -3/4
theoretical_results <- data.frame(
  Parameter = c("beta_0", "beta_1"),
  True_Value = c(beta0_true, beta1_true)
)

# Function to compute OLS estimates for a given dataset
compute_ols_estimates <- function(data) {
  # Perform linear regression: Y ~ X
  ols_model <- lm(Y ~ X, data = data)
  
  # Extract coefficients
  beta0_hat <- coef(ols_model)[1]
  beta1_hat <- coef(ols_model)[2]
  
  return(c(beta0_hat = beta0_hat, beta1_hat = beta1_hat))
}

# Apply OLS to all simulated datasets
ols_results <- do.call(rbind, lapply(datasets, compute_ols_estimates))
ols_results <- as.data.frame(ols_results)
ols_results$N <- N_list

# Format the results table
ols_summary <- data.frame(
  N = ols_results$N,
  beta0_hat = ols_results$beta0_hat,
  beta1_hat = ols_results$beta1_hat,
  Bias_beta0 = ols_results$beta0_hat - beta0_true,
  Bias_beta1 = ols_results$beta1_hat - beta1_true
)

# Print the OLS estimates in a summary table
print("Theoretical Parameters:")
print(theoretical_results)
print("OLS Estimates Summary:")
print(ols_summary)
# =======================================================================

# 3. Visualization (Output to file using ggsave)
# =========================== 修改片段 (使用 R 基础绘图系统) #===========================
# =========================== 修改片段 (使用 R 基础绘图系统) #===========================
# 3. Visualization (Output to file using Base R Graphics)

# Theoretical Conditional Expectation E[Y | X=x]
conditional_expectation <- function(x) {
  # E[Y | X=x] = 3*(1-x)*(x^2 + 1) / (4*(x^2 + x + 1))
  return(3 * (1 - x) * (x^2 + 1) / (4 * (x^2 + x + 1)))
}

# Define True Parameters
beta0_true <- 2/3
beta1_true <- -3/4

# Use the largest dataset for the scatter plot (N=5000)
N_max <- max(N_list) # N_list = c(50, 100, 500, 1000, 5000)
data_plot <- datasets[[paste0("N_", N_max)]]

# Define the N values for which to plot the estimated OLS lines
N_plot_list <- c(50, 100, 1000, N_max) 

# Define colors and line types for estimated OLS lines for clear distinction
ols_colors <- c("red", "orange", "purple", "maroon")
ols_ltys <- 3:6
ols_legend_labels <- paste0("OLS Est. (N=", N_plot_list, ")")

# Create sequence for smooth lines
x_seq <- seq(0, 1, length.out = 100)
y_cond_exp <- conditional_expectation(x_seq)
y_theo_reg <- beta0_true + beta1_true * x_seq

# Setup output file
output_file_name <- paste0("Regression_Simulation_MultiN_BaseR.png")
png(filename = output_file_name, width = 800, height = 800, units = "px")

# Start plotting: Scatter plot uses the largest dataset N_max
plot(data_plot$X, data_plot$Y, 
     main = paste0("Conditional Expectation and OLS Regression Lines"),
     xlab = "X", ylab = "Y",
     xlim = c(0, 1), ylim = c(0, 1),
     pch = 20, col = rgb(0, 0, 0, 0.2), # Lighter scatter plot for background
     cex = 0.8) 

# Add Theoretical Conditional Expectation E[Y | X=x]
lines(x_seq, y_cond_exp, col = "blue", lwd = 3, lty = 1)

# Add Theoretical Optimal Regression Line
lines(x_seq, y_theo_reg, col = "darkgreen", lwd = 2, lty = 2)

# Add Estimated OLS Regression Lines for N=50, 100, 1000, 5000
for (i in 1:length(N_plot_list)) {
  N_current <- N_plot_list[i]
  # Retrieve estimates for the current N
  ols_current <- ols_results[ols_results$N == N_current, ]
  beta0_hat <- ols_current$beta0_hat
  beta1_hat <- ols_current$beta1_hat
  
  # Plot the estimated line
  abline(a = beta0_hat, b = beta1_hat, 
         col = ols_colors[i], 
         lwd = 1.5, 
         lty = ols_ltys[i])
}

# Add a Legend
legend_labels <- c("Simulated Data (N=5000)", 
                   "E[Y|X=x] (Theoretical)", 
                   "Theoretical Optimal Regression", 
                   ols_legend_labels)
legend_colors <- c(rgb(0, 0, 0, 0.2), "blue", "darkgreen", ols_colors)
legend_lwd <- c(NA, 3, 2, rep(1.5, length(N_plot_list)))
legend_lty <- c(NA, 1, 2, ols_ltys)
legend_pch <- c(20, NA, NA, rep(NA, length(N_plot_list)))


legend("topright", 
       legend = legend_labels,
       col = legend_colors,
       lwd = legend_lwd,
       lty = legend_lty,
       pch = legend_pch,
       bty = "n",
       cex = 0.8) # Adjust size for better fit

# Finalize the plot and save the file
dev.off() 

cat(paste0("Plot saved to file: ", output_file_name, "\n"))
# =======================================================================