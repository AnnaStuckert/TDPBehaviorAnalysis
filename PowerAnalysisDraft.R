power.t.test(delta = 50, sd = 35, power = 0.8)

install.packages("simr")
install.packages("lme4")
library(simr)
library(lme4)

# Install and load necessary packages
install.packages("simr")
install.packages("lme4")
library(simr)
library(lme4)

# Define parameters
mean_diff <- 1052
std_dev <- 443
timepoints <- 3
effect_size <- mean_diff / std_dev
alpha <- 0.05
power <- 0.8

# Simulate data for a larger initial number of participants
set.seed(123)
n_initial <- 50  # Start with a larger number to avoid singular fits
data <- expand.grid(subject = factor(1:n_initial), time = factor(1:timepoints))
data$y <- rnorm(n_initial * timepoints, mean = rep(seq(0, mean_diff, length.out = timepoints), each = n_initial), sd = std_dev)

# Fit a linear mixed model
model <- lmer(y ~ time + (1|subject), data = data)

# Define the power simulation function
power_sim <- function(n) {
  data <- expand.grid(subject = factor(1:n), time = factor(1:timepoints))
  data$y <- rnorm(n * timepoints, mean = rep(seq(0, mean_diff, length.out = timepoints), each = n), sd = std_dev)
  model <- lmer(y ~ time + (1|subject), data = data)
  power_result <- tryCatch({
    powerSim(model, nsim = 100)$power
  }, error = function(e) {
    NA
  })
  return(power_result)
}

# Find the sample size needed to achieve the desired power
target_power <- 0.8
sample_size <- NA

for (n in seq(20, 200, by = 10)) {  # Start from a higher number to avoid initial errors
  current_power <- power_sim(n)
  if (!is.na(current_power) && current_power >= target_power) {
    sample_size <- n
    break
  }
}

# Print the required sample size
cat("Required sample size per group:", sample_size)
