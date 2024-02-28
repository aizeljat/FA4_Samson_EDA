#FA4 Samson

library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("C:\\Users\\User\\OneDrive\\FRESHMAN\\3rd yr - 2nd Sem\\Data Analysis\\mortality_by_latitude.csv")

# Check the structure of the dataset
str(data)

# 1. Plot mortality index against mean average temperature
ggplot(data, aes(x = temperature, y = mortality_index)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Mortality Index vs. Mean Average Temperature",
       x = "Mean Average Temperature",
       y = "Mortality Index")

# The plot is hollow up because the correlation between the mortality index and mean average temperature follows a U-shaped pattern. 
# The mortality index initially decreases as the mean average temperature increases but then rises again. This implies a non-linear relationship in which the mortality index is affected by the mean average temperature in a not monotonic manner. 
# This U-shaped pattern suggests that particular temperature ranges may be associated with lower mortality rates, whereas others may be associated with higher mortality rates.

# Apply a transformation to straighten out the relationship
data <- data %>%
  mutate(log_mortality_index = log(mortality_index))

# Plot residuals to check for any remaining patterns
ggplot(data, aes(x = temperature, y = residuals(lm(log_mortality_index ~ temperature, data = data)))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals Plot",
       x = "Mean Average Temperature",
       y = "Residuals")

# Apply a transformation to straighten out the relationship
data_transformed <- data %>%
  mutate(log_mortality_index = log(mortality_index))

# Plot transformed data
ggplot(data_transformed, aes(x = temperature, y = log_mortality_index)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Log Mortality Index vs. Mean Average Temperature",
       x = "Mean Average Temperature",
       y = "Log Mortality Index")


# 2. Plot log price as a function of carat with a loess smoother
ggplot(data, aes(x = temperature, y = log(mortality_index))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +
  labs(title = "Log Mortality Index vs. Mean Average Temperature with Loess Smoother",
       x = "Mean Average Temperature",
       y = "Log Mortality Index")


# 3. Compare the fit of loess smoother to polynomial + step function regression using residuals plot
# Fit a polynomial + step function regression
fit_lm <- lm(log(mortality_index) ~ poly(temperature, 3) + I(temperature^3 >= 2), data = data)

# Plot residuals for both models
residuals_df <- data.frame(
  temperature = data$temperature,
  loess_residuals = resid(loess(log(mortality_index) ~ temperature, data = data)),
  poly_step_residuals = residuals(fit_lm)
)

ggplot(residuals_df, aes(x = temperature)) +
  geom_line(aes(y = loess_residuals, color = "Loess Smoother")) +
  geom_line(aes(y = poly_step_residuals, color = "Polynomial + Step Function")) +
  labs(title = "Residuals Comparison",
       x = "Mean Average Temperature",
       y = "Residuals",
       color = "Model")

# The loess smoother's adaptability enables it to more accurately capture the fundamental structure of the data, resulting in residuals with fewer systematic patterns and a better reflection of the dataset's actual variability. 
# As a result, the loess smoother is frequently regarded as more faithful to the data because of its ability to provide a deeper representation of the underlying relationships.








