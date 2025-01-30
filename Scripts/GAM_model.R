
# packages
library(readr)
library(tidyr)
library(dplyr)
library(car)
library(ggplot2)
library(viridis)
library(binom)
library(mgcv)

setwd("C:/Users/jacob/OneDrive/Desktop/Sensiromotor Lab")


dat <- read_csv("trial_results_ex_jan1_25.csv")


# clean data

# change launch angle to a numerical variable
dat$launch_angle <- as.numeric(dat$launch_angle)
str(dat$launch_angle)


str(dat$target_hit)

# rename water_speed_m/s to water_speed_m_s
colnames(dat)[which(names(dat) == "water_speed_m/s")] <- "water_speed_m_s"
dat$water_speed_m_s <- as.factor(dat$water_speed_m_s)
dat$target_angle <- as.factor(dat$target_angle)


# check
head(dat)
summary(dat)


mod_gam_x <- gam(final_ball_pos_x ~ s(launch_angle) + s(launch_Speed) + water_speed_m_s, data = dat)
summary(mod_gam_x)


plot(mod_gam_x, pages = 1, residuals = TRUE, se = TRUE)  # evidence for non linear effects

# extract fitted values and residuals
fitted_values <- fitted(mod_gam_x)
residuals_x <- residuals(mod_gam_x)

plot(fitted_values, residuals_x,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Check the distribution of residuals
hist(residuals_x, main = "Distribution of Residuals", xlab = "Residuals")





dat_clean <- na.omit(dat)
count(dat_clean)

weights_x <- 1 / abs(residuals(mod_gam_x))
length(weights)


mod_gam_x_weighted <- gam(final_ball_pos_x ~ s(launch_angle) + s(launch_Speed) + water_speed_m_s, data = dat_clean, weights = weights)
summary(mod_gam_x_weighted)
gam.check(mod_gam_x)


plot(mod_gam_x_weighted, pages = 1, residuals = TRUE, se = TRUE)  # evidence for non linear effects

# extract fitted values and residuals
fitted_values_x <- fitted(mod_gam_x_weighted)
residuals_x <- residuals(mod_gam_x_weighted)

plot(fitted_values_x, residuals_x,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


# =================== most updated model (x pos) ==========================

mod_gam_x_weighted_k <- gam(final_ball_pos_x ~ s(launch_angle, k = 20) + s(launch_Speed, k = 20) + water_speed_m_s + target_angle, 
                            data = dat_clean, weights = weights_x, select = TRUE, gamma = 2)
summary(mod_gam_x_weighted_k)
gam.check(mod_gam_x_weighted_k)

plot(mod_gam_x_weighted_k, pages = 1, residuals = TRUE, se = TRUE)  # evidence for non linear effect

residuals_x_weighted_k <- residuals(mod_gam_x_weighted_k)

# Check the distribution of residuals
hist(residuals_x_weighted_k, main = "Distribution of Residuals", xlab = "Residuals")

plot(mod_gam_x_weighted_k, pages = 1, residuals = TRUE, se = TRUE)

qqnorm(residuals(mod_gam_x_weighted_k))
qqline(residuals(mod_gam_x_weighted_k), col = "red")


## smoothed version: 

mod_gam_x_weighted_k_smooth <- gam(final_ball_pos_x ~ s(launch_angle, k = 10) + s(launch_Speed, k = 10) + water_speed_m_s + target_angle, 
                                   data = dat_clean, weights = weights_x, select = TRUE, gamma = 2)
summary(mod_gam_x_weighted_k_smooth)
gam.check(mod_gam_x_weighted_k_smooth)

plot(mod_gam_x_weighted_k_smooth, pages = 1, residuals = TRUE, se = TRUE)  # evidence for non linear effect

residuals_x_weighted_k_smooth <- residuals(mod_gam_x_weighted_k_smooth)

# Check the distribution of residuals
hist(residuals_x_weighted_k_smooth, main = "Distribution of Residuals", xlab = "Residuals")

plot(mod_gam_x_weighted_k_smooth, pages = 1, residuals = TRUE, se = TRUE)

qqnorm(residuals(mod_gam_x_weighted_k_smooth))
qqline(residuals(mod_gam_x_weighted_k_smooth), col = "red")







## cross validation for x GAM ====

set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(dat_clean), 0.7 * nrow(dat_clean))
train_dat <- dat_clean[train_indices, ]
test_dat <- dat_clean[-train_indices, ]

mod_gam_x_train <- gam(final_ball_pos_x ~ s(launch_angle, k = 20) + s(launch_Speed, k = 20) + water_speed_m_s, 
                       data = train_dat, select = TRUE, gamma = 2)
residuals_x_train <- residuals(mod_gam_x_train)


# Calculate weights: Inverse of absolute residuals
weights_x_train <- 1 / abs(residuals_x_train)

count(train_dat)
length(weights_x_train)





# training weighted model
mod_gam_x_weighted_k_train <-  gam(final_ball_pos_x ~ s(launch_angle, k = 20) + s(launch_Speed, k = 20) + water_speed_m_s, 
                                   data = train_dat, weights = weights_x_train, select = TRUE, gamma = 2)

summary(mod_gam_x_weighted_k_train)
gam.check(mod_gam_x_weighted_k_train)

residuals_x_weighted_train <- residuals(mod_gam_x_weighted_k_train)

# Check the distribution of residuals
hist(residuals_x_weighted_train, main = "Distribution of Residuals", xlab = "Residuals")

qqnorm(residuals(mod_gam_x_weighted_k_train))
qqline(residuals(mod_gam_x_weighted_k_train), col = "red")

residuals_train <- residuals(mod_gam_x_weighted_k_train)
predicted_train <- predict(mod_gam_x_weighted_k_train, newdata = train_dat)

plot(predicted_train, residuals_train,
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residuals vs. Predicted Values")
abline(h = 0, col = "red")


plot(mod_gam_x_weighted_k_train, pages = 1, residuals = TRUE, se = TRUE)

predictions_x_test <- predict(mod_gam_x_weighted_k_train, newdata = test_dat)

# Observed vs. Predicted for the Test Set
observed_x_test <- test_dat$final_ball_pos_x

# Calculate RMSE (test)
rmse_test <- sqrt(mean((observed_x_test - predictions_x_test)^2))
print(paste("RMSE on Test Set:", rmse_test))

# Calculate MAE (test)
mae_test <- mean(abs(observed_x_test - predictions_x_test))
print(paste("MAE on Test Set:", mae_test))



predictions_x_train <- predict(mod_gam_x_weighted_k_train, newdata = train_dat)

observed_x_train <- train_dat$final_ball_pos_x

# Calculate RMSE (training)
rmse_train <- sqrt(mean((observed_x_train - predictions_x_train)^2))
print(paste("RMSE on Train Set:", rmse_train))

# Calculate MAE (training)
mae_test <- mean(abs(observed_x_train - predictions_x_train))
print(paste("MAE on Train Set:", mae_test))


# Print results
print(paste("RMSE on Train Set (original scale):", rmse_train_original))
print(paste("MAE on Train Set (original scale):", mae_train_original))
print(paste("RMSE on Test Set (original scale):", rmse_test_original))
print(paste("MAE on Test Set (original scale):", mae_test_original))


## z ====

mod_gam_z <- gam(final_ball_pos_z ~ s(launch_Speed, k = 20) + s(launch_angle, k = 20), data = dat_clean, select = TRUE, gamma = 2)
summary(mod_gam_z)

gam.check(mod_gam_z)

plot(mod_gam_z, pages = 1, residuals = TRUE, se = TRUE)

qqnorm(residuals(mod_gam_z))
qqline(residuals(mod_gam_z), col = "red")

residuals_z <- residuals(mod_gam_z)

# Check the distribution of residuals
hist(residuals_z, main = "Distribution of Residuals", xlab = "Residuals")






# overall models at this stage:

# ============================================================================================================================================
mod_gam_x_weighted_k <- gam(final_ball_pos_x ~ s(launch_angle, k = 20) + s(launch_Speed, k = 20) + water_speed_m_s + target_angle, 
                            data = dat_clean, weights = weights_x, select = TRUE, gamma = 2)


mod_gam_z <- gam(final_ball_pos_z ~ s(launch_Speed, k = 20) + s(launch_angle, k = 20), data = dat_clean, select = TRUE, gamma = 2)

# ============================================================================================================================================

# Predict values for mod_gam_x_weighted_k
pred_x <- predict(mod_gam_x_weighted_k, newdata = dat_clean)

# Calculate residuals
residuals_x <- dat_clean$final_ball_pos_x - pred_x

# Compute RMSE and MAE for mod_gam_x_weighted_k
rmse_x <- sqrt(mean(residuals_x^2)) # RMSE
mae_x <- mean(abs(residuals_x))     # MAE

# Print results
cat("RMSE for mod_gam_x_weighted_k:", rmse_x, "\n")
cat("MAE for mod_gam_x_weighted_k:", mae_x, "\n")


mod_gam_z <- gam(final_ball_pos_z ~ s(launch_Speed, k = 20) + s(launch_angle, k = 20), data = dat_clean, select = TRUE, gamma = 2)

# Predict values for mod_gam_z
pred_z <- predict(mod_gam_z, newdata = dat_clean)

# Calculate residuals
residuals_z <- dat_clean$final_ball_pos_z - pred_z

# Compute RMSE and MAE for mod_gam_z
rmse_z <- sqrt(mean(residuals_z^2)) # RMSE
mae_z <- mean(abs(residuals_z))     # MAE

# Print results
cat("RMSE for mod_gam_z:", rmse_z, "\n")
cat("MAE for mod_gam_z:", mae_z, "\n")



# extract upper and lower bounds for CI
results_3$predicted_x <- predictions$fit
results_3$lower_x <- predictions$fit - 1.96 * predictions$se.fit  # Lower bound (95% CI)
results_3$upper_x <- predictions$fit + 1.96 * predictions$se.fit  # Upper bound (95% CI)

results_3 <- solution_space_final_pos_gam(
  model_x = mod_gam_x_weighted_k_smooth, 
  model_z = mod_gam_z,  
  dat = dat_clean,  
  search_range = list(
    launch_angle = seq(min(dat_clean$launch_angle, na.rm = TRUE), max(dat_clean$launch_angle, na.rm = TRUE), by = 1),
    launch_Speed = seq(min(dat_clean$launch_Speed, na.rm = TRUE), max(dat_clean$launch_Speed, na.rm = TRUE), by = 0.2)
  )
)

# ========================================= model confidence ======

results_3_filt <- results_3 %>%
  filter(water_speed_m_s == "-4.004162") %>%
  filter(target_angle == 0)

# Plot for final_ball_pos_x
ggplot(results_3_filt, aes(x = launch_angle, y = final_ball_pos_x)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = final_ball_pos_x_lower, ymax = final_ball_pos_x_upper), 
              alpha = 1, fill = "blue") +
  facet_grid(target_angle ~ ., scales = "free_y") +
  labs(
    title = "Confidence in Final Ball Position (X)",
    x = "Launch Angle [°]",
    y = "Predicted Final Ball Position (X) [m]"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot for final_ball_pos_z
ggplot(results_3, aes(x = launch_angle, y = final_ball_pos_z, color = as.factor(water_speed_m_s))) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = final_ball_pos_z_lower, ymax = final_ball_pos_z_upper, fill = as.factor(water_speed_m_s)), 
              alpha = 0.3, color = NA) +
  facet_grid(target_angle ~ ., scales = "free_y") +
  labs(
    title = "Confidence in Final Ball Position (Z)",
    x = "Launch Angle [°]",
    y = "Predicted Final Ball Position (Z) [m]",
    color = "Water Speed [m/s]",
    fill = "Water Speed [m/s]"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


# analyzing trends in raw data
ggplot(dat, aes(x = launch_angle, y = final_ball_pos_x, color = water_speed_m_s)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ target_angle, scales = "free_y") +
  labs(
    title = "Raw Data: Final Ball Position (X) vs Launch Angle",
    x = "Launch Angle [°]",
    y = "Final Ball Position (X) [m]"
  ) +
  theme_minimal()


# Select numeric columns
cor_data <- dat %>% select(launch_angle, launch_Speed, final_ball_pos_x)

# Compute correlation
cor_matrix <- cor(cor_data, use = "complete.obs")
print(cor_matrix)


# Compute residuals
dat$residuals_x <- dat$final_ball_pos_x - predict(mod_gam_x_weighted_k, newdata = dat)

# Residual plot
ggplot(dat, aes(x = launch_angle, y = residuals_x, color = water_speed_m_s)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ target_angle, scales = "free_y") +
  labs(
    title = "Residuals of Model for Final Ball Position (X)",
    x = "Launch Angle [°]",
    y = "Residuals [m]"
  ) +
  theme_minimal()


# Plot observed vs. predicted
ggplot() +
  geom_point(data = dat_clean, aes(x = launch_angle, y = final_ball_pos_x, color = water_speed_m_s), alpha = 0.5) +
  geom_line(data = results_3, aes(x = launch_angle, y = final_ball_pos_x, color = water_speed_m_s), size = 1, alpha = 0.5) +
  facet_grid(~ target_angle + water_speed_m_s, scales = "free_y") +
  labs(
    title = "Observed vs. Predicted Final Ball Position (X)",
    x = "Launch Angle [°]",
    y = "Final Ball Position (X) [m]",
    color = "Water Speed [m/s]"
  ) +
  theme_minimal()





# predicted solution space (combined launch speed and launch angle)
ggplot(results_3, aes(x = final_ball_pos_x, y = final_ball_pos_z, color = launch_angle, size = launch_Speed)) +
  geom_jitter(alpha = 0.3, position = position_jitter(width = 0.05, height = 0.02)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(target_angle ~ water_speed_m_s) +
  geom_rect(data = dat,
            aes(xmin = target_position_x - (target_width / 2),
                xmax = target_position_x + (target_width / 2),
                ymin = 0.95, ymax = 1.2), 
            fill = NA, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  scale_size_continuous(name = "Launch Speed [m/s]") +
  scale_color_viridis(name = "Launch Angle [°]") +
  labs(
    title = "Solution Space: Final Ball Positions (Jittered)",
    x = "Final Ball Position (X) [m]",
    y = "Final Ball Position (Z) [m]"
  ) +
  theme_minimal()

# predicted solution space without launch speed as size aes
ggplot(results_3, aes(x = final_ball_pos_x, y = final_ball_pos_z, color = launch_angle)) +
  geom_jitter(alpha = 0.3, position = position_jitter(width = 0.05, height = 0.02)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(target_angle ~ water_speed_m_s) +
  geom_rect(data = dat,
            aes(xmin = target_position_x - (target_width / 2),
                xmax = target_position_x + (target_width / 2),
                ymin = 0.95, ymax = 1.2), 
            fill = NA, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  scale_color_viridis(name = "Launch Angle [°]") +
  labs(
    title = "Solution Space: Final Ball Positions (Jittered)",
    x = "Final Ball Position (X) [m]",
    y = "Final Ball Position (Z) [m]"
  ) +
  theme_minimal()


# Target position along the x-axis varies.
# -0.420 m (-20 degrees)
# -0.203 m (-10 degrees)
# 0 m (0 degrees)

# On the z-axis the target is consistently at 1.154 m

# raw data solution space
ggplot(dat, aes(x = final_ball_pos_x, y = final_ball_pos_z, color = launch_angle, shape = target_hit, size = launch_Speed)) +
  geom_point(alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_grid(target_angle ~ water_speed_m_s) +
  # rectangle for target width
  geom_rect(data = dat,
            aes(xmin = target_position_x - (target_width / 2),
                xmax = target_position_x + (target_width / 2),
                ymin = 0.95, ymax = 1.2), 
            fill = NA, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  scale_color_viridis(
    name = "Launch Angle [°]") +
  scale_shape_manual(
    name = "Target Hit",
    values = c("TRUE" = 16, "FALSE" = 4)  
  ) +
  scale_size_continuous(
    name = "Launch Speed [m/s]"
  ) +
  xlim(-2, 1) +
  ylim(0.95, 1.2) +
  labs(
    title = "Solution Space: Final Ball Positions",
    x = "Final Ball Position (X) [m]",
    y = "Final Ball Position (Z) [m]"
  ) +
  theme_minimal()
