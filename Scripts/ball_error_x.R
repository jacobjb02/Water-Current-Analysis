
# libraries
library(readr)
library(ggplot2)
library(viridis)
library(dplyr)

# variables:
text_size = 12

# load data
dat_1 <- read.csv('Pilot Data/J/trial_results_longer_paradigm.csv')

#dat_2 <- read.csv('Pilot Data/J/trial_results_40_current_0_target.csv')
#dat_2$ppid[dat_2$ppid == "123"] <- "124"
#dat_combined <- rbind(dat_1, dat_2)

dat_combined <- dat_1
  
# load save path
save_path <- "Figures/Pilot"

# create ball_error_x variable
dat_combined$ball_error_x <- dat_combined$final_ball_pos_x - dat_combined$target_position_x

# change launch angle to a numerical variable
dat_combined$launch_angle <- as.numeric(dat_combined$launch_angle)
str(dat_combined$launch_angle)

# rename water_speed_m/s to water_speed_m_s
colnames(dat_combined)[which(names(dat_combined) == "water_speed_m.s")] <- "water_speed_m_s"
dat_combined$water_speed_m_s <- as.factor(dat_combined$water_speed_m_s)
dat_combined$target_angle <- as.factor(dat_combined$target_angle)

# create abs_ball_error_x
dat_combined$abs_ball_error_x <- abs(dat_combined$ball_error_x)







# graph showing absolute ball_error_x across all trials
plt1 <- ggplot(dat_combined, aes(x = trial_num, y = abs_ball_error_x)) +
  
  # Add points with color based on target_hit
  geom_point(aes(color = as.factor(target_hit)), alpha = 0.8) +  
  
  theme_minimal(base_size = 14) +
  
  # Add vertical lines at specific trials, colored by water_speed_m_s
  geom_vline(data = dat_combined %>% filter(trial_num %in% c(61, 67, 307, 313, 325, 331, 343, 349, 361, 367, 379)), 
             aes(xintercept = trial_num, color = as.factor(water_speed_m_s), linetype = as.factor(type)), 
             size = 1.5, alpha = 0.5) +
  
  labs(
    x = "Trial Number",
    y = "Ball Error (X)",
    fill = "Target Hit",
    color = "Water Speed (m/s)",  # Added legend label for color mapping
    title = "Ball Error (X) Across Trials - Faceted by Target Angle",
    subtitle = "Data points with curves highlighting key transitions"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~target_angle)

plt1
ggsave("abs_ball_error_x.png", path = save_path, plot = plt1)


# Bar graph showing ball_error_x across all trials (faceted by target angle)
plt2 <- ggplot(dat_combined, aes(x = trial_num, y = ball_error_x, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +  
  theme_minimal(base_size = 14) +
  
  # Add vertical lines at specific trials, colored by water_speed_m_s
  geom_vline(data = dat_combined %>% filter(trial_num %in% c(61, 67, 307, 313, 325, 331, 343, 349, 361, 367, 379)), 
             aes(xintercept = trial_num, color = as.factor(water_speed_m_s), linetype = as.factor(type)), 
             size = 1.5, alpha = 0.5) +
  labs(
    x = "Trial Number",
    y = "Ball Error (X)",
    fill = "Target Hit",
    color = "Water Speed (m/s)",  # Added legend label for color mapping
    title = "Ball Error (X) Across Trials - Faceted by Target Angle",
    subtitle = "Bar height represents error magnitude per trial"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~target_angle)



plt2
ggsave("ball_error_x.png", path = save_path, plot = plt2)

sum(dat$target_hit == 'TRUE') / sum(dat$trial_num) * 100

# Boxplots showing ball_error_x within targets (all trials)
plt3 <- ggplot(dat, aes(x = as.factor(target_angle), y = ball_error_x, color = as.factor(target_angle))) +
  geom_boxplot(width = 0.8, alpha = 0.8) +  
  theme_minimal(base_size = 14) +
  labs(
    x = "",
    y = "Ball Error (X)",
    color = "Target Angle",
    title = "Ball Error (X) Across Target Angle",
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) 
plt3
ggsave("ball_error_x_boxplot.png", path = save_path,  plot = plt3)




plt4 <- ggplot(dat, aes(x = trial_num, y = launch_Speed, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +  
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 25, linetype = 'dashed') + # dashed line = invisible trials start
  geom_vline(xintercept = 31, linetype = 'solid') + # solid line = water current trials start
  geom_vline(xintercept = 151, linetype = 'dashed') + # dashed line = invisible trials start
  geom_vline(xintercept = 163, linetype = 'solid') + # solid line = water current trials start
  geom_vline(xintercept = 175, linetype = 'dashed') + # dashed line = invisible trials start
  labs(
    x = "Trial Number",
    y = "Launch Speed [m/s]",
    fill = "Target Hit",
    title = "Launch Speed [m/s] Across Trials - Faceted by Target Angle",
    subtitle = "Bar height represents launch speed per trial"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~target_angle)  
plt4

ggsave("launch_speed_trials.png", path = save_path, plot = plt4)



plt5 <- ggplot(dat, aes(x = trial_num, y = launch_angle, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +  
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 25, linetype = 'dashed') + # dashed line = invisible trials start
  geom_vline(xintercept = 31, linetype = 'solid') + # solid line = water current trials start
  geom_vline(xintercept = 151, linetype = 'dashed') + # dashed line = invisible trials start
  geom_vline(xintercept = 163, linetype = 'solid') + # solid line = water current trials start
  geom_vline(xintercept = 175, linetype = 'dashed') + # dashed line = invisible trials start
  labs(
    x = "Trial Number",
    y = "Launch Angle (°)",
    fill = "Target Hit",
    title = "Launch Angle Across Trials - Faceted by Target Angle",
    subtitle = "Bar height represents launch speed per trial"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~target_angle)  
plt5

ggsave("launch_angle_trials.png", path = save_path, plot = plt5)


plt6 <- dat_combined %>%
  ggplot(aes(
    x = launch_angle,
    y = launch_Speed,
    size = -water_speed_m.s,
    color = ball_error_x
  )) +
  geom_point(alpha = 0.7, stroke = 0) +  
  theme_classic() +
  scale_size_continuous(range = c(2, 6)) +  
  scale_color_viridis_c() +  
  labs(
    x = "Launch Angle [°]",
    y = "Throw Speed [m/s]",
    size = "Water Speed [m/s]",  
    color = "Ball Error [x]"
  ) +
  theme(
    legend.position = "right",
    text = element_text(size = 14)
  )

plt6



# needs to be updated for new paradigms.......
# filter each phase of experiment
dat_baseline <- filter(dat, trial_num < 37)
dat_exposure <- filter(dat, trial_num > 36 & trial_num < 211)
dat_washout <- filter(dat, trial_num > 210)


# linear models to test if abs ball error decreases across trials (for rudimentary evidence of learning curves?)
mod_base <- lm(abs_ball_error_x ~ trial_num, data = dat_baseline)
summary(mod_base)

mod_exp <- lm(abs_ball_error_x ~ trial_num, data = dat_exposure)
summary(mod_exp)

mod_wash <- lm(abs_ball_error_x ~ trial_num, data = dat_washout)
summary(mod_wash)


# summary stats for experimental phase data
dat_baseline %>%
  group_by(target_angle) %>%
  summarise(mean_ball_error_x = mean(ball_error_x, na.rm = TRUE), var_ball_error_x = var(ball_error_x, na.rm = TRUE))

dat_exposure %>%
  group_by(target_angle) %>%
  summarise(mean_ball_error_x = mean(ball_error_x, na.rm = TRUE), var_ball_error_x = var(ball_error_x, na.rm = TRUE))

dat_washout %>%
  group_by(target_angle) %>%
  summarise(mean_ball_error_x = mean(ball_error_x, na.rm = TRUE), var_ball_error_x = var(ball_error_x, na.rm = TRUE))



# linear models illustrating the effect of launch speed and angle on ball_error_x
mod_speed <- lm(ball_error_x ~ launch_Speed, data = dat)
summary(mod_speed)

mod_angle <- lm(ball_error_x ~ launch_angle, data = dat)
summary(mod_angle)


mod_interaction <- lm(ball_error_x ~ launch_Speed + I(launch_Speed^2) * launch_angle, data = dat)
summary(mod_interaction)

mod_quad <- lm(abs_ball_error_x ~ launch_Speed + I(launch_Speed^2) * launch_angle + I(launch_angle^2), data = dat)
summary(mod_quad)

