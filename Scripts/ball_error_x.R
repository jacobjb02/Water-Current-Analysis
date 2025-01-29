
# libraries
library(readr)
library(ggplot2)
library(dplyr)

# load data
dat <- read_csv("Example Data/trial_results_120_current.csv")

# load save path
save_path <- "Figures/Pilot"

# create ball_error_x variable
dat$ball_error_x <- dat$final_ball_pos_x - dat$target_position_x

# basic plot
plot(x = dat$trial_num, y = dat$ball_error_x)

# create abs_ball_error_x
dat$abs_ball_error_x <- abs(dat$ball_error_x)

# Bar graph showing absolute ball_error_x across all trials
plt1 <- ggplot(dat, aes(x = trial_num, y = abs_ball_error_x, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 37, linetype = 'dashed') +
  geom_vline(xintercept = 210, linetype = 'dashed') +
  labs(
    x = "Trial Number",
    y = "Absolute Ball Error (X)",
    fill = "Target Hit",
    title = "Ball Error (X) Across Trials",
    subtitle = "Bar height represents error magnitude per trial"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) + facet_wrap(~target_angle)
plt1
ggsave("abs_ball_error_x.png", path = save_path, plot = plt1)


# Bar graph showing ball_error_x across all trials (faceted by target angle)
plt2 <- ggplot(dat, aes(x = trial_num, y = ball_error_x, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +  
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 37, linetype = 'dashed') +
  geom_vline(xintercept = 210, linetype = 'dashed') +
  labs(
    x = "Trial Number",
    y = "Ball Error (X)",
    fill = "Target Hit",
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


