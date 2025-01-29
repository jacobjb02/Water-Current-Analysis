library(readr)
library(ggplot2)
library(dplyr)


dat <- read_csv("Example Data/trial_results_120_current.csv")

dat$ball_error_x <- dat$final_ball_pos_x - dat$target_position_x

plot(x = dat$trial_num, y = dat$ball_error_x)

dat$abs_ball_error_x <- abs(dat$ball_error_x)


plt1 <- ggplot(dat, aes(x = trial_num, y = abs_ball_error_x, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +  # Use geom_col() for bar plot
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
  )
plt1
ggsave("abs_ball_error_x.png", plot = plt1)

plt2 <- ggplot(dat, aes(x = trial_num, y = ball_error_x, fill = as.factor(target_hit))) +
  geom_col(width = 0.8, alpha = 0.8) +  # Use geom_col() for bar plot
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 37, linetype = 'dashed') +
  geom_vline(xintercept = 210, linetype = 'dashed') +
  labs(
    x = "Trial Number",
    y = "Ball Error (X)",
    fill = "Target Hit",
    title = "Ball Error (X) Across Trials",
    subtitle = "Bar height represents error magnitude per trial"
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )
plt2
ggsave("ball_error_x.png", plot = plt2)




# filter each phase of experiment
dat_baseline <- filter(dat, trial_num < 37)
dat_exposure <- filter(dat, trial_num > 36 & trial_num < 211)
dat_washout <- filter(dat, trial_num > 210)


mod_base <- lm(abs_ball_error_x ~ trial_num, data = dat_baseline)
summary(mod_base)

mod_exp <- lm(abs_ball_error_x ~ trial_num, data = dat_exposure)
summary(mod_exp)

mod_wash <- lm(abs_ball_error_x ~ trial_num, data = dat_washout)
summary(mod_wash)
