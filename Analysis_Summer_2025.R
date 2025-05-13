# libraries
library(readr)
library(dplyr)
library(ggplot2)

# load participant data
dat_1 <- read_csv("Data_Summer_25/trial_results_jacob_b.csv")


# DV - distance to center
dat_1 <- dat_1 %>%
  #select(closest_center_ball_pos_x, closest_center_ball_pos_z, target_position_x, target_position_z) %>%
  mutate(ball_dist_to_center_cm = sqrt((closest_center_ball_pos_x - target_position_x)^2 
                                    + (closest_center_ball_pos_z - target_position_z)^2)*100)

# Create dummy variable for water current on/off
dat_1$water_speed_binary <- ifelse(dat_1$water_speed_m_s == -2, 1, 0)
dat_1$water_speed_binary <- factor(dat_1$water_speed_binary)



dat_1 <- dat_1 %>%
  mutate(target_angle = case_when(
    target_position_x == -0.6 ~ -23.2,
    target_position_x == -0.3 ~ -12.2,
    target_position_x ==  0.3 ~  12.2,
    target_position_x ==  0.6 ~  23.2,
    TRUE ~ NA_real_  # fallback in case of other values
  ))

dat_1 <- dat_1 %>%
  group_by(target_angle) %>%
  mutate(target_angle_mt = abs(target_angle) + 90) %>%
  ungroup()

dat_1 <- dat_1 %>%
  group_by(ppid,trial_num) %>%
  mutate(target_angle_dev = launch_angle - target_angle_mt) %>%
  ungroup()


transitions <- function(df) {
  df %>%
    arrange(trial_num) %>%
    mutate(water_speed_binary_prev = lag(water_speed_binary)) %>%
    filter(water_speed_binary != water_speed_binary_prev) %>%
    select(trial_num, water_speed_binary) %>%
    rename(xintercept = trial_num)
}


transition_pts <- transitions(dat_1)
transition_pts

# figures


# min dist to center
plot_trial_min_dist_ppid <- function(df) {
  
  # Plot
  p <- ggplot(df, aes(
    x = trial_num,
    y = ball_dist_to_center_cm,
    group = ppid
  )) +
    geom_point(aes(color = factor(target_position_x)), alpha = 0.8, size = 1) +
    facet_wrap(~ppid, ncol = 2) +
    geom_vline(
      data = transition_pts,
      aes(xintercept = xintercept, color = factor(water_speed_binary)),
      linetype = "dashed",
      alpha = 0.6, size = 1
    ) +
    scale_color_manual(
      values = c(
        "-0.6" = "red", "-0.3" = "orange", "0.3" = "blue", "0.6" = "purple",
        "0" = "black", "1" = "dodgerblue"  # for water_speed_binary
      ),
      name = "Target X / Water Speed"
    )
  
  
  ggsave("ppid_min_dist_plot.png", p, width = 12, height = 10, dpi = 300, bg = 'white')
  
  return(p)
}
# plot figure
plot_trial_min_dist_ppid(dat_1)


# plot x and z pos

# plot final ball pos x,z
plot_closest_xz_pos <- function(df) {
  
  # Create label for color mapping
  df <- df %>%
    mutate(
      target_label = factor(
        paste0("x=", target_position_x, ", z=", target_position_z),
        levels = rev(unique(paste0("x=", target_position_x, ", z=", target_position_z)))
      )
    )
  
  # Filter to hits
  #df_hits <- df %>% filter(target_hit == TRUE)
  
  # Plot
  p <- ggplot(df, aes(x = closest_center_ball_pos_x, y = closest_center_ball_pos_z, color = factor(target_position_x))) +
    geom_point(alpha = 0.5, size = 1.2) +
    
    # Add one point per target center 
    geom_point(data = df,
               aes(x = target_position_x, y = target_position_z),
               color = "black", shape = 10, size = 1.5, inherit.aes = FALSE) +
    
    facet_wrap(~target_position_x, ncol = 2) +
    coord_fixed() +
    labs(
      x = "Final X Position for Ball (m)",
      y = "Final Z Position for Ball (m)",
      color = "Target Location"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_brewer(palette = "Dark2")
  
  # Save
  ggsave("closest_ball_pos_x_z.png", p, width = 12, height = 6, dpi = 300, bg = 'white')
  
  p
}

plot_closest_xz_pos(dat_1)



p <- ggplot(dat_1, aes(x = target_angle_dev, y = launch_Speed, color = ball_dist_to_center_cm)) +
  scale_color_viridis_c(option = "plasma", limits = c(0.9, 50), oob = scales::squish) +
  facet_wrap(~target_position_x, ncol = 2) +
  labs(
    title = "Exploration of Motor Solution Space",
    x = "Launch Deviation (degrees)",
    y = "Launch Speed (m/s)",
    color = "Min Distance (m)"
  ) +
  theme_minimal(base_size = 18)
p

ggplot(dat_1, aes(x = target_angle_dev, y = launch_Speed, color = ball_dist_to_center_cm)) +
  geom_point() +  
  facet_wrap(~target_position_x, ncol = 2) +
  theme_minimal(base_size = 18)

