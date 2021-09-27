# Setting the working directory
directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directory)

# Load the plot_court() function and other court dimensions

# Load shot data
shots <- readRDS(file = "shots.rds")

# Add a few variables and clean others
shots <- shots %>%
  tibble() %>%
  mutate(
    # convert players to a factor
    player = factor(player, levels = c(
      "Player 1", "Player 2", "Player 3", "Player 4", "Player 5", "Player 6",
      "Player 7", "Player 8", "Player 9", "Player 10", "Player 11", "Player 12",
      "Player 13", "Player 14", "Player 15", "Player 16", "Player 17", "Player 18"
      )),
    shot_made_factor = recode_factor(
      factor(shot_made_numeric),
      "0" = "Miss", "1" = "Make"
      ),
    # Edit the distance variable by adjusting for the center of the hoop at (0, 1.575)
    dist_meters = sqrt((loc_x-width/2)^2 + (loc_y-hoop_center_y)^2),
    dist_feet = dist_meters * 3.28084,
    # Calculate angle of shot location from the center of the rim
    # zero degrees would be directly centered
    # 90 degrees would be shooting from the corner threes
    theta_rad = case_when(
      # Quadrant 1: Shots from left side higher than the rim
      loc_x > width/2 & loc_y > hoop_center_y ~ atan((loc_x-width/2)/(loc_y-hoop_center_y)),
      # Quadrant 2: Shots from right side higher than the rim
      loc_x < width/2 & loc_y > hoop_center_y ~ atan((width/2-loc_x)/(loc_y-hoop_center_y)),
      # Quadrant 3: Shots from right side lower than the rim
      loc_x < width/2 & loc_y < hoop_center_y ~ atan((hoop_center_y-loc_y)/(width/2-loc_x))+(pi/2),
      # Quadrant 4: Shots from left side lower than the rim
      loc_x > width/2 & loc_y < hoop_center_y ~ atan((hoop_center_y-loc_y)/(loc_x-width/2))+(pi/2),
      # Special Cases
      loc_x == width/2  & loc_y >= hoop_center_y ~ 0, # Directly centered front
      loc_x == width/2  & loc_y < hoop_center_y ~ pi, # Directly centered back
      loc_y == hoop_center_y ~ pi/2, # Directly parallel to hoop center
    ),
    theta_rad = ifelse(loc_x > width/2, -theta_rad, theta_rad),
    theta_deg = theta_rad * (180/pi)
  )

