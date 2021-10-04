# # Setting the working directory
# directory <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(directory)
# 
# source("fiba_court_points.R")
# source("zone_polygons.R")

# Load shot data
shots <- readRDS(file = "data/shots.rds")

# Add a few variables and clean others
shots <- shots %>%
  tibble() %>%
  mutate(
    # convert players to a factor
    ####################################### paste("Player", 1:18)
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

# convert shots to an sf object
shots_sf <- st_as_sf(shots, coords = c("loc_x", "loc_y"))

# shot_zone_range
shots_sf <- st_join(
  x = shots_sf,
  y = distance_polys
) %>%
  # mutate(shot_zone_range = description) %>%
  # select(-description) %>%
  st_join(
    y = angle_polys
  ) %>%
  # shot_zone_area
  # mutate(shot_zone_area = description) %>%
  # select(-description) %>%
  st_join(
    y = basic_polys
  ) %>%
  # shot_zone_basic
  # mutate(shot_zone_basic = description) %>%
  # select(-description) %>%
  st_join(
    y = point_polys
  ) %>%
  # shot_value
  # mutate(area_value = description) %>%
  # select(-description) %>%
  mutate(
    shot_value = ifelse(area_value == "Two-Point Area", 2, 3)
  ) %>%
  # Reorder and only keep relevant variables
  select(date, player, shot_made_numeric, shot_made_factor,
         dist_feet, dist_meters, theta_deg, theta_rad, shot_value,
         shot_zone_range, shot_zone_area, shot_zone_basic, area_value,
         geometry)

# # Save cleaned data
# saveRDS(shots_sf, file = "data/shots_sf.rds")

# Remove objects from working directory
rm(
  half_court_int, half_court_ext, half_court,
  key, key_circle, key_circle_ext, key_circle_int, key_int, key_ext, key_center,
  hoop, hoop_ext, hoop_int, hoop_center,
  backboard, backboard_points,
  neck, neck_points,
  three_center, three_ext, three_int, three_point_line, three_ext_flip,
  half_circle, half_circle_int, half_circle_ext, half_center,
  restricted_area, ra_center, ra_ext, ra_int, ra_int_flip,
  n, b1, b2, x1, y2
)

rm(
  line_thick, height, key_height, key_width, key_radius, backboard_width,
  backboard_thick, backboard_offset, hoop_radius, rim_thick,
  neck_length, three_point_radius, three_point_side_offset,
  three_point_side_height, restricted_area_radius, width,
  theta_1_deg, theta_1_rad, theta_2_deg, theta_2_rad,
  hoop_center_y
)

rm(
  above_break_three, center, eight_sixteen_ft, left_center, left_corner_three,
  left_side, mid_range, paint, right_center, right_corner_three, right_side,
  sixteen_twentyfour_ft, three_point_area, twentyfour_plus_ft, two_point_area,
  zero_eight_ft
)
