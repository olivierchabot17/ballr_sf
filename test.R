# Setting the working directory
directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directory)

source("fiba_court_points.R")
source("spatial_shots_prep.R")

plot_court()

plot_court(court_theme = court_themes$dark)

# ggsave("court.png", width = 8, height = 8)

# Angle plot (degrees)
plot_court() +
  geom_text(
    data = tail(shots, n = 30),
    aes(x = loc_x, y = loc_y, label = round(theta_deg), col = shot_made_factor),
    alpha = 0.6
  )

# Distance plot (feet)
plot_court() +
  geom_text(
    data = tail(shots, n = 30),
    aes(x = loc_x, y = loc_y, label = round(dist_feet, digits = 1), col = shot_made_factor),
    alpha = 0.6
  )

# convert shots to an sf object
shots_sf <- st_as_sf(shots, coords = c("loc_x", "loc_y"))

plot_court() +
  geom_sf(data = shots_sf, aes(colour = shot_made_factor), alpha = 0.2)

plot_court(court_theme = court_themes$dark) +
  geom_sf(data = shots_sf, aes(colour = shot_made_factor), alpha = 0.2)

plot_court(court_theme = court_themes$dark) +
  geom_sf(data = shots_sf, aes(colour = shot_made_factor), alpha = 0.2) +
  facet_wrap(~player)

