# Setting the working directory
directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directory)

# Load the plot_court() function and other court dimensions
source("fiba_court_points.R")
source("zone_polygons.R")
source("spatial_shots_prep.R")

plot_court()
#ggsave("pictures/light_court.png", width = 8, height = 8)

plot_court(court_theme = court_themes$dark)

#ggsave("pictures/dark_court.png", width = 8, height = 8)

# Angle plot (degrees)
plot_court() +
  geom_text(
    data = tail(shots, n = 30),
    aes(x = loc_x, y = loc_y, label = round(theta_deg), col = shot_made_factor),
    alpha = 0.6
  )
#ggsave("pictures/angles.png", width = 8, height = 8)

# Distance plot (feet)
plot_court() +
  geom_text(
    data = tail(shots, n = 30),
    aes(x = loc_x, y = loc_y, label = round(dist_feet, digits = 1), col = shot_made_factor),
    alpha = 0.6
  )
#ggsave("pictures/distances.png", width = 8, height = 8)

############################ Shots
plot_court() +
  geom_sf(data = shots_sf, aes(colour = shot_made_factor), alpha = 0.2)
#ggsave("pictures/light_shots.png", width = 8, height = 8)

plot_court(court_theme = court_themes$dark) +
  geom_sf(data = shots_sf, aes(colour = shot_made_factor), alpha = 0.2)
#ggsave("pictures/dark_shots.png", width = 8, height = 8)

plot_court(court_theme = court_themes$dark) +
  geom_sf(data = shots_sf, aes(colour = shot_made_factor), alpha = 0.2) +
  facet_wrap(~player)
#ggsave("pictures/facet_player.png", width = 8, height = 8)

##################################
plot_court() +
  geom_sf(data = basic_polys, aes(fill = factor(description)), alpha = 0.2)
#ggsave("pictures/basic.png", width = 4, height = 4)

plot_court() +
  geom_sf(data = point_polys, aes(fill = factor(description)), alpha = 0.2)
#ggsave("pictures/point_value.png", width = 4, height = 4)

plot_court() +
  geom_sf(data = distance_polys, aes(fill = factor(description)), alpha = 0.2)
#ggsave("pictures/distance.png", width = 4, height = 4)

plot_court() +
  geom_sf(data = angle_polys, aes(fill = factor(description)), alpha = 0.2)
#ggsave("pictures/angle.png", width = 4, height = 4)

##################################

plot_court(court_theme = court_themes$dark) +
  geom_sf_text(data = shots_sf %>% slice_sample(n = 30),
               aes(color = shot_zone_range, label = shot_zone_range),
               alpha = 0.8)
#ggsave("pictures/shot_zone_range.png", width = 4, height = 4)

plot_court(court_theme = court_themes$dark) +
  geom_sf_text(data = shots_sf %>% slice_sample(n = 30),
               aes(color = shot_zone_area, label = shot_zone_area),
               alpha = 0.8)
#ggsave("pictures/shot_zone_area.png", width = 4, height = 4)
plot_court(court_theme = court_themes$dark) +
  geom_sf_text(data = shots_sf %>% slice_sample(n = 30),
               aes(color = shot_zone_basic, label = shot_zone_basic),
               alpha = 0.8)

#ggsave("pictures/shot_zone_basic.png", width = 4, height = 4)

plot_court(court_theme = court_themes$dark) +
  geom_sf_text(data = shots_sf %>% slice_sample(n = 30),
               aes(color = factor(shot_value), label = shot_value),
               alpha = 0.8)
#ggsave("pictures/shot_value.png", width = 4, height = 4)

################################## Group Stats

zone_stats_team <- shots_sf %>%
  group_by(shot_zone_range, shot_zone_area) %>%
  summarize(
    fgm = sum(shot_made_numeric),
    fga = n(),
    pct = mean(shot_made_numeric),
    points_per_shot = mean(shot_value * shot_made_numeric),
    .groups = "drop"
    ) %>%
  arrange(desc(fga), desc(fgm))

# Create polygons for each sub-zone intersection
test <- st_intersection(
  x = angle_polys,
  y = distance_polys
)

trial <- st_join(
  x = test,
  y = zone_stats_team
) %>%
  transmute(
    shot_zone_area = shot_zone_area.x,
    shot_zone_range = shot_zone_range.x,
    fgm,
    fga,
    pct,
    points_per_shot, geom
  )
trial

plot(trial)

library(scales)
plot_court() +
  geom_sf(data = trial, aes(fill = pct), alpha = 0.4) +
  geom_sf_label(data = trial, aes(label = percent(pct%>%round(digits = 2)))) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )

plot_court() +
  geom_sf(data = trial, aes(fill = fga), alpha = 0.4) +
  geom_sf_label(data = trial, aes(label = fga)) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )


plot_court() +
  geom_sf(data = trial, aes(fill = points_per_shot), alpha = 0.4) +
  geom_sf_label(data = trial, aes(label = points_per_shot%>%round(digits = 2))) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )
##############################\
basic_zone_stats <- shots_sf %>%
  group_by(shot_zone_basic) %>%
  summarize(
    fgm = sum(shot_made_numeric),
    fga = n(),
    pct = mean(shot_made_numeric),
    points_per_shot = mean(shot_value * shot_made_numeric),
    .groups = "drop"
  ) %>%
  arrange(desc(fga), desc(fgm))

trial_run <- st_join(
  x = basic_polys,
  y = basic_zone_stats
) %>%
  transmute(
    shot_zone_basic = shot_zone_basic.x,
    fgm,
    fga,
    pct,
    points_per_shot, geom
  )
trial_run

plot(trial_run)

plot_court() +
  geom_sf(data = trial_run, aes(fill = fga), alpha = 0.4) +
  geom_sf_label(data = trial_run, aes(label = fga)) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )

plot_court() +
  geom_sf(data = trial_run, aes(fill = pct), alpha = 0.4) +
  geom_sf_label(data = trial_run, aes(label = percent(pct%>%round(digits = 2)))) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )

plot_court() +
  geom_sf(data = trial_run, aes(fill = points_per_shot), alpha = 0.4) +
  geom_sf_label(data = trial_run, aes(label = points_per_shot%>%round(digits = 2))) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )






#################################

plot(zone_stats_team)

plot_court() +
  geom_sf(data = trial, aes(fill = fga), alpha = 0.4) +
  geom_sf_label(data = trial, aes(label = fga)) +
  scale_fill_distiller(
    palette = "RdBu",
    na.value = "white"
  )plot(st_geometry(zone_stats_team)[1])

plot(as.data.frame(st_coordinates(zone_stats_team)) %>% filter(L1 == 1))

plot_court() +
  geom_point(
    data = as.data.frame(st_coordinates(zone_stats_team)) %>% filter(L1 == 1),
    aes(x = X, y = Y)
    )







# Stop Here






















