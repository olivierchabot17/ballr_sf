# Setting the working directory
directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directory)

source("fiba_court_points.R")

plot_court()

plot_court(court_theme = court_themes$dark)

# ggsave("court.png", width = 8, height = 8)

# Load shot data
shots <- readRDS(file = "shots.rds")

plot_court(court_theme = court_themes$light) +
  geom_point(
    data = shots %>% 
      filter(free_throw == 0),
    aes(x = loc_x, y = loc_y, color = shot_made_factor),
    alpha = 0.4
  ) +
  theme(legend.title = element_blank())


plot_court(court_theme = court_themes$light) +
  geom_point(
    data = shots %>% 
      filter(
        #player %in% c("Player 1", "Player 2", "Player 3", "Player 4"),
        free_throw == 0
      ),
    aes(x = loc_x, y = loc_y, color = shot_made_factor),
    alpha = 0.4
  ) +
  theme(legend.title = element_blank()) +
  facet_wrap(~player)
