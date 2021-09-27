# Setting the working directory
directory <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(directory)

source("fiba_court_points.R")

plot_court()

plot_court(court_theme = court_themes$dark)

# ggsave("court.png", width = 8, height = 8)
