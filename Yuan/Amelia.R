# Amelia Yixin Yuan
library(tidyverse)
wta_grand_slam_matches <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/wta_grand_slam_matches.csv")

# Exploring the dataset
colnames(wta_grand_slam_matches)

summary(wta_grand_slam_matches)

unique_tourney_names <- wta_grand_slam_matches |>
  pull(tourney_name) |>
  unique()  
unique_tourney_names
# [1] "ROLAND GARROS"   "WIMBLEDON"       "US OPEN"         "AUSTRALIAN OPEN"

unique_surface <- wta_grand_slam_matches |>
  pull(surface) |>
  unique()  
unique_surface
# [1] "Clay"  "Grass" "Hard" 

# Question: Is there a relationship between the height of the players and 
# their success in winning matches at Grand Slams?

# Visualizing
library(tidyverse)

wta_grand_slam_matches %>%
  ggplot(aes(x = winner_age, y = minutes)) +
  geom_smooth(aes(color = "Winner Age"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = loser_age, y = minutes, color = "Loser Age"), method = "lm", se = FALSE) +
  labs(title = "Influence of Age on Match Duration",
       x = "Age (years)",
       y = "Match Duration (minutes)",
       color = "Player Type") +
  theme_minimal() +
  coord_flip()

# Findings
# Two lines: Loser Age vs Match Duration and Winner Age vs Match Duration
# They intersects at the age of 26 and 97 minutes.

# In the shorter games, i.e. within 97 minutes, the winners tend to be the older 
# However, in the longer games, the winner age tends to be smaller than the loser age

# Insights
# In the shorter games, the older players are more experienced and more strategic, 
# so they may play better than the young people.
# But as the match gets longer, the older players may have felt tired already,
# while the younger players have more endurance so the winner age is smaller.








