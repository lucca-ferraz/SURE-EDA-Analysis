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

# Question 1: Is there a relationship between the height of the players and 
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

average_duration <- wta_grand_slam_matches %>%
  summarise(average_duration = mean(minutes, na.rm = TRUE))

# ave
print(average_duration)
# 97.3


# Question 2: Does the player that ranks higher always win the one raking lower?
# for example, we audience usually expect the top 10 player win against 
# a top 100 player.

# Visualization:
ggplot(wta_grand_slam_matches, aes(x = winner_rank, y = loser_rank)) +
  geom_point(alpha = 0.4, color = "midnightblue", size = 0.5) +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "orange", size = 1.5) +  # 红色虚线更明显
  labs(
    title = "Comparing the Winner Rank and the Loser Rank",
    x = "Winner Rank",
    y = "Loser Rank",
    caption = "The orange line means that Winner Rank and the Loser Rank are the same."
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1) +  
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "none"  
  )

# Expanation:
# The orange line means that Winner Rank and the Loser Rank are the same.
# Our common expectation is that the player with higher ranking will win.
# The ranking is that the lower the number is, the higher the rank is.
# In the plot, 
# If we see a dot is under the orange line, it means the winner rank is higher than the loser rank.
# Thus, we expect that most of the dots should be under the orange line.
# But
# they are not.

# Exact Calculation
wta_grand_slam_matches <- wta_grand_slam_matches |>
  mutate(winner_rank_higher = winner_rank < loser_rank)

total_matches <- nrow(wta_grand_slam_matches)
total_matches
# We have 2921 games in total.
winner_rank_higher_count <- sum(wta_grand_slam_matches$winner_rank_higher, na.rm = TRUE)
winner_rank_higher_count
# Out of 1967, there are 1967 games that the winner ranks higher.

winner_rank_higher_ratio <- winner_rank_higher_count / total_matches

# Print the results
cat("The ratio of the winner ranks higher:", winner_rank_higher_ratio * 100, "%\n")
# The ratio of the winner ranks higher: 67.33995 %

library(dplyr)
library(ggplot2)
library(cluster)


wta_prepared <- wta_grand_slam_matches |>
  select(winner_age) |>
  na.omit() |> 
  scale()  


hc <- hclust(dist(wta_prepared), method = "ward.D2")


plot(hc, main = "Hierarchical Clustering of WTA Grand Slam Matches")


clusters <- cutree(hc, k = 4)
wta_grand_slam_matches$cluster <- as.factor(clusters)


ggplot(wta_grand_slam_matches, aes(x = winner_age, y = w_1stWon, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Cluster Analysis of WTA Grand Slam Matches by Age")

























