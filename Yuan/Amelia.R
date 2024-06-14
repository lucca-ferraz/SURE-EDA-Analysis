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

# Question 1: Age by match duration

# Visualizing
library(tidyverse)



wta_grand_slam_matches %>%
  ggplot(aes(x = winner_age, y = minutes)) +
  geom_smooth(aes(color = "Winner Age"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = loser_age, y = minutes, color = "Loser Age"), method = "lm", se = FALSE) +
  labs(title = "Winner and Loser Age vs Match Duration",
       x = "Age (years)",
       y = "Match Duration (minutes)",
       color = "Player Type") +
  theme_minimal() +
  scale_color_manual(values = c("Winner Age" = "midnightblue", "Loser Age" = "orange")) +
  coord_flip() +
  theme(plot.title = element_text(size = 15, face = "bold")) 

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


wta_prepared <- wta_grand_slam_matches %>%
  select(winner_age, w_ace) %>%
  na.omit()

# caculate
distance_matrix <- dist(wta_prepared, method = "euclidean")

# clu
hc <- hclust(distance_matrix, method = "complete")

# den
plot(hc, main = "Hierarchical Clustering of WTA Grand Slam Matches by Age and Aces")

# hight
clusters <- cutree(hc, k = 4) 

# add to original dataset
wta_prepared$cluster <- as.factor(clusters)

# point plot
ggplot(wta_prepared, aes(x = winner_age, y = w_ace, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Cluster Analysis of Winner's Age vs. Number of Aces",
       x = "Winner's Age",
       y = "Number of Aces",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")

# Conclusion: This one does not have a very clear trend.

##################################################

library(dplyr)
library(ggplot2)
library(cluster)


wta_prepared_2 <- wta_grand_slam_matches %>%
  select(winner_seed, w_ace) %>%
  na.omit()

# cleaning
wta_prepared_2$winner_seed <- gsub("[^0-9]", "", wta_prepared_2$winner_seed) # 移除非数字字符
wta_prepared_2$winner_seed <- as.numeric(wta_prepared_2$winner_seed) # 转换为数值型

# caculate
distance_matrix <- dist(wta_prepared_2, method = "euclidean")

# clu
hc <- hclust(distance_matrix, method = "complete")

# den
plot(hc, main = "Hierarchical Clustering of WTA Grand Slam Matches by Winner Seed and Winner Aces")

# hight
clusters <- cutree(hc, k = 4) 

# add to original dataset
wta_prepared_2$cluster <- as.factor(clusters)

max_aces_per_seed <- wta_prepared_2 %>%
  group_by(winner_seed) %>%
  summarise(max_ace = max(w_ace, na.rm = TRUE))
# point plot
ggplot(wta_prepared_2, aes(x = factor(winner_seed, levels = sort(unique(winner_seed))), y = w_ace, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Cluster Analysis of Winner's Seed vs. Number of Aces",
       x = "Winner's Seed",
       y = "Number of Aces",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")

# Trend: The graph shows that winners with lower seed (e.g., 21-30) 
# tend to have more concentrated ace scores, while winners with higher seed 
#  (e.g. 1-15) exhibit greater variability in their ace scores."

#################################

library(dplyr)
library(ggplot2)
library(cluster)


wta_prepared_3 <- wta_grand_slam_matches %>%
  select(winner_seed, winner_age) %>%
  na.omit()

wta_prepared_3$winner_seed <- gsub("[^0-9]", "", wta_prepared_3$winner_seed) # 移除非数字字符
wta_prepared_3$winner_seed <- as.numeric(wta_prepared_3$winner_seed) # 转换为数值型

# caculate
distance_matrix <- dist(wta_prepared_3, method = "euclidean")

# clu
hc <- hclust(distance_matrix, method = "complete")

# den
plot(hc, main = "Hierarchical Clustering of WTA Grand Slam Matches by Winner Seed and Winner Age")

# hight
clusters <- cutree(hc, k = 4) 

# add to original dataset
wta_prepared_3$cluster <- as.factor(clusters)

max_winner_age_per_seed <- wta_prepared_3 %>%
  group_by(winner_seed) %>%
  summarise(winner_age = max(winner_age, na.rm = TRUE))
# point plot
ggplot(wta_prepared_3, aes(x = factor(winner_seed, levels = sort(unique(winner_seed))), y = winner_age, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Cluster Analysis of Winner's Seed vs. Winner Age",
       x = "Winner's Seed",
       y = "Winner Age",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")



library(dplyr)
library(ggplot2)
library(cluster)


wta_prepared_4 <- wta_grand_slam_matches %>%
  select(round, minutes) %>%
  na.omit()

wta_prepared_4$round <- gsub("[^0-9]", "", wta_prepared_4$round) 
wta_prepared_4$round <- as.numeric(wta_prepared_4$round) 

# caculate
distance_matrix <- dist(wta_prepared_4, method = "euclidean")

# clu
hc <- hclust(distance_matrix, method = "complete")

# den
plot(hc, main = "Hierarchical Clustering of WTA Grand Slam Matches by Round and Minutes")

# hight
clusters <- cutree(hc, k = 4) 

# add to original dataset
wta_prepared_4$cluster <- as.factor(clusters)

max_winner_age_per_seed <- wta_prepared_3 %>%
  group_by(round) %>%
  summarise(minutes = max(minutes, na.rm = TRUE))
# point plot
ggplot(wta_prepared_4, aes(x = factor(round, levels = sort(unique(round))), y = minutes, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Cluster Analysis of Round vs. Minutes",
       x = "Round",
       y = "Minutes",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_flip()























