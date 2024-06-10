#hello world
library(tidyverse)
wta_grand_slam_matches <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/wta_grand_slam_matches.csv")

colnames(wta_grand_slam_matches)
unique(wta_grand_slam_matches$tourney_name)

#plot: minutes per game sorted by tournament
wta_grand_slam_matches |> 
  group_by(tourney_name) |> 
  ggplot(aes(x = minutes, color = tourney_name)) +
  geom_bar() + 
  facet_wrap(vars(tourney_name))

#plot: minutes per game sorted by surface
wta_grand_slam_matches |> 
  group_by(surface) |> 
  ggplot(aes(x = minutes, color = surface)) +
  geom_bar() + 
  facet_wrap(vars(surface))

#plot: comparing winners and losers 1st serves won
wta_grand_slam_matches |> 
  ggplot(aes(x = w_1stWon, y = l_1stWon)) +
  geom_point() + 
  geom_smooth()
  
#plot: comparing winners and losers 2nd serves won
wta_grand_slam_matches |> 
  ggplot(aes(x = w_2ndWon, y = l_2ndWon)) +
  geom_point() + 
  geom_smooth()

#plot: comparing winners and losers serve points won
wta_grand_slam_matches |> 
  ggplot(aes(x = w_svpt, y = l_svpt)) +
  geom_point() + 
  geom_smooth()

#plot: comparing winners break points faced and saved
wta_grand_slam_matches |> 
  ggplot(aes(x = w_bpFaced, y = w_bpSaved)) + 
  geom_point() +
  geom_smooth()

#plot: comparing losers break points faced and saved
wta_grand_slam_matches |> 
  ggplot(aes(x = l_bpFaced, y = l_bpSaved)) + 
  geom_point() +
  geom_smooth()

#plot: comparing winners and losers break points faced
wta_grand_slam_matches |> 
  ggplot(aes(x = w_bpFaced, y = l_bpFaced)) + 
  geom_point() +
  geom_smooth()

#plot: comparing winners and losers break points saved
wta_grand_slam_matches |> 
  ggplot(aes(x = w_bpSaved, y = l_bpSaved)) + 
  geom_point() +
  geom_smooth()

#plot: distribution of winner's countries
wta_grand_slam_matches |> 
  count(winner_ioc, name = "count") |> 
  filter(count >= 50) |> 
  ggplot(aes(x = winner_ioc, y = count)) +
  geom_col()

#plot: distribution of loser's countries
wta_grand_slam_matches |> 
  count(loser_ioc, name = "count") |> 
  filter(count >= 50) |> 
  ggplot(aes(x = loser_ioc, y = count)) +
  geom_col()

#plot: comparing winners and losers aces
wta_grand_slam_matches |> 
  ggplot(aes(x = w_ace, y = l_ace)) + 
  geom_point() +
  geom_smooth()

#plot: typical ace differential in a match
wta_grand_slam_matches |> 
  mutate(ace_diff = w_ace - l_ace) |> 
  ggplot(aes(x = ace_diff)) +
  geom_bar()

#plot: typical 1st serves won differential in a match
wta_grand_slam_matches |> 
  mutate(firstsrv_diff = w_1stWon - l_1stWon) |> 
  ggplot(aes(x = firstsrv_diff)) +
  geom_bar()

#plot: typical 2nd serves won differential in a match
wta_grand_slam_matches |> 
  mutate(secondsrv_diff = w_2ndWon - l_2ndWon) |> 
  ggplot(aes(x = secondsrv_diff)) +
  geom_bar()

#box plot: distribution of match length
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes)) +
  geom_boxplot() +
  theme(axis.text.y = element_blank())

#histogram: distribution of match length
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes)) + 
  geom_histogram()

#beeswarm plot: distribution of match length
library(ggbeeswarm)
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes, y = "")) +
  geom_beeswarm(cex = 2)

#violin plot + boxplot: distribution of match length
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes, y = "")) +
  geom_violin() + 
  geom_boxplot(width = 0.4)

#ECDF plot: match length
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes)) +
  stat_ecdf()

# handedness of winner vs loser comparison
table(wta_grand_slam_matches$winner_hand, wta_grand_slam_matches$loser_hand)
wta_grand_slam_matches |>
  group_by(winner_hand, loser_hand) |>
  summarize(
    freq = n(), 
    joint = n() / nrow(wta_grand_slam_matches)
  ) |> 
  ggplot(aes(x = loser_hand, y = winner_hand)) +
  geom_tile(aes(fill = freq), color = "white") +
  geom_text(aes(label = scales::percent(joint))) +
  scale_fill_gradient2()

#plot: do left handed players serve better than right handed_counterparts
# install.packages("tidymodels")
library(tidymodels)
wta_grand_slam_matches |> 
  filter((winner_hand == "R" & loser_hand == "L") | (winner_hand == "L" & loser_hand == "R")) |> 
  mutate(lefty_svpts = ifelse(winner_hand == "L", w_svpt, l_svpt),
         righty_svpts = ifelse(winner_hand == "R", w_svpt, l_svpt)) |> 
  select(lefty_svpts, righty_svpts) |> 
  ggplot(aes(lefty_svpts, righty_svpts)) +
  geom_point(alpha = 0.5) +
  coord_obs_pred() +
  geom_abline(slope = 1, intercept = 0, color = "red")

# trying to find most important variables for an upset
#option 1: serve point differential
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         svpt_diff = w_svpt - l_svpt) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = svpt_diff)) +
  geom_histogram()

#option 2: 1st serve won differential (MOST IMPORTANT)
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         firstwon_diff = w_1stWon - l_1stWon) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = firstwon_diff)) +
  geom_histogram()

#option 3: 2nd serve won differential
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         secondwon_diff = w_2ndWon - l_2ndWon) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = secondwon_diff)) +
  geom_histogram()

#option 4: ace differential
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         ace_diff = w_ace - l_ace) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = ace_diff)) +
  geom_histogram()

#option 5: first point made differential
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         firstin_diff = w_1stIn - l_1stIn) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = firstin_diff)) +
  geom_histogram()

#option 6: break points saved differential
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         bpsaved_diff = w_bpSaved - l_bpSaved) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = bpsaved_diff)) +
  geom_histogram()

#option 7: double fault differential
wta_grand_slam_matches |> 
  mutate(year = substr(tourney_date, 0, 4),
         rank_diff = winner_rank - loser_rank,
         df_diff = w_df - l_df) |> 
  filter(rank_diff >=10) |> 
  ggplot(aes(x = df_diff)) +
  geom_histogram()

