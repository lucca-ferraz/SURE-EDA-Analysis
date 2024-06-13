#hello world
library(tidyverse)
wta_grand_slam_matches <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/wta_grand_slam_matches.csv")

colnames(wta_grand_slam_matches)
unique(wta_grand_slam_matches$tourney_name)

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

#histogram: distribution of match length
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes)) + 
  geom_histogram()

#beeswarm plot: distribution of match length
library(ggbeeswarm)
wta_grand_slam_matches |> 
  ggplot(aes(x = minutes, y = "")) +
  geom_beeswarm(cex = 2)

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

# Create a dataframe for winners
winners <- wta_grand_slam_matches |> 
  select(tourney_name, surface, tourney_date, 
         seed = winner_seed, name = winner_name, hand = winner_hand, 
         height = winner_ht, ioc = winner_ioc, age = winner_age, 
         ace = w_ace, df = w_df, svpt = w_svpt, 
         firstIn = w_1stIn, firstWon = w_1stWon, 
         secondWon = w_2ndWon, svGms = w_SvGms, bpSaved = w_bpSaved, 
         bpFaced = w_bpFaced, rank = winner_rank, opponent_rank = loser_rank,
         opponent_seed = loser_seed, opponent_name = loser_name, opponent_hand = loser_hand, 
         opponent_height = loser_ht, opponent_ioc = loser_ioc, opponent_age = loser_age, 
         opponent_ace = l_ace, opponent_df = l_df, opponent_svpt = l_svpt, 
         opponent_firstIn = l_1stIn, opponent_firstWon = l_1stWon, 
         opponent_secondWon = l_2ndWon, opponent_svGms = l_SvGms, opponent_bpSaved = l_bpSaved, 
         opponent_bpFaced = l_bpFaced,
         minutes, score, round) |> 
  mutate(result = "Win",
         rank_diff = opponent_rank - rank)

# Create a dataframe for losers
losers <- wta_grand_slam_matches |> 
  select(tourney_name, surface, tourney_date, 
         seed = loser_seed, name = loser_name, hand = loser_hand, 
         height = loser_ht, ioc = loser_ioc, age = loser_age, 
         ace = l_ace, df = l_df, svpt = l_svpt, 
         firstIn = l_1stIn, firstWon = l_1stWon, 
         secondWon = l_2ndWon, svGms = l_SvGms, bpSaved = l_bpSaved, 
         bpFaced = l_bpFaced, rank = loser_rank, opponent_rank = winner_rank,
         opponent_seed = winner_seed, opponent_name = winner_name, opponent_hand = winner_hand, 
         opponent_height = winner_ht, opponent_ioc = winner_ioc, opponent_age = winner_age, 
         opponent_ace = w_ace, opponent_df = w_df, opponent_svpt = w_svpt, 
         opponent_firstIn = w_1stIn, opponent_firstWon = w_1stWon, 
         opponent_secondWon = w_2ndWon, opponent_svGms = w_SvGms, opponent_bpSaved = w_bpSaved, 
         opponent_bpFaced = w_bpFaced,
         minutes, score, round) |> 
  mutate(result = "Loss",
         rank_diff = opponent_rank - rank)

# Create a combined dataframe 
wta_grand_slam_players <- winners |> 
  mutate(row_num = row_number()) |> 
  bind_rows(losers |>  mutate(row_num = row_number())) |> 
  arrange(row_num, result)  |> 
  select(-row_num)

# plot: serves won vs opponent serves won in upsets
wta_grand_slam_players |> 
  filter(rank_diff < -50) |> 
  ggplot(aes(x = firstWon, y = opponent_firstWon, color = result, alpha = 0.5)) +
  geom_point() +
  ggthemes::theme_clean() +
  facet_wrap(~ result) +
  ggtitle("How Do First Serves Won Impact Upsets in Tennis?",
          subtitle = "Among matches with a rank differential above 50") +
  xlab("First Serves Won") + 
  ylab("Opponent First Serves Won") +
  guides(alpha = "none") + 
  labs(caption = "Data from WTA matches 2018-2023")

# plot: top 3 countries wins and losses
wta_grand_slam_players |> 
  count(ioc, name = "count", result) |> 
  filter(count >= 190) |> 
  ggplot(aes(x = ioc, y = count, fill = result)) +
  geom_col() +
  ggthemes::theme_clean() +
  facet_wrap(~ result)

colSums(is.na(wta_grand_slam_players))

player_stats <- wta_grand_slam_players |> 
  filter(score != "W/O") |> 
  group_by(name) |> 
  summarise(height = mean(height, na.rm = TRUE), first_serve_pct = sum(firstIn) / sum(svpt),
            first_serve_win_pct = sum(firstWon) / sum(firstIn),
            second_serve_win_pct = sum(secondWon) / (sum(svpt) - sum(firstIn)),
            ace_pct = sum(ace) / sum(svpt), bp_save_pct = sum(bpSaved) / sum(bpFaced),
            df_pct = sum(df, na.rm = TRUE) / sum(svpt), avg_match_length = mean(minutes, na.rm = TRUE), 
            games = n(), win_pct = sum(result == "Win") / n(),
            opponent_first_serve_win_pct = sum(opponent_firstWon) / sum(opponent_firstIn),
            opponent_second_serve_win_pct = sum(opponent_secondWon) / (sum(opponent_svpt) - sum(opponent_firstIn)),
            opponent_ace_pct = sum(opponent_ace) / sum(opponent_svpt),
            opponent_bp_break_pct = 1 - sum(opponent_bpSaved)/sum(opponent_bpFaced)
            )

# plot: first serve percent vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = first_serve_pct, y = win_pct)) +
  geom_point()

# plot: first serve win pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = first_serve_win_pct, y = win_pct)) +
  geom_point()

# plot: second serve win pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = second_serve_win_pct, y = win_pct)) +
  geom_point()

# plot: ace pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = ace_pct, y = win_pct)) +
  geom_point()

# plot: double fault pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = df_pct, y = win_pct)) +
  geom_point()

# plot: break point save pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = bp_save_pct, y = win_pct)) +
  geom_point()

# plot: opponent first serve win pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = opponent_first_serve_win_pct, y = win_pct)) +
  geom_point()

# plot: breaking opponents serve pct vs win pct
player_stats |> 
  filter(games > 10) |> 
  ggplot(aes(x = opponent_bp_break_pct, y = win_pct)) +
  geom_point()


#clustering players based on bp save pct and opponent bp break pct (how well they perform on break points)
clean_cluster_stats <- player_stats |> 
  filter(games > 10) |> 
  mutate(std_bp_save_pct = as.numeric(scale(bp_save_pct)),
         std_opponent_bp_break_pct = as.numeric(scale(opponent_bp_break_pct)))

# install.packages("NbClust")
library(NbClust)
clean_cluster_stats |> 
  select(std_bp_save_pct, std_opponent_bp_break_pct) |>
  NbClust(method = "complete", index = "all")

kmeans <- clean_cluster_stats |> 
  select(std_bp_save_pct, std_opponent_bp_break_pct) |> 
  kmeans(centers = 4, nstart = 30, algorithm = "Lloyd")

# Visualizing k-means clusters
clean_cluster_stats |> 
  mutate(player_cluster = factor(kmeans$cluster)) |> 
  ggplot(aes(x = std_bp_save_pct, y = std_opponent_bp_break_pct,
             color = player_cluster, shape = player_cluster)) +
  geom_point() +
  ggthemes::scale_color_colorblind() +
  ggthemes::theme_clean() +
  coord_fixed() +
  xlab("Standardized Break Point Saved %") +
  ylab("Standardized Opponent Break Points Broken %") +
  labs(title = "K-Means Clustering - Break Point Performance")

# density plot comparing win percentages for each cluster
clean_cluster_stats |> 
  mutate(player_cluster = factor(kmeans$cluster)) |> 
  group_by(player_cluster) |> 
  ggplot(aes(x = win_pct, fill = player_cluster)) +
  geom_density() +
  facet_wrap(~ player_cluster, labeller = labeller(player_cluster = c(
    "1" = "Good Saving BP, Avg Converting BP",
    "2" = "Avg Converting BP, Bad Saving BP",
    "3" = "Good Converting BP, Avg Saving BP",
    "4" = "Avg Saving BP, Bad Converting BP"
  ))) +
  labs(title = "Win Percentages Based on Cluster", 
       subtitle = "Clusters 1 and 3 tend to dominate",
       caption = "Data from WTA Matches 2018 - 2023") +
  ggthemes::theme_clean() + 
  scale_fill_discrete(name = "Player Cluster")


