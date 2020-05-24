# Offence Rating v Wins

of_wins <- comb_team %>%
  ggplot(aes(ORtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  labs(title = "Offensive Rating relative to Wins",
       subtitle = "A Linear Relationship Explored",
       caption = "Plotting Team Offensive Ratings and their correlating Wins",
       x = "Team Offensive Ratings",
       y = "Season Wins") # Changing and configuring the Titles of the graph. 

# Defence Rating v Wins

def_wins <- comb_team %>%
  ggplot(aes(DRtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "orange") +
  labs(title = "Defensive Rating relative to Wins",
       subtitle = "A Linear Relationship",
       caption = "Plotting Team Defensive Ratings and their correlating Wins",
       x = "Team Defensive Ratings",
       y = "Season Wins") # Changing and configuring the Titles of the graph. 
# Testing for a linear relationship between Defensive Rating and Wins. Important to note that the negative linear relationship is actually reversed, as a higher defensive rating is not a good outcome for the team. A higher win rate and lower defensive rating is the ideal outcome.

# The Offence Residuals

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_stdres)) +
  geom_point(colour = "red") +
  geom_text(aes(label = of_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "darkgreen", linetype = "dashed") +
  labs(title = "Offensive Rating - Potential Leverage",
       subtitle = "A Linear Relationship Explored",
       caption = "Plotting Team Offensive Ratings and their leverage potential",
       x = "Points of Offensive Rating Residuals",
       y = "Standardised Residuals") # Changing and configuring the Titles of the graph. 

# Leverage of Offensive Rating

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_hats)) +
  geom_point(colour = "dodgerblue") +
  labs(title = "Measuring Offensive Rating Leverage relative to Wins",
       subtitle = "A Linear Relationship Explored",
       caption = "Measuring hat values to determine leverage points",
       x = "Points of Offensive Rating Residuals",
       y = "Hat Values of Linear Model") # Changing and configuring the Titles of the graph. 

# Collective Change in the Coefficients of Offensive Rating and Wins

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_cook))+
  geom_point(colour = "tomato2") +
  labs(title = "Collective Change in Coefficients of Offensive Rating and Wins",
       subtitle = "A Linear Relationship Explored",
       caption = "Plotting the Coefficient Changes",
       x = "Points of Offensive Rating Residuals",
       y = "Change in Coefficient") # Changing and configuring the Titles of the graph. 

# Homoscedasticity

ggplot(data = NULL, aes(x = of_wins_fitted, y = of_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  labs(title = "Offensive Rating relative to Wins",
       subtitle = "A Linear Relationship Explored",
       caption = "Measuring the constant variance of Predicted Wins",
       x = "Predicted Wins Relative to Offensive Rating",
       y = "Residuals of Wins Relative to Offensive Rating") # Changing and configuring the Titles of the graph. 


## Linear Relationship for Points 

#Points = 3-Point FG + 2-Point FG + Free Throw

#3 Point Relationship

final_x3pts_ppg <- ggplot(data = p_stats, aes(x = x3P, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") + # 3pt FG makes to total points
  labs(title = "Successful 3 Point Attempts and Overall Points",
       subtitle = "Identifying a Relationship between 3 Point Baskets and Total Points",
       x = "Successful 3 Point Baskets",
       y = "Total Points") + # Changing and configuring the Titles of the graph. 
  theme_linedraw()
  
  
#2 Point Relationship

final_x2pts_ppg <- ggplot(data = p_stats, aes(x = x2P, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") + # 2pt FG makes to total points
  labs(title = "Successful 2 Point Attempts and Overall Points",
     subtitle = "Identifying a Relationship between 2 Point Baskets and Total Points",
     x = "Successful 2 Point Baskets",
     y = "Total Points") + # Changing and configuring the Titles of the graph. 
  theme_linedraw()


#Free Throw Relationship

final_ftpts_ppg <- ggplot(data = p_stats, aes(x = FT, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") + # FT makes to total points
  labs(title = "Successful Free Throw Point Attempts and Overall Points",
       subtitle = "Identifying a Relationship between Free Throws and Total Points",
       x = "Successful Free Throws",
       y = "Total Points") + # Changing and configuring the Titles of the graph. 
  theme_linedraw()

# Player Points Predictions by Position

## Point Guard

final_pg_ppg_pts_predict <- pg %>%
  filter(PPG >= 10) %>% # This will filter out Point Guards who score under 10 Points Per Game
  mutate(pg_r_hat = predict(pg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pg_r_hat, PTS, label = player_name)) +
  geom_point(colour = "Red", alpha = 0.8) +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() + # This formula will predict each Point Guards predicted Points Per Game against their actual output 
  labs(title = "Predicted Points in comparison to Actual Points Output",
       subtitle = "Identifying Point Guards who perform to expected levels",
       x = "Predicted Points Per Game Average",
       y = "Total Points") + # Changing and configuring the Titles of the graph. 
  theme_linedraw() # Changing the theme of the graph.

## Shooting Guard

final_sg_ppg_pts_predict <- sg %>%
  filter(PPG >= 10) %>% # This will filter out Shooting Guards who score under 10 Points Per Game
  mutate(sg_r_hat = predict(sg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sg_r_hat, PTS, label = player_name)) +
  geom_point(colour = "Red", alpha = 0.8) +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() + # This formula will predict each Shooting Guards predicted Points Per Game against their actual output
  labs(title = "Predicted Points in comparison to Actual Points Output",
       subtitle = "Identifying Shooting Guards who perform to expected levels",
       x = "Predicted Points Per Game Average",
       y = "Total Points") + # Changing and configuring the Titles of the graph
  theme_linedraw() # Changing the theme of the graph.

## Small Forward

final_sf_ppg_pts_predict <- sf %>%
  filter(PPG >= 10) %>% # This will filter out Small Forwards who score under 10 Points Per Game
  mutate(sf_r_hat = predict(sf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sf_r_hat, PTS, label = player_name)) +
  geom_point(colour = "Red", alpha = 0.8) +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() + # This formula will predict each Small Forwards predicted Points Per Game against their actual output
  labs(title = "Predicted Points in comparison to Actual Points Output",
       subtitle = "Identifying Small Forwards who perform to expected levels",
       x = "Predicted Points Per Game Average",
       y = "Total Points") + # Changing and configuring the Titles of the graph
  theme_linedraw() # Changing the theme of the graph.

## Power Forward

final_pf_ppg_pts_predict <- pf %>%
  filter(PPG >= 10) %>% # This will filter out Power Forwards who score under 10 Points Per Game
  mutate(pf_r_hat = predict(pf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pf_r_hat, PTS, label = player_name)) +
  geom_point(colour = "Red", alpha = 0.8) +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() + # This formula will predict each Power Forwards predicted Points Per Game against their actual output
  labs(title = "Predicted Points in comparison to Actual Points Output",
       subtitle = "Identifying Power Forwards who perform to expected levels",
       x = "Predicted Points Per Game Average",
       y = "Total Points") + # Changing and configuring the Titles of the graph 
  theme_linedraw() # Changing the theme of the graph.

## Centre

final_c_ppg_pts_predict <- centres %>%
  filter(PPG >= 10) %>% # This will filter out Centres who score under 10 Points Per Game
  mutate(c_r_hat = predict(c_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(c_r_hat, PTS, label = player_name)) +
  geom_point(colour = "Red", alpha = 0.8) +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() + # This formula will predict each Centres predicted Points Per Game against their actual output 
  labs(title = "Predicted Points in comparison to Actual Points Output",
       subtitle = "Identifying Centres who perform to expected levels",
       x = "Predicted Points Per Game Average",
       y = "Total Points") + # Changing and configuring the Titles of the graph 
  theme_linedraw() # Changing the theme of the graph.

# Adding Salary values

## Point Guard
pg_pts_hat <- pg %>%
  mutate(pg_hat = predict(pg_overall_tidy_ppg, newdata = .))

pg_salary <- pg_pts_hat %>%
  ggplot(aes(Salary, pg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1, cex = 2) + # Gives the predicted points average against the players salary, to see where the value is. We select D'Angelo Russell at Point Guard. 
  geom_hline(yintercept = 20, colour = "navy", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "navy", linetype = "dotted") + # Divides the graph into sections to indicate where players may be overpriced for their output
  labs(title = "Predicted Points and Current Salary for Point Guards",
       subtitle = "Finding Value in the Point Guard Position",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Predicted Points Per Game",
       colour = "Player Position")

## Shooting Guard
sg_pts_hat <- sg %>%
  mutate(sg_hat = predict(sg_overall_tidy_ppg, newdata = .))

sg_salary <- sg_pts_hat %>%
  ggplot(aes(Salary, sg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) + # Gives the predicted points average against the players salary, to see where the value is. We select Donovan Mitchell at Shooting Guard. 
  geom_hline(yintercept = 20, colour = "cyan", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "cyan", linetype = "dotted") + # Divides the graph into sections to indicate where players may be overpriced for their output
  labs(title = "Predicted Points and Current Salary for Shooting Guards",
       subtitle = "Finding Value in the Shooting Guard Position",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Predicted Points Per Game",
       colour = "Player Position")

## Small Forward
sf_pts_hat <- sf %>%
  mutate(sf_hat = predict(sf_overall_tidy_ppg, newdata = .))

sf_salary <- sf_pts_hat %>%
  ggplot(aes(Salary, sf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) + # Gives the predicted points average against the players salary, to see where the value is. Depending on salary space, we either pick Kevin Durant or Kawhi Leonard at Small Forward as they stand out from the pack of lower priced players.
  geom_hline(yintercept = 20, colour = "darkorchid2", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "darkorchid2", linetype = "dotted") + # Divides the graph into sections to indicate where players may be overpriced for their output
  labs(title = "Predicted Points and Current Salary for Small Forwards",
       subtitle = "Finding Value in the Small Forward Position",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Predicted Points Per Game",
       colour = "Player Position")

## Power Forward
pf_pts_hat <- pf %>%
  mutate(pf_hat = predict(pf_overall_tidy_ppg, newdata = .))

pf_salary <- pf_pts_hat %>%
  ggplot(aes(Salary, pf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) + # Gives the predicted points average against the players salary, to see where the value is. We select either Julius Randle or Tobias Harris at the Power Forward position. 
  geom_hline(yintercept = 20, colour = "dodgerblue", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "dodgerblue", linetype = "dotted") + # Divides the graph into sections to indicate where players may be overpriced for their output
  labs(title = "Predicted Points and Current Salary for Power Forwards",
       subtitle = "Finding Value in the Power Forward Position",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Predicted Points Per Game",
       colour = "Player Position")

## Centre

c_pts_hat <- centres %>%
  mutate(c_hat = predict(c_overall_tidy_ppg, newdata = .))

c_salary <- c_pts_hat %>%
  ggplot(aes(Salary, c_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) + # Gives the predicted points average against the players salary, to see where the value is. We select Karl-Anthony Towns at Centre. 
  geom_hline(yintercept = 20, colour = "black", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "black", linetype = "dotted") + # Divides the graph into sections to indicate where players may be overpriced for their output
  labs(title = "Predicted Points and Current Salary for Centres",
        subtitle = "Finding Value in the Centre Position",
        caption = "Finding a cheaper alternative with similar predicted output",
        x = "Player Salary",
        y = "Predicted Points Per Game",
       colour = "Player Position") # Changing and configuring the Titles of the graph. 

# Combined Salary and Per Game Statistics

## Points Per Game

combined_ppgsalary_value <- p_stats %>%
  ggplot(aes(Salary, PPG, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1, cex = 2) +
  geom_hline(yintercept = 20, colour = "navy", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "navy", linetype = "dotted") +
  scale_y_continuous(limits = c(10,35)) +
  labs(title = "Current Points Per Game",
       subtitle = "Finding Value in the NBA",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Points Per Game",
       colour = "Player Position") # Changing and configuring the Titles of the graph. 

## Rebounds Per Game 

combined_rpgsalary_value <- p_stats %>%
  ggplot(aes(Salary, RPG, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = .5, cex = 2) +
  geom_hline(yintercept = 7, colour = "navy", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "navy", linetype = "dotted") +
  scale_y_continuous(limits = c(5,17)) +
  labs(title = "Current Total Rebounds Per Game",
       subtitle = "Finding Value in the NBA",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Rebounds Per Game",
       colour = "Player Position") # Changing and configuring the Titles of the graph. 

## Assists Per Game

combined_astsalary_value <- p_stats %>%
  ggplot(aes(Salary, APG, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 0.5, cex = 2) +
  geom_hline(yintercept = 7, colour = "navy", linetype = "dotted") +
  geom_vline(xintercept = 15000000, colour = "navy", linetype = "dotted") +
  scale_y_continuous(limits = c(4,12)) +
  labs(title = "Current Assists Per Game",
       subtitle = "Finding Value in the NBA",
       caption = "Finding a cheaper alternative with similar predicted output",
       x = "Player Salary",
       y = "Assists Per Game",
       colour = "Player Position") + # Changing and configuring the Titles of the graph. 
  theme_bw() # Change the theme of the graph






