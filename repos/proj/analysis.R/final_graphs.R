
*Offence rating v Wins*
  ```{r ofrating-wins}
of_wins <- comb_team %>%
  ggplot(aes(ORtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta")  # Strong linear relationship. Explore this more heavily

of_wins

cor(x = comb_team$ORtg, y = comb_team$W, method = "pearson")

of_wins_lm <- lm(W ~ ORtg, data = comb_team)
summary(of_wins_lm)

predict(of_wins_lm)

of_wins_stdres <- rstandard(of_wins_lm)
of_wins_points <- 1:length(of_wins_stdres)
of_wins_labels <- if_else(abs(of_wins_stdres) >= 1.5, paste(of_wins_points), "")

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = of_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

of_wins_hats <- hatvalues(of_wins_lm)

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_hats)) +
  geom_point()

of_wins_cook <- cooks.distance(of_wins_lm)

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_cook))+
  geom_point()

car::durbinWatsonTest(of_wins_lm)

of_wins_res <- residuals(of_wins_lm)
of_wins_fitted <- predict(of_wins_lm)

ggplot(data = NULL, aes(x = of_wins_fitted, y = of_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = of_wins_res)) +
  stat_qq() + stat_qq_line()
```











## Linear Relationship for Points 

Points = 3-Point FG + 2-Point FG + Free Throw

```{r pos-specific}
x3pts_ppg <- ggplot(data = p_stats, aes(x = x3P, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") # 3pt FG makes to total points

x3pts_ppg

x2pts_ppg <- ggplot(data = p_stats, aes(x = x2P, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") # 2pt FG makes to total points

x2pts_ppg


ftpts_ppg <- ggplot(data = p_stats, aes(x = FT, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") # FT makes to total points

ftpts_ppg
```

## Player Points Predictions by Position
*Point Guard*
  ```{r pts_predict_pg}
pg_ppg_pts_predict <- pg %>%
  filter(PPG >= 10) %>%
  mutate(pg_r_hat = predict(pg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pg_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()

pg_ppg_pts_predict
```


*Shooting Guard*
  ```{r pts_predict_sg}
sg_ppg_pts_predict <- sg %>%
  filter(PPG >= 10) %>%
  mutate(sg_r_hat = predict(sg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sg_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()

sg_ppg_pts_predict
```

*Small Forward*
  ```{r pts_predict_sf}
sf_ppg_pts_predict <- sf %>%
  filter(PPG >= 10) %>%
  mutate(sf_r_hat = predict(sf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sf_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()

sf_ppg_pts_predict
```

*Power Forward*
  ```{r pts_predict_pf}
pf_ppg_pts_predict <- pf %>%
  filter(PPG >= 10) %>%
  mutate(pf_r_hat = predict(pf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pf_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()

pf_ppg_pts_predict
```

*Centres*
  ```{r pts_predict_c}
c_ppg_pts_predict <- centres %>%
  filter(PPG >= 10) %>%
  mutate(c_r_hat = predict(c_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(c_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()

c_ppg_pts_predict
```

# Adding Salary values

*Point Guard*
  ```{r pg-value-pick}
pg_pts_hat <- pg %>%
  mutate(pg_hat = predict(pg_overall_tidy_ppg, newdata = .))

pg_salary <- pg_pts_hat %>%
  ggplot(aes(Salary, pg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1, cex = 2)


pg_salary # Gives the predicted points average against the players salary, to see where the value is. We select D'Angelo Russell at Point Guard. 
```

*Shooting Guard*
  ```{r sg-value-pick}
sg_pts_hat <- sg %>%
  mutate(sg_hat = predict(sg_overall_tidy_ppg, newdata = .))

sg_salary <- sg_pts_hat %>%
  ggplot(aes(Salary, sg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


sg_salary # Gives the predicted points average against the players salary, to see where the value is. We select Donovan Mitchell at Shooting Guard. 
```

*Small Forward*
  ```{r sf-value-pick}
sf_pts_hat <- sf %>%
  mutate(sf_hat = predict(sf_overall_tidy_ppg, newdata = .))

sf_salary <- sf_pts_hat %>%
  ggplot(aes(Salary, sf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


sf_salary # Gives the predicted points average against the players salary, to see where the value is. Depending on salary space, we either pick Kevin Durant or Kawhi Leonard at Small Forward as they stand out from the pack of lower priced players.
```

*Power Forward*
  ```{r pf-value-pick}
pf_pts_hat <- pf %>%
  mutate(pf_hat = predict(pf_overall_tidy_ppg, newdata = .))

pf_salary <- pf_pts_hat %>%
  ggplot(aes(Salary, pf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


pf_salary # Gives the predicted points average against the players salary, to see where the value is. We select either Julius Randle or Tobias Harris at the Power Forward position. 
```

*Centres*
  ```{r c-value-pick}
c_pts_hat <- centres %>%
  mutate(c_hat = predict(c_overall_tidy_ppg, newdata = .))

c_salary <- c_pts_hat %>%
  ggplot(aes(Salary, c_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


c_salary # Gives the predicted points average against the players salary, to see where the value is. We select Karl-Anthony Towns at Centre. 
```

## Selected Team
```{r selected-team}
selected_player_pg <- pg %>%
  filter(player_name == "D'Angelo Russell") # Filter for the chosen PG

selected_player_pg_2 <- pg %>%
  filter(player_name == "Kemba Walker") # Filter for the chosen PG

selected_pg <- bind_rows(selected_player_pg, selected_player_pg_2) # Combine the two selected PG players for a comparison to make a final decision. 

selected_player_sg <- sg %>%
  filter(player_name == "Donovan Mitchell") # Filter for the chosen SG

selected_player_sg_2 <- sg %>%
  filter(player_name == "Devin Booker") # Filter for the chosen SG

selected_sg <- bind_rows(selected_player_sg, selected_player_sg_2) # Combine the two selected SG players for a comparison to make a final decision. 

selected_player_sf <- sf %>%
  filter(player_name == "Kevin Durant") # Filter for the chosen SF

selected_player_sf_2 <- sf %>%
  filter(player_name == "Kawhi Leonard") # Filter for the chosen SF

selected_sf <- bind_rows(selected_player_sf, selected_player_sf_2) # Combine the two selected SF players for a comparison to make a final decision. 

selected_player_pf <- pf %>%
  filter(player_name == "Giannis Antetokounmpo") # Filter for the chosen PF

selected_player_pf_2 <- pf %>%
  filter(player_name == "Julius Randle") # Filter for the chosen PF

selected_pf <- bind_rows(selected_player_pf, selected_player_pf_2) # Combine the two selected PF players for a comparison to make a final decision. 

selected_player_c <- centres %>%
  filter(player_name == "Karl-Anthony Towns") # Filter for the chosen C
```




```{r completed_team}
selected_team <- bind_rows(selected_player_c, selected_player_pf_2, selected_player_sf, selected_player_sg, selected_player_pg_2) # Combine above filtered players and combine to express the chosen side. 

selected_team 
```