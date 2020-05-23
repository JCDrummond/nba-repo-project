## Explore various data models
### Single Varible Linear Regression

#Team Statistics
#Pace v Wins

Pace_wins <- comb_team %>%
  ggplot(aes(Pace, W)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", colour = "magenta") # No linear relationship

Pace_wins

cor(x = comb_team$Pace, y = comb_team$W, method = "pearson")

pace_wins_lm <- lm(W ~ Pace, data = comb_team)
summary(pace_wins_lm)

predict(pace_wins_lm)

pace_wins_stdres <- rstandard(pace_wins_lm)
pace_wins_points <- 1:length(pace_wins_stdres)
pace_wins_labels <- if_else(abs(pace_wins_stdres) >= 1.5, paste(pace_wins_points), "")

ggplot(data = NULL, aes(x = pace_wins_points, y = pace_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = pace_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

pace_wins_hats <- hatvalues(pace_wins_lm)

ggplot(data = NULL, aes(x = pace_wins_points, y = pace_wins_hats)) +
  geom_point()

pace_wins_cook <- cooks.distance(pace_wins_lm)

ggplot(data = NULL, aes(x = pace_wins_points, y = pace_wins_cook))+
  geom_point()

car::durbinWatsonTest(pace_wins_lm)

pace_wins_res <- residuals(pace_wins_lm)
pace_wins_fitted <- predict(pace_wins_lm)

ggplot(data = NULL, aes(x = pace_wins_fitted, y = pace_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = pace_wins_res)) +
  stat_qq() + stat_qq_line()


#Offence rating v Wins

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


#Defence rating v Wins

def_wins <- comb_team %>%
  ggplot(aes(DRtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Strong linear relationship. Explore this more heavily. (Appears as a negative because the lower the rating better in this instance.)

def_wins

cor(x = comb_team$DRtg, y = comb_team$W, method = "pearson")

def_wins_lm <- lm(W ~ DRtg, data = comb_team)
summary(def_wins_lm)

predict(def_wins_lm)

def_wins_stdres <- rstandard(def_wins_lm)
def_wins_points <- 1:length(def_wins_stdres)
def_wins_labels <- if_else(abs(def_wins_stdres) >= 1.5, paste(def_wins_points), "")

ggplot(data = NULL, aes(x = def_wins_points, y = def_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = def_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

def_wins_hats <- hatvalues(def_wins_lm)

ggplot(data = NULL, aes(x = def_wins_points, y = def_wins_hats)) +
  geom_point()

def_wins_cook <- cooks.distance(def_wins_lm)

ggplot(data = NULL, aes(x = def_wins_points, y = def_wins_cook))+
  geom_point()

car::durbinWatsonTest(def_wins_lm)

def_wins_res <- residuals(def_wins_lm)
def_wins_fitted <- predict(def_wins_lm)

ggplot(data = NULL, aes(x = def_wins_fitted, y = def_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = def_wins_res)) +
  stat_qq() + stat_qq_line()


#3-Point Attempt Rating v Wins

x3PAr_wins <- comb_team %>%
  ggplot(aes(x3PAr, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Neutral, no relationship

x3PAr_wins

cor(x = comb_team$x3PAr, y = comb_team$W, method = "pearson")

x3PAr_wins_lm <- lm(W ~ x3PAr, data = comb_team)
summary(x3PAr_wins_lm)

predict(x3PAr_wins_lm)

x3PAr_wins_stdres <- rstandard(x3PAr_wins_lm)
x3PAr_wins_points <- 1:length(x3PAr_wins_stdres)
x3PAr_wins_labels <- if_else(abs(x3PAr_wins_stdres) >= 1.5, paste(x3PAr_wins_points), "")

ggplot(data = NULL, aes(x = x3PAr_wins_points, y = x3PAr_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = x3PAr_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x3PAr_wins_hats <- hatvalues(x3PAr_wins_lm)

ggplot(data = NULL, aes(x = x3PAr_wins_points, y = x3PAr_wins_hats)) +
  geom_point()

x3PAr_wins_cook <- cooks.distance(x3PAr_wins_lm)

ggplot(data = NULL, aes(x = x3PAr_wins_points, y = x3PAr_wins_cook))+
  geom_point()

car::durbinWatsonTest(x3PAr_wins_lm)

x3PAr_wins_res <- residuals(x3PAr_wins_lm)
x3PAr_wins_fitted <- predict(x3PAr_wins_lm)

ggplot(data = NULL, aes(x = x3PAr_wins_fitted, y = x3PAr_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x3PAr_wins_res)) +
  stat_qq() + stat_qq_line()


#True Shooting Percentage v Wins

TSP_wins <- comb_team %>%
  ggplot(aes(TSp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship. Explore for player specific models

TSP_wins

cor(x = comb_team$TSp, y = comb_team$W, method = "pearson")

TSP_wins_lm <- lm(W ~ TSp, data = comb_team)
summary(TSP_wins_lm)

predict(TSP_wins_lm)

TSP_wins_stdres <- rstandard(TSP_wins_lm)
TSP_wins_points <- 1:length(TSP_wins_stdres)
TSP_wins_labels <- if_else(abs(TSP_wins_stdres) >= 1.5, paste(TSP_wins_points), "")

ggplot(data = NULL, aes(x = TSP_wins_points, y = TSP_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = TSP_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

TSP_wins_hats <- hatvalues(TSP_wins_lm)

ggplot(data = NULL, aes(x = TSP_wins_points, y = TSP_wins_hats)) +
  geom_point()

TSP_wins_cook <- cooks.distance(TSP_wins_lm)

ggplot(data = NULL, aes(x = TSP_wins_points, y = TSP_wins_cook))+
  geom_point()

car::durbinWatsonTest(TSP_wins_lm)

TSP_wins_res <- residuals(TSP_wins_lm)
TSP_wins_fitted <- predict(TSP_wins_lm)

ggplot(data = NULL, aes(x = TSP_wins_fitted, y = TSP_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = TSP_wins_res)) +
  stat_qq() + stat_qq_line()


#Effective Field Goal Percentage

eFGp_wins <- comb_team %>%
  ggplot(aes(eFGp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relatonship. Explore for player specific models

eFGp_wins

cor(x = comb_team$eFGp, y = comb_team$W, method = "pearson")

eFGp_wins_lm <- lm(W ~ eFGp, data = comb_team)
summary(eFGp_wins_lm)

predict(eFGp_wins_lm)

eFGp_wins_stdres <- rstandard(eFGp_wins_lm)
eFGp_wins_points <- 1:length(eFGp_wins_stdres)
eFGp_wins_labels <- if_else(abs(eFGp_wins_stdres) >= 1.5, paste(eFGp_wins_points), "")

ggplot(data = NULL, aes(x = eFGp_wins_points, y = eFGp_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = eFGp_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

eFGp_wins_hats <- hatvalues(eFGp_wins_lm)

ggplot(data = NULL, aes(x = eFGp_wins_points, y = eFGp_wins_hats)) +
  geom_point()

eFGp_wins_cook <- cooks.distance(eFGp_wins_lm)

ggplot(data = NULL, aes(x = eFGp_wins_points, y = eFGp_wins_cook))+
  geom_point()

car::durbinWatsonTest(eFGp_wins_lm)

eFGp_wins_res <- residuals(eFGp_wins_lm)
eFGp_wins_fitted <- predict(eFGp_wins_lm)

ggplot(data = NULL, aes(x = eFGp_wins_fitted, y = eFGp_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = eFGp_wins_res)) +
  stat_qq() + stat_qq_line()


#Defensive Rebound Percentage v Lower Defensive Rating

defrb_drtg <- comb_team %>%
  ggplot(aes(DRBp, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta")

defrb_drtg

cor(x = comb_team$DRBp, y = comb_team$DRtg, method = "pearson")

defrb_drtg_lm <- lm(DRtg ~ DRBp, data = comb_team)
summary(defrb_drtg_lm)

predict(defrb_drtg_lm)

defrb_drtg_stdres <- rstandard(defrb_drtg_lm)
defrb_drtg_points <- 1:length(defrb_drtg_stdres)
defrb_drtg_labels <- if_else(abs(defrb_drtg_stdres) >= 1.5, paste(defrb_drtg_points), "")

ggplot(data = NULL, aes(x = defrb_drtg_points, y = defrb_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = defrb_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") ## Has an outlier at point number 6, need to explore this to see it's impact on the data. Doesn't appear to have a high leverage or high influence.

defrb_drtg_hats <- hatvalues(defrb_drtg_lm)

ggplot(data = NULL, aes(x = defrb_drtg_points, y = defrb_drtg_hats)) +
  geom_point()

defrb_drtg_cook <- cooks.distance(defrb_drtg_lm)

ggplot(data = NULL, aes(x = defrb_drtg_points, y = defrb_drtg_cook))+
  geom_point()

car::durbinWatsonTest(defrb_drtg_lm)

defrb_drtg_res <- residuals(defrb_drtg_lm)
defrb_drtg_fitted <- predict(defrb_drtg_lm)

ggplot(data = NULL, aes(x = defrb_drtg_fitted, y = defrb_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = defrb_drtg_res)) +
  stat_qq() + stat_qq_line()


#Turnover Totals contributing to Losses
  
tov_loss <- comb_team %>%
  ggplot(aes(TOV, L)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # No relationship

tov_loss

cor(x = comb_team$TOV, y = comb_team$L, method = "pearson")

tov_loss_lm <- lm(L ~ TOV, data = comb_team)
summary(tov_loss_lm)

predict(tov_loss_lm)

tov_loss_stdres <- rstandard(tov_loss_lm)
tov_loss_points <- 1:length(tov_loss_stdres)
tov_loss_labels <- if_else(abs(tov_loss_stdres) >= 1.5, paste(tov_loss_points), "")

ggplot(data = NULL, aes(x = tov_loss_points, y = tov_loss_stdres)) +
  geom_point() +
  geom_text(aes(label = tov_loss_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

tov_loss_hats <- hatvalues(tov_loss_lm)

ggplot(data = NULL, aes(x = tov_loss_points, y = tov_loss_hats)) +
  geom_point()

tov_loss_cook <- cooks.distance(tov_loss_lm)

ggplot(data = NULL, aes(x = tov_loss_points, y = tov_loss_cook))+
  geom_point()

car::durbinWatsonTest(tov_loss_lm)

tov_loss_res <- residuals(tov_loss_lm)
tov_loss_fitted <- predict(tov_loss_lm)

ggplot(data = NULL, aes(x = tov_loss_fitted, y = tov_loss_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = tov_loss_res)) +
  stat_qq() + stat_qq_line()


#Net Rating v Wins
  
net_win <- comb_team %>%
  ggplot(aes(NRtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Strong relationship. Check impact

net_win

cor(x = comb_team$NRtg, y = comb_team$W, method = "pearson")

net_wins_lm <- lm(W ~ NRtg, data = comb_team)
summary(net_wins_lm)

predict(net_wins_lm)

net_wins_stdres <- rstandard(net_wins_lm)
net_wins_points <- 1:length(net_wins_stdres)
net_wins_labels <- if_else(abs(net_wins_stdres) >= 1.5, paste(net_wins_points), "")

ggplot(data = NULL, aes(x = net_wins_points, y = net_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = net_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

net_wins_hats <- hatvalues(net_wins_lm)

ggplot(data = NULL, aes(x = net_wins_points, y = net_wins_hats)) +
  geom_point()

net_wins_cook <- cooks.distance(net_wins_lm)

ggplot(data = NULL, aes(x = net_wins_points, y = net_wins_cook))+
  geom_point()

car::durbinWatsonTest(net_wins_lm)

net_wins_res <- residuals(net_wins_lm)
net_wins_fitted <- predict(net_wins_lm)

ggplot(data = NULL, aes(x = net_wins_fitted, y = net_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = net_wins_res)) +
  stat_qq() + stat_qq_line()


#Total Points contributing to Offensive Rating*

pts_off <- comb_team %>%
  ggplot(aes(PTS, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Strong linear relationship

pts_off

cor(x = comb_team$PTS, y = comb_team$W, method = "pearson")

pts_wins_lm <- lm(W ~ PTS, data = comb_team)
summary(pts_wins_lm)

predict(pts_wins_lm)

pts_wins_stdres <- rstandard(pts_wins_lm)
pts_wins_points <- 1:length(pts_wins_stdres)
pts_wins_labels <- if_else(abs(pts_wins_stdres) >= 1.5, paste(pts_wins_points), "")

ggplot(data = NULL, aes(x = pts_wins_points, y = pts_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = pts_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

pts_wins_hats <- hatvalues(pts_wins_lm)

ggplot(data = NULL, aes(x = pts_wins_points, y = net_wins_hats)) +
  geom_point()

pts_wins_cook <- cooks.distance(pts_wins_lm)

ggplot(data = NULL, aes(x = pts_wins_points, y = pts_wins_cook))+
  geom_point()

car::durbinWatsonTest(pts_wins_lm)

pts_wins_res <- residuals(pts_wins_lm)
pts_wins_fitted <- predict(pts_wins_lm)

ggplot(data = NULL, aes(x = pts_wins_fitted, y = pts_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = pts_wins_res)) +
  stat_qq() + stat_qq_line()


#Three points v Wins
  
x3_wins <- comb_team %>%
  ggplot(aes(x3P, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

x3_wins

cor(x = comb_team$x3P, y = comb_team$W, method = "pearson")

x3_wins_lm <- lm(W ~ x3P, data = comb_team)
summary(x3_wins_lm)

predict(x3_wins_lm)

x3_wins_stdres <- rstandard(x3_wins_lm)
x3_wins_points <- 1:length(x3_wins_stdres)
x3_wins_labels <- if_else(abs(x3_wins_stdres) >= 1.5, paste(x3_wins_points), "")

ggplot(data = NULL, aes(x = x3_wins_points, y = x3_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = x3_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x3_wins_hats <- hatvalues(x3_wins_lm)

ggplot(data = NULL, aes(x = x3_wins_points, y = x3_wins_hats)) +
  geom_point()

x3_wins_cook <- cooks.distance(x3_wins_lm)

ggplot(data = NULL, aes(x = x3_wins_points, y = x3_wins_cook))+
  geom_point()

car::durbinWatsonTest(x3_wins_lm)

x3_wins_res <- residuals(x3_wins_lm)
x3_wins_fitted <- predict(x3_wins_lm)

ggplot(data = NULL, aes(x = x3_wins_fitted, y = x3_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x3_wins_res)) +
  stat_qq() + stat_qq_line()


#Two points v Wins
  
x2_wins <- comb_team %>%
  ggplot(aes(x2P, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # No linear relationship

x2_wins

cor(x = comb_team$x2P, y = comb_team$W, method = "pearson")

x2_wins_lm <- lm(W ~ x2P, data = comb_team)
summary(x2_wins_lm)

predict(x2_wins_lm)

x2_wins_stdres <- rstandard(x2_wins_lm)
x2_wins_points <- 1:length(x2_wins_stdres)
x2_wins_labels <- if_else(abs(x2_wins_stdres) >= 1.5, paste(x2_wins_points), "")

ggplot(data = NULL, aes(x = x2_wins_points, y = x2_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = x2_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x2_wins_hats <- hatvalues(x2_wins_lm)

ggplot(data = NULL, aes(x = x2_wins_points, y = x2_wins_hats)) +
  geom_point()

x2_wins_cook <- cooks.distance(x2_wins_lm)

ggplot(data = NULL, aes(x = x2_wins_points, y = x2_wins_cook))+
  geom_point()

car::durbinWatsonTest(x2_wins_lm)

x2_wins_res <- residuals(x2_wins_lm)
x2_wins_fitted <- predict(x2_wins_lm)

ggplot(data = NULL, aes(x = x2_wins_fitted, y = x2_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x2_wins_res)) +
  stat_qq() + stat_qq_line()


#Free Throws v wins
  
ft_wins <- comb_team %>%
  ggplot(aes(FT, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Weak linear relationship

ft_wins

cor(x = comb_team$FT, y = comb_team$W, method = "pearson")

ft_wins_lm <- lm(W ~ FT, data = comb_team)
summary(ft_wins_lm)

predict(ft_wins_lm)

ft_wins_stdres <- rstandard(ft_wins_lm)
ft_wins_points <- 1:length(ft_wins_stdres)
ft_wins_labels <- if_else(abs(ft_wins_stdres) >= 1.5, paste(ft_wins_points), "")

ggplot(data = NULL, aes(x = ft_wins_points, y = ft_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = ft_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

ft_wins_hats <- hatvalues(ft_wins_lm)

ggplot(data = NULL, aes(x = ft_wins_points, y = ft_wins_hats)) +
  geom_point()

ft_wins_cook <- cooks.distance(ft_wins_lm)

ggplot(data = NULL, aes(x = ft_wins_points, y = ft_wins_cook))+
  geom_point()

car::durbinWatsonTest(ft_wins_lm)

ft_wins_res <- residuals(ft_wins_lm)
ft_wins_fitted <- predict(ft_wins_lm)

ggplot(data = NULL, aes(x = ft_wins_fitted, y = ft_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = ft_wins_res)) +
  stat_qq() + stat_qq_line()


#Three Points v Offensive Rating

x3p_ortg <- comb_team %>%
  ggplot(aes(x3P, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

x3p_ortg

cor(x = comb_team$x3P, y = comb_team$ORtg, method = "pearson")

x3p_ortg_lm <- lm(ORtg ~ x3P, data = comb_team)
summary(x3p_ortg_lm)

predict(x3p_ortg_lm)

x3p_ortg_stdres <- rstandard(x3p_ortg_lm)
x3p_ortg_points <- 1:length(x3p_ortg_stdres)
x3p_ortg_labels <- if_else(abs(x3p_ortg_stdres) >= 1.5, paste(x3p_ortg_points), "")

ggplot(data = NULL, aes(x = x3p_ortg_points, y = x3p_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x3p_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x3p_ortg_hats <- hatvalues(x3p_ortg_lm)

ggplot(data = NULL, aes(x = x3p_ortg_points, y = x3p_ortg_hats)) +
  geom_point()

x3p_ortg_cook <- cooks.distance(x3p_ortg_lm)

ggplot(data = NULL, aes(x = x3p_ortg_points, y = x3p_ortg_cook))+
  geom_point()

car::durbinWatsonTest(x3p_ortg_lm)

x3p_ortg_res <- residuals(x3p_ortg_lm)
x3p_ortg_fitted <- predict(x3p_ortg_lm)

ggplot(data = NULL, aes(x = x3p_ortg_fitted, y = x3p_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x3p_ortg_res)) +
  stat_qq() + stat_qq_line()


#Two Points v Offensive Rating

x2p_ortg <- comb_team %>%
  ggplot(aes(x2P, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Weak to no linear relationship, but teams making the higher two point baskets generally had a higher ORtg. Outlier may be included here though. 

x2p_ortg

cor(x = comb_team$x2P, y = comb_team$ORtg, method = "pearson")

x2p_ortg_lm <- lm(ORtg ~ x2P, data = comb_team)
summary(x2p_ortg_lm)

predict(x2p_ortg_lm)

x2p_ortg_stdres <- rstandard(x2p_ortg_lm)
x2p_ortg_points <- 1:length(x2p_ortg_stdres)
x2p_ortg_labels <- if_else(abs(x2p_ortg_stdres) >= 1.5, paste(x2p_ortg_points), "")

ggplot(data = NULL, aes(x = x2p_ortg_points, y = x2p_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x2p_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x2p_ortg_hats <- hatvalues(x2p_ortg_lm)

ggplot(data = NULL, aes(x = x2p_ortg_points, y = x2p_ortg_hats)) +
  geom_point()

x2p_ortg_cook <- cooks.distance(x2p_ortg_lm)

ggplot(data = NULL, aes(x = x2p_ortg_points, y = x2p_ortg_cook))+
  geom_point()

car::durbinWatsonTest(x2p_ortg_lm)

x2p_ortg_res <- residuals(x2p_ortg_lm)
x2p_ortg_fitted <- predict(x2p_ortg_lm)

ggplot(data = NULL, aes(x = x2p_ortg_fitted, y = x2p_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x2p_ortg_res)) +
  stat_qq() + stat_qq_line()


#Free Throws v Offensive Rating

ft_ortg <- comb_team %>%
  ggplot(aes(FT, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

ft_ortg

cor(x = comb_team$FT, y = comb_team$ORtg, method = "pearson")

ft_ortg_lm <- lm(ORtg ~ FT, data = comb_team)
summary(ft_ortg_lm)

predict(ft_ortg_lm)

ft_ortg_stdres <- rstandard(ft_ortg_lm)
ft_ortg_points <- 1:length(ft_ortg_stdres)
ft_ortg_labels <- if_else(abs(ft_ortg_stdres) >= 1.5, paste(ft_ortg_points), "")

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = ft_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

ft_ortg_hats <- hatvalues(ft_ortg_lm)

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_hats)) +
  geom_point()

ft_ortg_cook <- cooks.distance(ft_ortg_lm)

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_cook))+
  geom_point()

car::durbinWatsonTest(ft_ortg_lm)

ft_ortg_res <- residuals(ft_ortg_lm)
ft_ortg_fitted <- predict(ft_ortg_lm)

ggplot(data = NULL, aes(x = ft_ortg_fitted, y = ft_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = ft_ortg_res)) +
  stat_qq() + stat_qq_line()


#Three Point Percentage v Wins

x3pp_w <- comb_team %>%
  ggplot(aes(x3Pp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

x3pp_w

cor(x = comb_team$x3Pp, y = comb_team$W, method = "pearson")

x3pp_w_lm <- lm(W ~ x3Pp, data = comb_team)
summary(x3pp_w_lm)

predict(x3pp_w_lm)

x3pp_w_stdres <- rstandard(x3pp_w_lm)
x3pp_w_points <- 1:length(x3pp_w_stdres)
x3pp_w_labels <- if_else(abs(x3pp_w_stdres) >= 1.5, paste(x3pp_w_points), "")

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_stdres)) +
  geom_point() +
  geom_text(aes(label = x3pp_w_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x3pp_w_hats <- hatvalues(x3pp_w_lm)

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_hats)) +
  geom_point()

x3pp_w_cook <- cooks.distance(x3pp_w_lm)

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_cook))+
  geom_point()

car::durbinWatsonTest(x3pp_w_lm)

x3pp_w_res <- residuals(x3pp_w_lm)
x3pp_w_fitted <- predict(x3pp_w_lm)

ggplot(data = NULL, aes(x = x3pp_w_fitted, y = x3pp_w_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x3pp_w_res)) +
  stat_qq() + stat_qq_line()


#Three Point Percentage v Offensive Rating

x3pp_ortg <- comb_team %>%
  ggplot(aes(x3Pp, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

x3pp_ortg

cor(x = comb_team$x3Pp, y = comb_team$ORtg, method = "pearson")

x3pp_ortg_lm <- lm(ORtg ~ x3Pp, data = comb_team)
summary(x3pp_ortg_lm)

predict(x3pp_ortg_lm)

x3pp_ortg_stdres <- rstandard(x3pp_ortg_lm)
x3pp_ortg_points <- 1:length(x3pp_ortg_stdres)
x3pp_ortg_labels <- if_else(abs(x3pp_ortg_stdres) >= 1.5, paste(x3pp_ortg_points), "")

ggplot(data = NULL, aes(x = x3pp_ortg_points, y = x3pp_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x3pp_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x3pp_ortg_hats <- hatvalues(x3pp_ortg_lm)

ggplot(data = NULL, aes(x = x3pp_ortg_points, y = x3pp_ortg_hats)) +
  geom_point()

x3pp_ortg_cook <- cooks.distance(x3pp_ortg_lm)

ggplot(data = NULL, aes(x = x3pp_ortg_points, y = x3pp_ortg_cook))+
  geom_point()

car::durbinWatsonTest(x3pp_ortg_lm)

x3pp_ortg_res <- residuals(x3pp_ortg_lm)
x3pp_ortg_fitted <- predict(x3pp_ortg_lm)

ggplot(data = NULL, aes(x = x3pp_ortg_fitted, y = x3pp_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x3pp_ortg_res)) +
  stat_qq() + stat_qq_line()


#Two Point Percentage v Wins

x2pp_w <- comb_team %>%
  ggplot(aes(x2Pp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

x2pp_w

cor(x = comb_team$x2Pp, y = comb_team$W, method = "pearson")

x2pp_w_lm <- lm(W ~ x2Pp, data = comb_team)
summary(x2pp_w_lm)

predict(x2pp_w_lm)

x2pp_w_stdres <- rstandard(x2pp_w_lm)
x2pp_w_points <- 1:length(x2pp_w_stdres)
x2pp_w_labels <- if_else(abs(x2pp_w_stdres) >= 1.5, paste(x2pp_w_points), "")

ggplot(data = NULL, aes(x = x2pp_w_points, y = x2pp_w_stdres)) +
  geom_point() +
  geom_text(aes(label = x2pp_w_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x3pp_w_hats <- hatvalues(x3pp_w_lm)

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_hats)) +
  geom_point()

x3pp_w_cook <- cooks.distance(x3pp_w_lm)

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_cook))+
  geom_point()

car::durbinWatsonTest(x3pp_w_lm)

x3pp_w_res <- residuals(x3pp_w_lm)
x3pp_w_fitted <- predict(x3pp_w_lm)

ggplot(data = NULL, aes(x = x3pp_w_fitted, y = x3pp_w_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x3pp_w_res)) +
  stat_qq() + stat_qq_line()


#Two Point Percentage v Offensive Rating

x2pp_ortg <- comb_team %>%
  ggplot(aes(x2Pp, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

x2pp_ortg

cor(x = comb_team$x3Pp, y = comb_team$ORtg, method = "pearson")

x2pp_ortg_lm <- lm(ORtg ~ x2Pp, data = comb_team)
summary(x2pp_ortg_lm)

predict(x2pp_ortg_lm)

x2pp_ortg_stdres <- rstandard(x2pp_ortg_lm)
x2pp_ortg_points <- 1:length(x2pp_ortg_stdres)
x2pp_ortg_labels <- if_else(abs(x2pp_ortg_stdres) >= 1.5, paste(x2pp_ortg_points), "")

ggplot(data = NULL, aes(x = x2pp_ortg_points, y = x2pp_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x2pp_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

x2pp_ortg_hats <- hatvalues(x2pp_ortg_lm)

ggplot(data = NULL, aes(x = x2pp_ortg_points, y = x2pp_ortg_hats)) +
  geom_point()

x2pp_ortg_cook <- cooks.distance(x2pp_ortg_lm)

ggplot(data = NULL, aes(x = x2pp_ortg_points, y = x2pp_ortg_cook))+
  geom_point()

car::durbinWatsonTest(x2pp_ortg_lm)

x2pp_ortg_res <- residuals(x2pp_ortg_lm)
x2pp_ortg_fitted <- predict(x2pp_ortg_lm)

ggplot(data = NULL, aes(x = x2pp_ortg_fitted, y = x2pp_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = x2pp_ortg_res)) +
  stat_qq() + stat_qq_line()


#Free Throw Percentage v Wins

ftp_w <- comb_team %>%
  ggplot(aes(FTp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Weak to no linear relationship. Free Throw efficiency not a big determining factor in Wins, can afford to bypass this analysis

ftp_w

cor(x = comb_team$FTp, y = comb_team$W, method = "pearson")

ftp_w_lm <- lm(W ~ FTp, data = comb_team)
summary(ftp_w_lm)

predict(ftp_w_lm)

ftp_w_stdres <- rstandard(ftp_w_lm)
ftp_w_points <- 1:length(ftp_w_stdres)
ftp_w_labels <- if_else(abs(ftp_w_stdres) >= 1.5, paste(ftp_w_points), "")

ggplot(data = NULL, aes(x = ftp_w_points, y = ftp_w_stdres)) +
  geom_point() +
  geom_text(aes(label = ftp_w_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

ftp_w_hats <- hatvalues(ftp_w_lm)

ggplot(data = NULL, aes(x = ftp_w_points, y = ftp_w_hats)) +
  geom_point()

ftp_w_cook <- cooks.distance(ftp_w_lm)

ggplot(data = NULL, aes(x = ftp_w_points, y = ftp_w_cook))+
  geom_point()

car::durbinWatsonTest(ftp_w_lm)

ftp_w_res <- residuals(ftp_w_lm)
ftp_w_fitted <- predict(ftp_w_lm)

ggplot(data = NULL, aes(x = ftp_w_fitted, y = ftp_w_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = ftp_w_res)) +
  stat_qq() + stat_qq_line()


#Free Throw Percentage v Offensive Rating

ft_ortg <- comb_team %>%
  ggplot(aes(FTp, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

ft_ortg

cor(x = comb_team$FTp, y = comb_team$ORtg, method = "pearson")

ft_ortg_lm <- lm(ORtg ~ FTp, data = comb_team)
summary(ft_ortg_lm)

predict(ft_ortg_lm)

ft_ortg_stdres <- rstandard(ft_ortg_lm)
ft_ortg_points <- 1:length(ft_ortg_stdres)
ft_ortg_labels <- if_else(abs(ft_ortg_stdres) >= 1.5, paste(ft_ortg_points), "")

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = ft_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

ft_ortg_hats <- hatvalues(ft_ortg_lm)

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_hats)) +
  geom_point()

ft_ortg_cook <- cooks.distance(ft_ortg_lm)

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_cook))+
  geom_point()

car::durbinWatsonTest(ft_ortg_lm)

ft_ortg_res <- residuals(ft_ortg_lm)
ft_ortg_fitted <- predict(ft_ortg_lm)

ggplot(data = NULL, aes(x = ft_ortg_fitted, y = ft_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = ft_ortg_res)) +
  stat_qq() + stat_qq_line()


#Offensive Rebounds v Offensive Rating

orb_ortg <- comb_team %>%
  ggplot(aes(ORB, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # No linear relationship

orb_ortg

cor(x = comb_team$ORB, y = comb_team$ORtg, method = "pearson")

orb_ortg_lm <- lm(ORtg ~ ORB, data = comb_team)
summary(orb_ortg_lm)

predict(orb_ortg_lm)

orb_ortg_stdres <- rstandard(orb_ortg_lm)
orb_ortg_points <- 1:length(orb_ortg_stdres)
orb_ortg_labels <- if_else(abs(orb_ortg_stdres) >= 1.5, paste(orb_ortg_points), "")

ggplot(data = NULL, aes(x = orb_ortg_points, y = orb_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = orb_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

orb_ortg_hats <- hatvalues(orb_ortg_lm)

ggplot(data = NULL, aes(x = orb_ortg_points, y = orb_ortg_hats)) +
  geom_point()

orb_ortg_cook <- cooks.distance(orb_ortg_lm)

ggplot(data = NULL, aes(x = orb_ortg_points, y = orb_ortg_cook))+
  geom_point()

car::durbinWatsonTest(orb_ortg_lm)

orb_ortg_res <- residuals(orb_ortg_lm)
orb_ortg_fitted <- predict(orb_ortg_lm)

ggplot(data = NULL, aes(x = orb_ortg_fitted, y = orb_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = orb_ortg_res)) +
  stat_qq() + stat_qq_line()


#Defensive Rebounds v Defensive Rating

drb_drtg <- comb_team %>%
  ggplot(aes(DRB, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

drb_drtg

cor(x = comb_team$DRB, y = comb_team$DRtg, method = "pearson")

drb_drtg_lm <- lm(DRtg ~ DRB, data = comb_team)
summary(drb_drtg_lm)

predict(drb_drtg_lm)

drb_drtg_stdres <- rstandard(drb_drtg_lm)
drb_drtg_points <- 1:length(drb_drtg_stdres)
drb_drtg_labels <- if_else(abs(drb_drtg_stdres) >= 1.5, paste(drb_drtg_points), "")

ggplot(data = NULL, aes(x = drb_drtg_points, y = drb_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = drb_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

drb_drtg_hats <- hatvalues(drb_drtg_lm)

ggplot(data = NULL, aes(x = drb_drtg_points, y = drb_drtg_hats)) +
  geom_point()

drb_drtg_cook <- cooks.distance(drb_drtg_lm)

ggplot(data = NULL, aes(x = drb_drtg_points, y = drb_drtg_cook))+
  geom_point()

car::durbinWatsonTest(drb_drtg_lm)

drb_drtg_res <- residuals(drb_drtg_lm)
drb_drtg_fitted <- predict(drb_drtg_lm)

ggplot(data = NULL, aes(x = drb_drtg_fitted, y = drb_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = drb_drtg_res)) +
  stat_qq() + stat_qq_line()


#Steals v Defensive Rating

stl_drtg <- comb_team %>%
  ggplot(aes(STL, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

stl_drtg

cor(x = comb_team$STL, y = comb_team$DRtg, method = "pearson")

stl_drtg_lm <- lm(DRtg ~ STL, data = comb_team)
summary(stl_drtg_lm)

predict(stl_drtg_lm)

stl_drtg_stdres <- rstandard(stl_drtg_lm)
stl_drtg_points <- 1:length(stl_drtg_stdres)
stl_drtg_labels <- if_else(abs(stl_drtg_stdres) >= 1.5, paste(stl_drtg_points), "")

ggplot(data = NULL, aes(x = stl_drtg_points, y = stl_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = stl_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

stl_drtg_hats <- hatvalues(stl_drtg_lm)

ggplot(data = NULL, aes(x = stl_drtg_points, y = stl_drtg_hats)) +
  geom_point()

stl_drtg_cook <- cooks.distance(stl_drtg_lm)

ggplot(data = NULL, aes(x = stl_drtg_points, y = stl_drtg_cook))+
  geom_point()

car::durbinWatsonTest(stl_drtg_lm)

stl_drtg_res <- residuals(stl_drtg_lm)
stl_drtg_fitted <- predict(stl_drtg_lm)

ggplot(data = NULL, aes(x = stl_drtg_fitted, y = stl_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = stl_drtg_res)) +
  stat_qq() + stat_qq_line()


#Steals v Offensive Rating

stl_ortg <- comb_team %>%
  ggplot(aes(STL, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # No linear relationship

stl_ortg

cor(x = comb_team$STL, y = comb_team$ORtg, method = "pearson")

stl_ortg_lm <- lm(ORtg ~ STL, data = comb_team)
summary(stl_ortg_lm)

predict(stl_ortg_lm)

stl_ortg_stdres <- rstandard(stl_ortg_lm)
stl_ortg_points <- 1:length(stl_ortg_stdres)
stl_ortg_labels <- if_else(abs(stl_ortg_stdres) >= 1.5, paste(stl_ortg_points), "")

ggplot(data = NULL, aes(x = stl_ortg_points, y = stl_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = stl_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

stl_ortg_hats <- hatvalues(stl_ortg_lm)

ggplot(data = NULL, aes(x = stl_ortg_points, y = stl_ortg_hats)) +
  geom_point()

stl_ortg_cook <- cooks.distance(stl_ortg_lm)

ggplot(data = NULL, aes(x = stl_ortg_points, y = stl_ortg_cook))+
  geom_point()

car::durbinWatsonTest(stl_ortg_lm)

stl_ortg_res <- residuals(stl_ortg_lm)
stl_ortg_fitted <- predict(stl_ortg_lm)

ggplot(data = NULL, aes(x = stl_ortg_fitted, y = stl_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = stl_ortg_res)) +
  stat_qq() + stat_qq_line()


#Assists v Offensive Rating

ast_ortg <- comb_team %>%
  ggplot(aes(AST, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Strong linear relationship. Assuming Houston would be the team with the high rating, low Assist level (James Harden effect)

ast_ortg

cor(x = comb_team$AST, y = comb_team$ORtg, method = "pearson")

ast_ortg_lm <- lm(ORtg ~ AST, data = comb_team)
summary(ast_ortg_lm)

predict(ast_ortg_lm)

ast_ortg_stdres <- rstandard(ast_ortg_lm)

ast_ortg_stdres

ast_ortg_points <- 1:length(ast_ortg_stdres)
ast_ortg_labels <- if_else(abs(ast_ortg_stdres) >= 1.5, paste(ast_ortg_points), "")

ggplot(data = NULL, aes(x = ast_ortg_points, y = ast_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = ast_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Point 11 has a std res value of just over 2.8, may be considered an outlier or a high leverage high influence point. 

ast_ortg_hats <- hatvalues(ast_ortg_lm)

ggplot(data = NULL, aes(x = ast_ortg_points, y = ast_ortg_hats)) +
  geom_point()

ast_ortg_cook <- cooks.distance(ast_ortg_lm)

ggplot(data = NULL, aes(x = ast_ortg_points, y = ast_ortg_cook))+
  geom_point()

car::durbinWatsonTest(ast_ortg_lm)

ast_ortg_res <- residuals(ast_ortg_lm)
ast_ortg_fitted <- predict(ast_ortg_lm)

ggplot(data = NULL, aes(x = ast_ortg_fitted, y = ast_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = ast_ortg_res)) +
  stat_qq() + stat_qq_line()


*Blocks v Defensive Rating*
blk_drtg <- comb_team %>%
  ggplot(aes(BLK, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Moderate linear relationship

blk_drtg

cor(x = comb_team$BLK, y = comb_team$DRtg, method = "pearson")

blk_drtg_lm <- lm(DRtg ~ BLK, data = comb_team)
summary(blk_drtg_lm)

predict(blk_drtg_lm)

blk_drtg_stdres <- rstandard(blk_drtg_lm)
blk_drtg_points <- 1:length(blk_drtg_stdres)
blk_drtg_labels <- if_else(abs(blk_drtg_stdres) >= 1.5, paste(blk_drtg_points), "")

ggplot(data = NULL, aes(x = blk_drtg_points, y = blk_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = blk_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

blk_drtg_hats <- hatvalues(blk_drtg_lm)

ggplot(data = NULL, aes(x = blk_drtg_points, y = blk_drtg_hats)) +
  geom_point()

blk_drtg_cook <- cooks.distance(blk_drtg_lm)

ggplot(data = NULL, aes(x = blk_drtg_points, y = blk_drtg_cook))+
  geom_point()

car::durbinWatsonTest(blk_drtg_lm)

blk_drtg_res <- residuals(blk_drtg_lm)
blk_drtg_fitted <- predict(blk_drtg_lm)

ggplot(data = NULL, aes(x = blk_drtg_fitted, y = blk_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

ggplot(data = NULL, aes(sample = blk_drtg_res)) +
  stat_qq() + stat_qq_line()


## Z-score for Team Points and breakdown of points

comb_team %>%
  mutate(z_pts = round((PTS - mean(PTS)) / sd(PTS)),
         pts_per_game = PTS / G,
         z_x3p = round((x3P - mean(x3P)) / sd(x3P))) %>%
  ggplot() +
  stat_qq(aes(sample = pts_per_game)) +
  facet_wrap(~ z_x3p)

comb_team %>%
  mutate(z_pts = round((PTS - mean(PTS)) / sd(PTS)),
         pts_per_game = PTS / G) %>%
  ggplot() +
  stat_qq(aes(sample = pts_per_game)) +
  facet_wrap(~ z_pts)  


## Five Summary Statistics
comb_team %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg))


comb_team %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg))


comb_team %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))


comb_team %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg))


comb_team %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg))


comb_team %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg))


comb_team %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg))

## Position Specific Data Frames
#New Data Frames for Specific Position

centres <- subset(p_stats, Pos == "C")

sg <- subset(p_stats, Pos == "SG")

sf <- subset(p_stats, Pos == "SF")

pf <- subset(p_stats, Pos == "PF")

pg <- subset(p_stats, Pos == "PG")


## Regression Lines
#Point Guard

reg_pg_drb_ppg <- p_stats %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_drb_ppg


reg_pg_orb_ppg <- p_stats %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_orb_ppg


reg_pg_stl_ppg <- p_stats %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_stl_ppg


reg_pg_ast_ppg <- p_stats %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_ast_ppg


reg_pg_x3p_ppg <- p_stats %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_x3p_ppg


reg_pg_x2p_ppg <- p_stats %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_x2p_ppg


reg_pg_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pg_ft_ppg


#Shooting Guard
  
reg_sg_drb_ppg <- p_stats %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_drb_ppg


reg_sg_orb_ppg <- p_stats %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_orb_ppg


reg_sg_stl_ppg <- p_stats %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_stl_ppg


reg_sg_ast_ppg <- p_stats %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_ast_ppg


reg_sg_x3p_ppg <- p_stats %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_x3p_ppg


reg_sg_x2p_ppg <- p_stats %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_x2p_ppg


reg_sg_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sg_ft_ppg


#Small Forward
  

reg_sf_drb_ppg <- p_stats %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_drb_ppg


reg_sf_orb_ppg <- p_stats %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_orb_ppg


reg_sf_stl_ppg <- p_stats %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_stl_ppg


reg_sf_ast_ppg <- p_stats %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_ast_ppg


reg_sf_x3p_ppg <- p_stats %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_x3p_ppg


reg_sf_x2p_ppg <- p_stats %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_x2p_ppg


reg_sf_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_sf_ft_ppg


#Power Forward
  
reg_pf_drb_ppg <- p_stats %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_drb_ppg


reg_pf_orb_ppg <- p_stats %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_orb_ppg


reg_pf_stl_ppg <- p_stats %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_stl_ppg



reg_pf_ast_ppg <- p_stats %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_ast_ppg



reg_pf_x3p_ppg <- p_stats %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_x3p_ppg



reg_pf_x2p_ppg <- p_stats %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_x2p_ppg



reg_pf_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_pf_ft_ppg


#Centres
  

reg_c_drb_ppg <- p_stats %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_drb_ppg


reg_c_orb_ppg <- p_stats %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_orb_ppg


reg_c_stl_ppg <- p_stats %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_stl_ppg


reg_c_ast_ppg <- p_stats %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_ast_ppg



reg_c_x3p_ppg <- p_stats %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_x3p_ppg



reg_c_x2p_ppg <- p_stats %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_x2p_ppg


reg_c_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5)

reg_c_ft_ppg


## Position specific five summary statistics
#Point Guard
  

sum_stats_pg_drb_ppg <- pg %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg))


sum_stats_pg_orb_ppg <- pg %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg))


sum_stats_pg_stl_ppg <- pg %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg))


sum_stats_pg_ast_ppg <- pg %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg))


sum_stats_pg_x3p_ppg <- pg %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg))


sum_stats_pg_x2p_ppg <- pg %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg))


sum_stats_pg_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))


#Shooting Guard
  
sum_stats_sg_drb_ppg <- sg %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg))


sum_stats_sg_orb_ppg <- sg %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg))


sum_stats_sg_stl_ppg <- sg %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg))


sum_stats_sg_ast_ppg <- sg %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg))


sum_stats_sg_x3p_ppg <- sg %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg))


sum_stats_sg_x2p_ppg <- sg %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg))


sum_stats_sg_ft_ppg <- sg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))


#Small Forward

sum_stats_sf_drb_ppg <- sf %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg))



sum_stats_sf_orb_ppg <- sf %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg))


sum_stats_sf_stl_ppg <- sf %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg))


sum_stats_sf_ast_ppg <- sf %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg))


sum_stats_sf_x3p_ppg <- sf %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg))


sum_stats_sf_x2p_ppg <- sf %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg))


sum_stats_sf_ft_ppg <- sf %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))


#Power Forward
  

sum_stats_pf_drb_ppg <- pf %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg))



sum_stats_pf_orb_ppg <- pf %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg))


sum_stats_pf_stl_ppg <- pf %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg))


sum_stats_pf_ast_ppg <- pf %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg))


sum_stats_pf_x3p_ppg <- pf %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg))


sum_stats_pf_x2p_ppg <- pf %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg))


sum_stats_pf_ft_ppg <- pf %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))


#Centres
  
sum_stats_c_drb_ppg <- centres %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg))



sum_stats_c_orb_ppg <- centres %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg))



sum_stats_c_stl_ppg <- centres %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg))



sum_stats_c_ast_ppg <- centres %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg))



sum_stats_c_x3p_ppg <- centres %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg))



sum_stats_c_x2p_ppg <- centres %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg))


sum_stats_c_ft_ppg <- centres %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))


# Regression Lines

#Point Guards

regline_pg_drb_ppg <- sum_stats_pg_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb)

reg_pg_drb_ppg +
  geom_abline(intercept = regline_pg_drb_ppg$intercept, slope = regline_pg_drb_ppg$slope)


regline_pg_orb_ppg <- sum_stats_pg_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb)

reg_pg_orb_ppg +
  geom_abline(intercept = regline_pg_orb_ppg$intercept, slope = regline_pg_orb_ppg$slope)

regline_pg_stl_ppg <- sum_stats_pg_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl)

reg_pg_stl_ppg +
  geom_abline(intercept = regline_pg_stl_ppg$intercept, slope = regline_pg_stl_ppg$slope)

regline_pg_ast_ppg <- sum_stats_pg_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast)

reg_pg_ast_ppg +
  geom_abline(intercept = regline_pg_ast_ppg$intercept, slope = regline_pg_ast_ppg$slope)

regline_pg_x3p_ppg <- sum_stats_pg_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p)

reg_pg_x3p_ppg +
  geom_abline(intercept = regline_pg_x3p_ppg$intercept, slope = regline_pg_x3p_ppg$slope)

regline_pg_x2p_ppg <- sum_stats_pg_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p)

reg_pg_x2p_ppg +
  geom_abline(intercept = regline_pg_x2p_ppg$intercept, slope = regline_pg_x2p_ppg$slope)


regline_pg_ft_ppg <- sum_stats_pg_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft)

reg_pg_ft_ppg +
  geom_abline(intercept = regline_pg_ft_ppg$intercept, slope = regline_pg_ft_ppg$slope)


#Shooting Guard
  
regline_sg_drb_ppg <- sum_stats_sg_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb)

reg_sg_drb_ppg +
  geom_abline(intercept = regline_sg_drb_ppg$intercept, slope = regline_sg_drb_ppg$slope)


regline_sg_orb_ppg <- sum_stats_sg_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb)

reg_sg_orb_ppg +
  geom_abline(intercept = regline_sg_orb_ppg$intercept, slope = regline_sg_orb_ppg$slope)


regline_sg_stl_ppg <- sum_stats_sg_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl)

reg_sg_stl_ppg +
  geom_abline(intercept = regline_sg_stl_ppg$intercept, slope = regline_sg_stl_ppg$slope)


regline_sg_ast_ppg <- sum_stats_sg_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast)

reg_sg_ast_ppg +
  geom_abline(intercept = regline_sg_ast_ppg$intercept, slope = regline_sg_ast_ppg$slope)


regline_sg_x3p_ppg <- sum_stats_sg_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p)

reg_sg_x3p_ppg +
  geom_abline(intercept = regline_sg_x3p_ppg$intercept, slope = regline_sg_x3p_ppg$slope)


regline_sg_x2p_ppg <- sum_stats_sg_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p)

reg_sg_x2p_ppg +
  geom_abline(intercept = regline_sg_x2p_ppg$intercept, slope = regline_sg_x2p_ppg$slope)


regline_sg_ft_ppg <- sum_stats_sg_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft)

reg_sg_ft_ppg +
  geom_abline(intercept = regline_sg_ft_ppg$intercept, slope = regline_sg_ft_ppg$slope)


#Small Forwards
  

regline_sf_drb_ppg <- sum_stats_sf_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb)

reg_sf_drb_ppg +
  geom_abline(intercept = regline_sf_drb_ppg$intercept, slope = regline_sf_drb_ppg$slope)


regline_sf_orb_ppg <- sum_stats_sf_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb)

reg_sf_orb_ppg +
  geom_abline(intercept = regline_sf_orb_ppg$intercept, slope = regline_sf_orb_ppg$slope)


regline_sf_stl_ppg <- sum_stats_sf_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl)

reg_sf_stl_ppg +
  geom_abline(intercept = regline_sf_stl_ppg$intercept, slope = regline_sf_stl_ppg$slope)


regline_sf_ast_ppg <- sum_stats_sf_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast)

reg_sf_ast_ppg +
  geom_abline(intercept = regline_sf_ast_ppg$intercept, slope = regline_sf_ast_ppg$slope)


regline_sf_x3p_ppg <- sum_stats_sf_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p)

reg_sf_x3p_ppg +
  geom_abline(intercept = regline_sf_x3p_ppg$intercept, slope = regline_sf_x3p_ppg$slope)


regline_sf_x2p_ppg <- sum_stats_sf_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p)

reg_sf_x2p_ppg +
  geom_abline(intercept = regline_sf_x2p_ppg$intercept, slope = regline_sf_x2p_ppg$slope)


regline_sf_ft_ppg <- sum_stats_sf_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft)

reg_sf_ft_ppg +
  geom_abline(intercept = regline_sf_ft_ppg$intercept, slope = regline_sf_ft_ppg$slope)


#Centres
  

regline_c_drb_ppg <- sum_stats_c_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb)

reg_c_drb_ppg +
  geom_abline(intercept = regline_c_drb_ppg$intercept, slope = regline_c_drb_ppg$slope)


regline_c_orb_ppg <- sum_stats_c_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb)

reg_c_orb_ppg +
  geom_abline(intercept = regline_c_orb_ppg$intercept, slope = regline_c_orb_ppg$slope)


regline_c_stl_ppg <- sum_stats_c_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl)

reg_c_stl_ppg +
  geom_abline(intercept = regline_c_stl_ppg$intercept, slope = regline_c_stl_ppg$slope)


regline_c_ast_ppg <- sum_stats_c_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast)

reg_c_ast_ppg +
  geom_abline(intercept = regline_c_ast_ppg$intercept, slope = regline_c_ast_ppg$slope)


regline_c_x3p_ppg <- sum_stats_c_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p)

reg_c_x3p_ppg +
  geom_abline(intercept = regline_c_x3p_ppg$intercept, slope = regline_c_x3p_ppg$slope)

regline_c_x2p_ppg <- sum_stats_c_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p)

reg_c_x2p_ppg +
  geom_abline(intercept = regline_c_x2p_ppg$intercept, slope = regline_c_x2p_ppg$slope)

regline_c_ft_ppg <- sum_stats_c_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft)

reg_c_ft_ppg +
  geom_abline(intercept = regline_c_ft_ppg$intercept, slope = regline_c_ft_ppg$slope)
















## Linear Relationship for Points 

#Points = 3-Point FG + 2-Point FG + Free Throw

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


## Linear Modelling For PLayers

#Combined Players

overall_tidy_combined <- lm(PTSpm ~ MP + x3P + x2P + FT, data = p_stats)

tidy(overall_tidy_combined, conf.int = TRUE)

overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = p_stats)

tidy(overall_tidy_ppg, conf.int = TRUE)

overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = p_stats)

tidy(overall_tidy_pts, conf.int = TRUE)


#Point Guard

pg_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = pg)

tidy(pg_overall_tidy, conf.int = TRUE)

pg_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = pg)

tidy(pg_overall_tidy_ppg, conf.int = TRUE)

pg_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = pg)

tidy(pg_overall_tidy_pts, conf.int = TRUE)


#Shooting Guard

sg_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = sg)

tidy(sg_overall_tidy, conf.int = TRUE)

sg_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = sg)

tidy(sg_overall_tidy_ppg, conf.int = TRUE)

sg_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = sg)

tidy(sg_overall_tidy_pts, conf.int = TRUE)


#Small Forward

sf_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = sf)

tidy(sf_overall_tidy, conf.int = TRUE)

sf_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = sf)

tidy(sf_overall_tidy_ppg, conf.int = TRUE)

sf_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = sf)

tidy(sf_overall_tidy_pts, conf.int = TRUE)


#Power Forward

pf_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = pf)

tidy(pf_overall_tidy, conf.int = TRUE)

pf_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = pf)

tidy(pf_overall_tidy_ppg, conf.int = TRUE)

pf_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = pf)

tidy(pf_overall_tidy_pts, conf.int = TRUE)


#Centres

c_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = centres)

tidy(c_overall_tidy, conf.int = TRUE)

c_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = centres)

tidy(c_overall_tidy_ppg, conf.int = TRUE)

c_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = centres)

tidy(c_overall_tidy_pts, conf.int = TRUE)



## Player Points Predictions by Position
#Point Guard

pg_ppg_pts_predict <- pg %>%
  filter(PPG >= 10) %>%
  mutate(pg_r_hat = predict(pg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pg_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()


#Shooting Guard
  
sg_ppg_pts_predict <- sg %>%
  filter(PPG >= 10) %>%
  mutate(sg_r_hat = predict(sg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sg_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()


#Small Forward

sf_ppg_pts_predict <- sf %>%
  filter(PPG >= 10) %>%
  mutate(sf_r_hat = predict(sf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sf_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()


#Power Forward

pf_ppg_pts_predict <- pf %>%
  filter(PPG >= 10) %>%
  mutate(pf_r_hat = predict(pf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pf_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()


#Centres

c_ppg_pts_predict <- centres %>%
  filter(PPG >= 10) %>%
  mutate(c_r_hat = predict(c_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(c_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth()


# Adding Salary values

#Point Guard

pg_pts_hat <- pg %>%
  mutate(pg_hat = predict(pg_overall_tidy_ppg, newdata = .))

pg_salary <- pg_pts_hat %>%
  ggplot(aes(Salary, pg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1, cex = 2)


pg_salary # Gives the predicted points average against the players salary, to see where the value is. We select D'Angelo Russell at Point Guard. 


#Shooting Guard

sg_pts_hat <- sg %>%
  mutate(sg_hat = predict(sg_overall_tidy_ppg, newdata = .))

sg_salary <- sg_pts_hat %>%
  ggplot(aes(Salary, sg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


sg_salary # Gives the predicted points average against the players salary, to see where the value is. We select Donovan Mitchell at Shooting Guard. 


#Small Forward

sf_pts_hat <- sf %>%
  mutate(sf_hat = predict(sf_overall_tidy_ppg, newdata = .))

sf_salary <- sf_pts_hat %>%
  ggplot(aes(Salary, sf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


sf_salary # Gives the predicted points average against the players salary, to see where the value is. Depending on salary space, we either pick Kevin Durant or Kawhi Leonard at Small Forward as they stand out from the pack of lower priced players.


#Power Forward

pf_pts_hat <- pf %>%
  mutate(pf_hat = predict(pf_overall_tidy_ppg, newdata = .))

pf_salary <- pf_pts_hat %>%
  ggplot(aes(Salary, pf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


pf_salary # Gives the predicted points average against the players salary, to see where the value is. We select either Julius Randle or Tobias Harris at the Power Forward position. 


#Centres

c_pts_hat <- centres %>%
  mutate(c_hat = predict(c_overall_tidy_ppg, newdata = .))

c_salary <- c_pts_hat %>%
  ggplot(aes(Salary, c_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2)


c_salary # Gives the predicted points average against the players salary, to see where the value is. We select Karl-Anthony Towns at Centre. 


## Selected Team

selected_player_pg <- p_stats %>%
  filter(player_name == "D'Angelo Russell") # Filter for the chosen PG

selected_player_sg <- p_stats %>%
  filter(player_name == "Donovan Mitchell") # Filter for the chosen SG

selected_player_sf <- p_stats %>%
  filter(player_name == "Kevin Durant") # Filter for the chosen SF

selected_player_pf <- p_stats %>%
  filter(player_name == "Tobias Harris") # Filter for the chosen PF

selected_player_c <- p_stats %>%
  filter(player_name == "Karl-Anthony Towns") # Filter for the chosen C

selected_team <- bind_rows(selected_player_c, selected_player_pf, selected_player_sf, selected_player_sg, selected_player_pg) # Combine above filtered players and combine to express the chosen side. 

selected_team