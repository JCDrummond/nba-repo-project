## Explore various data models
### Single Varible Linear Regression

#Team Statistics
#Pace v Wins

Pace_wins <- comb_team %>%
  ggplot(aes(Pace, W)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Team Pace Ratings and Wins.

Pace_wins # Prints the above graph

cor(x = comb_team$Pace, y = comb_team$W, method = "pearson") # Checks the correlation between the two variables

pace_wins_lm <- lm(W ~ Pace, data = comb_team)
summary(pace_wins_lm) # Creates a linear regression model for Pace v Wins.

predict(pace_wins_lm) # Obtain the predicted/expected values.

pace_wins_stdres <- rstandard(pace_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
pace_wins_points <- 1:length(pace_wins_stdres) # Gives the length of the variable
pace_wins_labels <- if_else(abs(pace_wins_stdres) >= 1.5, paste(pace_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = pace_wins_points, y = pace_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = pace_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified. 

pace_wins_hats <- hatvalues(pace_wins_lm) # Measures the leverage of the points. 

ggplot(data = NULL, aes(x = pace_wins_points, y = pace_wins_hats)) +
  geom_point()  # Shows the leverage on a graph

pace_wins_cook <- cooks.distance(pace_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = pace_wins_points, y = pace_wins_cook))+
  geom_point() # shows the collective change through a scatterplot graph. 

car::durbinWatsonTest(pace_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

pace_wins_res <- residuals(pace_wins_lm)
pace_wins_fitted <- predict(pace_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values. 

ggplot(data = NULL, aes(x = pace_wins_fitted, y = pace_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = pace_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals.


#Offence rating v Wins

of_wins <- comb_team %>%
  ggplot(aes(ORtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Team Pace Ratings and Wins.

of_wins # Prints the above graph

cor(x = comb_team$ORtg, y = comb_team$W, method = "pearson") # Checks the correlation between the two variables

of_wins_lm <- lm(W ~ ORtg, data = comb_team)
summary(of_wins_lm) # Creates a linear regression model for Offensive Rating v Wins.

predict(of_wins_lm) # Obtain the predicted/expected values.

of_wins_stdres <- rstandard(of_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
of_wins_points <- 1:length(of_wins_stdres) # Gives the length of the variable
of_wins_labels <- if_else(abs(of_wins_stdres) >= 1.5, paste(of_wins_points), "")  # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = of_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified. 

of_wins_hats <- hatvalues(of_wins_lm) # Measures the leverage of the points. 

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_hats)) +
  geom_point() # Shows the leverage on a graph

of_wins_cook <- cooks.distance(of_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = of_wins_points, y = of_wins_cook))+
  geom_point()# shows the collective change through a scatterplot graph. 

car::durbinWatsonTest(of_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

of_wins_res <- residuals(of_wins_lm)
of_wins_fitted <- predict(of_wins_lm)# Testing for homoscedasticity, and whether they have a constant variance across all x values. 

ggplot(data = NULL, aes(x = of_wins_fitted, y = of_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = of_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals


#Defence rating v Wins

def_wins <- comb_team %>%
  ggplot(aes(DRtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Defensive Rating and Wins. Important to note that the negative linear relationship is actually reversed, as a higher defensive rating is not a good outcome for the team. A higher win rate and lower defensive rating is the ideal outcome.

def_wins # Prints the above graph

cor(x = comb_team$DRtg, y = comb_team$W, method = "pearson") # Checks the correlation

def_wins_lm <- lm(W ~ DRtg, data = comb_team)
summary(def_wins_lm) # Creates a linear regression model for Defensive Rating v Wins

predict(def_wins_lm) # Obtain the predicted/expected values

def_wins_stdres <- rstandard(def_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
def_wins_points <- 1:length(def_wins_stdres) # Gives the length of the variable
def_wins_labels <- if_else(abs(def_wins_stdres) >= 1.5, paste(def_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = def_wins_points, y = def_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = def_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified. 

def_wins_hats <- hatvalues(def_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = def_wins_points, y = def_wins_hats)) +
  geom_point() # Shows the leverage on a graph

def_wins_cook <- cooks.distance(def_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = def_wins_points, y = def_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(def_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

def_wins_res <- residuals(def_wins_lm)
def_wins_fitted <- predict(def_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values. 

ggplot(data = NULL, aes(x = def_wins_fitted, y = def_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = def_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals.


#3-Point Attempt Rating v Wins

x3PAr_wins <- comb_team %>%
  ggplot(aes(x3PAr, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between 3-Point Attempt Rate and Wins. 

x3PAr_wins # Prints the above graph

cor(x = comb_team$x3PAr, y = comb_team$W, method = "pearson") # checks the correlation

x3PAr_wins_lm <- lm(W ~ x3PAr, data = comb_team)
summary(x3PAr_wins_lm) # Creates a linear regression model.

predict(x3PAr_wins_lm) # Obtain the predicted/expected values

x3PAr_wins_stdres <- rstandard(x3PAr_wins_lm) # Gives the length of the variable
x3PAr_wins_points <- 1:length(x3PAr_wins_stdres) # Gives the length of the variable
x3PAr_wins_labels <- if_else(abs(x3PAr_wins_stdres) >= 1.5, paste(x3PAr_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x3PAr_wins_points, y = x3PAr_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = x3PAr_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified. 

x3PAr_wins_hats <- hatvalues(x3PAr_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x3PAr_wins_points, y = x3PAr_wins_hats)) +
  geom_point()# Shows the leverage

x3PAr_wins_cook <- cooks.distance(x3PAr_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x3PAr_wins_points, y = x3PAr_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x3PAr_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x3PAr_wins_res <- residuals(x3PAr_wins_lm)
x3PAr_wins_fitted <- predict(x3PAr_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x3PAr_wins_fitted, y = x3PAr_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x3PAr_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#True Shooting Percentage v Wins

TSP_wins <- comb_team %>%
  ggplot(aes(TSp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between True Shooting Percentage and Wins. 

TSP_wins # Prints the above graph

cor(x = comb_team$TSp, y = comb_team$W, method = "pearson") # checks the correlation


TSP_wins_lm <- lm(W ~ TSp, data = comb_team)
summary(TSP_wins_lm) # Creates a linear regression model.


predict(TSP_wins_lm) # Obtain the predicted/expected values

TSP_wins_stdres <- rstandard(TSP_wins_lm) # Gives the length of the variable
TSP_wins_points <- 1:length(TSP_wins_stdres) # Gives the length of the variable
TSP_wins_labels <- if_else(abs(TSP_wins_stdres) >= 1.5, paste(TSP_wins_points), "")# Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = TSP_wins_points, y = TSP_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = TSP_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")# Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

TSP_wins_hats <- hatvalues(TSP_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = TSP_wins_points, y = TSP_wins_hats)) +
  geom_point() # Shows the leverage

TSP_wins_cook <- cooks.distance(TSP_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = TSP_wins_points, y = TSP_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(TSP_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

TSP_wins_res <- residuals(TSP_wins_lm)
TSP_wins_fitted <- predict(TSP_wins_lm)# Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = TSP_wins_fitted, y = TSP_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = TSP_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Effective Field Goal Percentage

eFGp_wins <- comb_team %>%
  ggplot(aes(eFGp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta")  # Testing for a linear relationship between Effective Field Goal Percentage and Wins. 

eFGp_wins # Prints the above graph

cor(x = comb_team$eFGp, y = comb_team$W, method = "pearson")# checks the correlation

eFGp_wins_lm <- lm(W ~ eFGp, data = comb_team)
summary(eFGp_wins_lm) # Creates a linear regression model.

predict(eFGp_wins_lm) # Obtain the predicted/expected values

eFGp_wins_stdres <- rstandard(eFGp_wins_lm) # Gives the length of the variable
eFGp_wins_points <- 1:length(eFGp_wins_stdres) # Gives the length of the variable
eFGp_wins_labels <- if_else(abs(eFGp_wins_stdres) >= 1.5, paste(eFGp_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = eFGp_wins_points, y = eFGp_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = eFGp_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

eFGp_wins_hats <- hatvalues(eFGp_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = eFGp_wins_points, y = eFGp_wins_hats)) +
  geom_point() # Shows the leverage

eFGp_wins_cook <- cooks.distance(eFGp_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = eFGp_wins_points, y = eFGp_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(eFGp_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

eFGp_wins_res <- residuals(eFGp_wins_lm)
eFGp_wins_fitted <- predict(eFGp_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = eFGp_wins_fitted, y = eFGp_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = eFGp_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Defensive Rebound Percentage v Lower Defensive Rating

defrb_drtg <- comb_team %>%
  ggplot(aes(DRBp, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Defensive Rebound Percentage and the Defensive Rating. 

defrb_drtg # Prints the above graph

cor(x = comb_team$DRBp, y = comb_team$DRtg, method = "pearson") # checks the correlation

defrb_drtg_lm <- lm(DRtg ~ DRBp, data = comb_team)
summary(defrb_drtg_lm) # Creates a linear regression model.

predict(defrb_drtg_lm) # Obtain the predicted/expected values

defrb_drtg_stdres <- rstandard(defrb_drtg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
defrb_drtg_points <- 1:length(defrb_drtg_stdres) # Gives the length of the variable
defrb_drtg_labels <- if_else(abs(defrb_drtg_stdres) >= 1.5, paste(defrb_drtg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = defrb_drtg_points, y = defrb_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = defrb_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")  # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.
## Has an outlier at point number 6, need to explore this to see it's impact on the data. Doesn't appear to have a high leverage or high influence.

defrb_drtg_hats <- hatvalues(defrb_drtg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = defrb_drtg_points, y = defrb_drtg_hats)) +
  geom_point() # Shows the leverage

defrb_drtg_cook <- cooks.distance(defrb_drtg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = defrb_drtg_points, y = defrb_drtg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(defrb_drtg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

defrb_drtg_res <- residuals(defrb_drtg_lm)
defrb_drtg_fitted <- predict(defrb_drtg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = defrb_drtg_fitted, y = defrb_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = defrb_drtg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Turnover Totals contributing to Losses
  
tov_loss <- comb_team %>%
  ggplot(aes(TOV, L)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Turnover Totals and Losses

tov_loss # Prints the above graph

cor(x = comb_team$TOV, y = comb_team$L, method = "pearson") # checks the correlation

tov_loss_lm <- lm(L ~ TOV, data = comb_team)
summary(tov_loss_lm) # Creates a linear regression model.

predict(tov_loss_lm) # Obtain the predicted/expected values

tov_loss_stdres <- rstandard(tov_loss_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
tov_loss_points <- 1:length(tov_loss_stdres) # Gives the length of the variable
tov_loss_labels <- if_else(abs(tov_loss_stdres) >= 1.5, paste(tov_loss_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = tov_loss_points, y = tov_loss_stdres)) +
  geom_point() +
  geom_text(aes(label = tov_loss_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

tov_loss_hats <- hatvalues(tov_loss_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = tov_loss_points, y = tov_loss_hats)) +
  geom_point() # Shows the leverage

tov_loss_cook <- cooks.distance(tov_loss_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = tov_loss_points, y = tov_loss_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(tov_loss_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

tov_loss_res <- residuals(tov_loss_lm)
tov_loss_fitted <- predict(tov_loss_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = tov_loss_fitted, y = tov_loss_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = tov_loss_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Net Rating v Wins
  
net_win <- comb_team %>%
  ggplot(aes(NRtg, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Team Net Rating and Wins

net_win # Prints the above graph

cor(x = comb_team$NRtg, y = comb_team$W, method = "pearson") # checks the correlation

net_wins_lm <- lm(W ~ NRtg, data = comb_team)
summary(net_wins_lm) # Creates a linear regression model.

predict(net_wins_lm) # Obtain the predicted/expected values

net_wins_stdres <- rstandard(net_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
net_wins_points <- 1:length(net_wins_stdres) # Gives the length of the variable
net_wins_labels <- if_else(abs(net_wins_stdres) >= 1.5, paste(net_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = net_wins_points, y = net_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = net_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

net_wins_hats <- hatvalues(net_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = net_wins_points, y = net_wins_hats)) +
  geom_point() # Shows the leverage

net_wins_cook <- cooks.distance(net_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = net_wins_points, y = net_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(net_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

net_wins_res <- residuals(net_wins_lm)
net_wins_fitted <- predict(net_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = net_wins_fitted, y = net_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = net_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Total Points contributing to Offensive Rating*

pts_off <- comb_team %>%
  ggplot(aes(PTS, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Points and Wins

pts_off # Prints the above graph

cor(x = comb_team$PTS, y = comb_team$W, method = "pearson") # checks the correlation

pts_wins_lm <- lm(W ~ PTS, data = comb_team)
summary(pts_wins_lm) # Creates a linear regression model.

predict(pts_wins_lm) # Obtain the predicted/expected values

pts_wins_stdres <- rstandard(pts_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
pts_wins_points <- 1:length(pts_wins_stdres) # Gives the length of the variable
pts_wins_labels <- if_else(abs(pts_wins_stdres) >= 1.5, paste(pts_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = pts_wins_points, y = pts_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = pts_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

pts_wins_hats <- hatvalues(pts_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = pts_wins_points, y = net_wins_hats)) +
  geom_point() # Shows the leverage

pts_wins_cook <- cooks.distance(pts_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = pts_wins_points, y = pts_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(pts_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

pts_wins_res <- residuals(pts_wins_lm)
pts_wins_fitted <- predict(pts_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = pts_wins_fitted, y = pts_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = pts_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Three points v Wins
  
x3_wins <- comb_team %>%
  ggplot(aes(x3P, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between successful 3 Point attempts and Wins

x3_wins # Prints the above graph

cor(x = comb_team$x3P, y = comb_team$W, method = "pearson") # checks the correlation

x3_wins_lm <- lm(W ~ x3P, data = comb_team)
summary(x3_wins_lm) # Creates a linear regression model.

predict(x3_wins_lm) # Obtain the predicted/expected values

x3_wins_stdres <- rstandard(x3_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x3_wins_points <- 1:length(x3_wins_stdres) # Gives the length of the variable
x3_wins_labels <- if_else(abs(x3_wins_stdres) >= 1.5, paste(x3_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x3_wins_points, y = x3_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = x3_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x3_wins_hats <- hatvalues(x3_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x3_wins_points, y = x3_wins_hats)) +
  geom_point() # Shows the leverage

x3_wins_cook <- cooks.distance(x3_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x3_wins_points, y = x3_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x3_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x3_wins_res <- residuals(x3_wins_lm)
x3_wins_fitted <- predict(x3_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x3_wins_fitted, y = x3_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x3_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Two points v Wins
  
x2_wins <- comb_team %>%
  ggplot(aes(x2P, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between successful Two Point Attempts and Wins

x2_wins # Prints the above graph

cor(x = comb_team$x2P, y = comb_team$W, method = "pearson") # checks the correlation

x2_wins_lm <- lm(W ~ x2P, data = comb_team)
summary(x2_wins_lm) # Creates a linear regression model.

predict(x2_wins_lm) # Obtain the predicted/expected values

x2_wins_stdres <- rstandard(x2_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x2_wins_points <- 1:length(x2_wins_stdres) # Gives the length of the variable
x2_wins_labels <- if_else(abs(x2_wins_stdres) >= 1.5, paste(x2_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x2_wins_points, y = x2_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = x2_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x2_wins_hats <- hatvalues(x2_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x2_wins_points, y = x2_wins_hats)) +
  geom_point() # Shows the leverage

x2_wins_cook <- cooks.distance(x2_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x2_wins_points, y = x2_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x2_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x2_wins_res <- residuals(x2_wins_lm)
x2_wins_fitted <- predict(x2_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x2_wins_fitted, y = x2_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x2_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Free Throws v wins
  
ft_wins <- comb_team %>%
  ggplot(aes(FT, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Successful Free Throw Attempts and Wins 

ft_wins # Prints the above graph

cor(x = comb_team$FT, y = comb_team$W, method = "pearson") # checks the correlation

ft_wins_lm <- lm(W ~ FT, data = comb_team)
summary(ft_wins_lm) # Creates a linear regression model.

predict(ft_wins_lm) # Obtain the predicted/expected values

ft_wins_stdres <- rstandard(ft_wins_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
ft_wins_points <- 1:length(ft_wins_stdres) # Gives the length of the variable
ft_wins_labels <- if_else(abs(ft_wins_stdres) >= 1.5, paste(ft_wins_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = ft_wins_points, y = ft_wins_stdres)) +
  geom_point() +
  geom_text(aes(label = ft_wins_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

ft_wins_hats <- hatvalues(ft_wins_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = ft_wins_points, y = ft_wins_hats)) +
  geom_point() # Shows the leverage

ft_wins_cook <- cooks.distance(ft_wins_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = ft_wins_points, y = ft_wins_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(ft_wins_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

ft_wins_res <- residuals(ft_wins_lm)
ft_wins_fitted <- predict(ft_wins_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = ft_wins_fitted, y = ft_wins_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = ft_wins_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Three Points v Offensive Rating

x3p_ortg <- comb_team %>%
  ggplot(aes(x3P, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between successful 3 Point Attempts and Offensive Rating

x3p_ortg # Prints the above graph

cor(x = comb_team$x3P, y = comb_team$ORtg, method = "pearson") # checks the correlation

x3p_ortg_lm <- lm(ORtg ~ x3P, data = comb_team)
summary(x3p_ortg_lm) # Creates a linear regression model.

predict(x3p_ortg_lm) # Obtain the predicted/expected values

x3p_ortg_stdres <- rstandard(x3p_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x3p_ortg_points <- 1:length(x3p_ortg_stdres) # Gives the length of the variable
x3p_ortg_labels <- if_else(abs(x3p_ortg_stdres) >= 1.5, paste(x3p_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x3p_ortg_points, y = x3p_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x3p_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x3p_ortg_hats <- hatvalues(x3p_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x3p_ortg_points, y = x3p_ortg_hats)) +
  geom_point() # Shows the leverage

x3p_ortg_cook <- cooks.distance(x3p_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x3p_ortg_points, y = x3p_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x3p_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x3p_ortg_res <- residuals(x3p_ortg_lm)
x3p_ortg_fitted <- predict(x3p_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x3p_ortg_fitted, y = x3p_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x3p_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Two Points v Offensive Rating

x2p_ortg <- comb_team %>%
  ggplot(aes(x2P, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between successful 2 Point Attempts and Offensive Rating

x2p_ortg # Prints the above graph

cor(x = comb_team$x2P, y = comb_team$ORtg, method = "pearson") # checks the correlation

x2p_ortg_lm <- lm(ORtg ~ x2P, data = comb_team)
summary(x2p_ortg_lm) # Creates a linear regression model.

predict(x2p_ortg_lm) # Obtain the predicted/expected values

x2p_ortg_stdres <- rstandard(x2p_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x2p_ortg_points <- 1:length(x2p_ortg_stdres) # Gives the length of the variable
x2p_ortg_labels <- if_else(abs(x2p_ortg_stdres) >= 1.5, paste(x2p_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x2p_ortg_points, y = x2p_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x2p_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x2p_ortg_hats <- hatvalues(x2p_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x2p_ortg_points, y = x2p_ortg_hats)) +
  geom_point() # Shows the leverage

x2p_ortg_cook <- cooks.distance(x2p_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x2p_ortg_points, y = x2p_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x2p_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x2p_ortg_res <- residuals(x2p_ortg_lm)
x2p_ortg_fitted <- predict(x2p_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x2p_ortg_fitted, y = x2p_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x2p_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Free Throws v Offensive Rating

ft_ortg <- comb_team %>%
  ggplot(aes(FT, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between successful Free Throws and Offensive Rating

ft_ortg # Prints the above graph

cor(x = comb_team$FT, y = comb_team$ORtg, method = "pearson") # checks the correlation

ft_ortg_lm <- lm(ORtg ~ FT, data = comb_team)
summary(ft_ortg_lm) # Creates a linear regression model.

predict(ft_ortg_lm) # Obtain the predicted/expected values

ft_ortg_stdres <- rstandard(ft_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
ft_ortg_points <- 1:length(ft_ortg_stdres) # Gives the length of the variable
ft_ortg_labels <- if_else(abs(ft_ortg_stdres) >= 1.5, paste(ft_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = ft_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

ft_ortg_hats <- hatvalues(ft_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_hats)) +
  geom_point() # Shows the leverage

ft_ortg_cook <- cooks.distance(ft_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(ft_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

ft_ortg_res <- residuals(ft_ortg_lm)
ft_ortg_fitted <- predict(ft_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = ft_ortg_fitted, y = ft_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = ft_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Three Point Percentage v Wins

x3pp_w <- comb_team %>%
  ggplot(aes(x3Pp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta")  # Testing for a linear relationship between 3 Point Shot Percentage and Wins

x3pp_w # Prints the above graph

cor(x = comb_team$x3Pp, y = comb_team$W, method = "pearson") # checks the correlation

x3pp_w_lm <- lm(W ~ x3Pp, data = comb_team)
summary(x3pp_w_lm) # Creates a linear regression model.

predict(x3pp_w_lm) # Obtain the predicted/expected values

x3pp_w_stdres <- rstandard(x3pp_w_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x3pp_w_points <- 1:length(x3pp_w_stdres) # Gives the length of the variable
x3pp_w_labels <- if_else(abs(x3pp_w_stdres) >= 1.5, paste(x3pp_w_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_stdres)) +
  geom_point() +
  geom_text(aes(label = x3pp_w_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x3pp_w_hats <- hatvalues(x3pp_w_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_hats)) +
  geom_point() # Shows the leverage

x3pp_w_cook <- cooks.distance(x3pp_w_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x3pp_w_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x3pp_w_res <- residuals(x3pp_w_lm)
x3pp_w_fitted <- predict(x3pp_w_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x3pp_w_fitted, y = x3pp_w_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x3pp_w_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Three Point Percentage v Offensive Rating

x3pp_ortg <- comb_team %>%
  ggplot(aes(x3Pp, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between 3 Point Shot Percentage and Offensive Rating

x3pp_ortg # Prints the above graph

cor(x = comb_team$x3Pp, y = comb_team$ORtg, method = "pearson") # checks the correlation

x3pp_ortg_lm <- lm(ORtg ~ x3Pp, data = comb_team)
summary(x3pp_ortg_lm) # Creates a linear regression model.

predict(x3pp_ortg_lm) # Obtain the predicted/expected values

x3pp_ortg_stdres <- rstandard(x3pp_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x3pp_ortg_points <- 1:length(x3pp_ortg_stdres) # Gives the length of the variable
x3pp_ortg_labels <- if_else(abs(x3pp_ortg_stdres) >= 1.5, paste(x3pp_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x3pp_ortg_points, y = x3pp_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x3pp_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x3pp_ortg_hats <- hatvalues(x3pp_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x3pp_ortg_points, y = x3pp_ortg_hats)) +
  geom_point() # Shows the leverage

x3pp_ortg_cook <- cooks.distance(x3pp_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x3pp_ortg_points, y = x3pp_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x3pp_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x3pp_ortg_res <- residuals(x3pp_ortg_lm)
x3pp_ortg_fitted <- predict(x3pp_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x3pp_ortg_fitted, y = x3pp_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x3pp_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Two Point Percentage v Wins

x2pp_w <- comb_team %>%
  ggplot(aes(x2Pp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between 2 Point Shot Percentage and Wins

x2pp_w # Prints the above graph

cor(x = comb_team$x2Pp, y = comb_team$W, method = "pearson") # checks the correlation

x2pp_w_lm <- lm(W ~ x2Pp, data = comb_team)
summary(x2pp_w_lm) # Creates a linear regression model.

predict(x2pp_w_lm) # Obtain the predicted/expected values

x2pp_w_stdres <- rstandard(x2pp_w_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x2pp_w_points <- 1:length(x2pp_w_stdres) # Gives the length of the variable
x2pp_w_labels <- if_else(abs(x2pp_w_stdres) >= 1.5, paste(x2pp_w_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x2pp_w_points, y = x2pp_w_stdres)) +
  geom_point() +
  geom_text(aes(label = x2pp_w_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x3pp_w_hats <- hatvalues(x3pp_w_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_hats)) +
  geom_point() # Shows the leverage

x3pp_w_cook <- cooks.distance(x3pp_w_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x3pp_w_points, y = x3pp_w_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x3pp_w_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x3pp_w_res <- residuals(x3pp_w_lm)
x3pp_w_fitted <- predict(x3pp_w_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x3pp_w_fitted, y = x3pp_w_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x3pp_w_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Two Point Percentage v Offensive Rating

x2pp_ortg <- comb_team %>%
  ggplot(aes(x2Pp, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between 2 Point Shot Percentage and Offensive Rating 

x2pp_ortg # Prints the above graph

cor(x = comb_team$x3Pp, y = comb_team$ORtg, method = "pearson") # checks the correlation

x2pp_ortg_lm <- lm(ORtg ~ x2Pp, data = comb_team)
summary(x2pp_ortg_lm) # Creates a linear regression model.

predict(x2pp_ortg_lm) # Obtain the predicted/expected values

x2pp_ortg_stdres <- rstandard(x2pp_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
x2pp_ortg_points <- 1:length(x2pp_ortg_stdres) # Gives the length of the variable
x2pp_ortg_labels <- if_else(abs(x2pp_ortg_stdres) >= 1.5, paste(x2pp_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = x2pp_ortg_points, y = x2pp_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = x2pp_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

x2pp_ortg_hats <- hatvalues(x2pp_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = x2pp_ortg_points, y = x2pp_ortg_hats)) +
  geom_point() # Shows the leverage

x2pp_ortg_cook <- cooks.distance(x2pp_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = x2pp_ortg_points, y = x2pp_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(x2pp_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

x2pp_ortg_res <- residuals(x2pp_ortg_lm)
x2pp_ortg_fitted <- predict(x2pp_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = x2pp_ortg_fitted, y = x2pp_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = x2pp_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Free Throw Percentage v Wins

ftp_w <- comb_team %>%
  ggplot(aes(FTp, W)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Free Throw Percentage and Wins 

ftp_w # Prints the above graph

cor(x = comb_team$FTp, y = comb_team$W, method = "pearson") # checks the correlation

ftp_w_lm <- lm(W ~ FTp, data = comb_team)
summary(ftp_w_lm) # Creates a linear regression model.

predict(ftp_w_lm) # Obtain the predicted/expected values

ftp_w_stdres <- rstandard(ftp_w_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
ftp_w_points <- 1:length(ftp_w_stdres) # Gives the length of the variable
ftp_w_labels <- if_else(abs(ftp_w_stdres) >= 1.5, paste(ftp_w_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = ftp_w_points, y = ftp_w_stdres)) +
  geom_point() +
  geom_text(aes(label = ftp_w_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

ftp_w_hats <- hatvalues(ftp_w_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = ftp_w_points, y = ftp_w_hats)) +
  geom_point() # Shows the leverage

ftp_w_cook <- cooks.distance(ftp_w_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = ftp_w_points, y = ftp_w_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(ftp_w_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

ftp_w_res <- residuals(ftp_w_lm)
ftp_w_fitted <- predict(ftp_w_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = ftp_w_fitted, y = ftp_w_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = ftp_w_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Free Throw Percentage v Offensive Rating

ft_ortg <- comb_team %>%
  ggplot(aes(FTp, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Free Throw Percentage and Offensive Rating 

ft_ortg # Prints the above graph

cor(x = comb_team$FTp, y = comb_team$ORtg, method = "pearson") # checks the correlation

ft_ortg_lm <- lm(ORtg ~ FTp, data = comb_team)
summary(ft_ortg_lm) # Creates a linear regression model.

predict(ft_ortg_lm) # Obtain the predicted/expected values

ft_ortg_stdres <- rstandard(ft_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
ft_ortg_points <- 1:length(ft_ortg_stdres) # Gives the length of the variable
ft_ortg_labels <- if_else(abs(ft_ortg_stdres) >= 1.5, paste(ft_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = ft_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

ft_ortg_hats <- hatvalues(ft_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_hats)) +
  geom_point() # Shows the leverage

ft_ortg_cook <- cooks.distance(ft_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = ft_ortg_points, y = ft_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(ft_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

ft_ortg_res <- residuals(ft_ortg_lm)
ft_ortg_fitted <- predict(ft_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = ft_ortg_fitted, y = ft_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = ft_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Offensive Rebounds v Offensive Rating

orb_ortg <- comb_team %>%
  ggplot(aes(ORB, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Offensive Rebounds and Offensive Rating 

orb_ortg # Prints the above graph

cor(x = comb_team$ORB, y = comb_team$ORtg, method = "pearson") # checks the correlation

orb_ortg_lm <- lm(ORtg ~ ORB, data = comb_team)
summary(orb_ortg_lm) # Creates a linear regression model.

predict(orb_ortg_lm) # Obtain the predicted/expected values

orb_ortg_stdres <- rstandard(orb_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
orb_ortg_points <- 1:length(orb_ortg_stdres) # Gives the length of the variable
orb_ortg_labels <- if_else(abs(orb_ortg_stdres) >= 1.5, paste(orb_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = orb_ortg_points, y = orb_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = orb_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

orb_ortg_hats <- hatvalues(orb_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = orb_ortg_points, y = orb_ortg_hats)) +
  geom_point() # Shows the leverage

orb_ortg_cook <- cooks.distance(orb_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = orb_ortg_points, y = orb_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(orb_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

orb_ortg_res <- residuals(orb_ortg_lm)
orb_ortg_fitted <- predict(orb_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = orb_ortg_fitted, y = orb_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = orb_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Defensive Rebounds v Defensive Rating

drb_drtg <- comb_team %>%
  ggplot(aes(DRB, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Defensive Rebounds and Defensive Ratings

drb_drtg # Prints the above graph

cor(x = comb_team$DRB, y = comb_team$DRtg, method = "pearson") # checks the correlation

drb_drtg_lm <- lm(DRtg ~ DRB, data = comb_team)
summary(drb_drtg_lm) # Creates a linear regression model.

predict(drb_drtg_lm) # Obtain the predicted/expected values

drb_drtg_stdres <- rstandard(drb_drtg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
drb_drtg_points <- 1:length(drb_drtg_stdres) # Gives the length of the variable
drb_drtg_labels <- if_else(abs(drb_drtg_stdres) >= 1.5, paste(drb_drtg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = drb_drtg_points, y = drb_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = drb_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

drb_drtg_hats <- hatvalues(drb_drtg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = drb_drtg_points, y = drb_drtg_hats)) +
  geom_point() # Shows the leverage

drb_drtg_cook <- cooks.distance(drb_drtg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = drb_drtg_points, y = drb_drtg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(drb_drtg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

drb_drtg_res <- residuals(drb_drtg_lm)
drb_drtg_fitted <- predict(drb_drtg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = drb_drtg_fitted, y = drb_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = drb_drtg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Steals v Defensive Rating

stl_drtg <- comb_team %>%
  ggplot(aes(STL, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Steals and Defensive Ratings

stl_drtg # Prints the above graph

cor(x = comb_team$STL, y = comb_team$DRtg, method = "pearson") # checks the correlation

stl_drtg_lm <- lm(DRtg ~ STL, data = comb_team)
summary(stl_drtg_lm) # Creates a linear regression model.

predict(stl_drtg_lm) # Obtain the predicted/expected values

stl_drtg_stdres <- rstandard(stl_drtg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
stl_drtg_points <- 1:length(stl_drtg_stdres) # Gives the length of the variable
stl_drtg_labels <- if_else(abs(stl_drtg_stdres) >= 1.5, paste(stl_drtg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = stl_drtg_points, y = stl_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = stl_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

stl_drtg_hats <- hatvalues(stl_drtg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = stl_drtg_points, y = stl_drtg_hats)) +
  geom_point() # Shows the leverage

stl_drtg_cook <- cooks.distance(stl_drtg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = stl_drtg_points, y = stl_drtg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(stl_drtg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

stl_drtg_res <- residuals(stl_drtg_lm)
stl_drtg_fitted <- predict(stl_drtg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = stl_drtg_fitted, y = stl_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = stl_drtg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Steals v Offensive Rating

stl_ortg <- comb_team %>%
  ggplot(aes(STL, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Steals and Offensive Rating

stl_ortg # Prints the above graph

cor(x = comb_team$STL, y = comb_team$ORtg, method = "pearson") # checks the correlation

stl_ortg_lm <- lm(ORtg ~ STL, data = comb_team)
summary(stl_ortg_lm) # Creates a linear regression model.

predict(stl_ortg_lm) # Obtain the predicted/expected values

stl_ortg_stdres <- rstandard(stl_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
stl_ortg_points <- 1:length(stl_ortg_stdres) # Gives the length of the variable
stl_ortg_labels <- if_else(abs(stl_ortg_stdres) >= 1.5, paste(stl_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = stl_ortg_points, y = stl_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = stl_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

stl_ortg_hats <- hatvalues(stl_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = stl_ortg_points, y = stl_ortg_hats)) +
  geom_point() # Shows the leverage

stl_ortg_cook <- cooks.distance(stl_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = stl_ortg_points, y = stl_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(stl_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

stl_ortg_res <- residuals(stl_ortg_lm)
stl_ortg_fitted <- predict(stl_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = stl_ortg_fitted, y = stl_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = stl_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Assists v Offensive Rating

ast_ortg <- comb_team %>%
  ggplot(aes(AST, ORtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Assists and Offensive Rating

ast_ortg # Prints the above graph

cor(x = comb_team$AST, y = comb_team$ORtg, method = "pearson") # checks the correlation

ast_ortg_lm <- lm(ORtg ~ AST, data = comb_team)
summary(ast_ortg_lm) # Creates a linear regression model.

predict(ast_ortg_lm) # Obtain the predicted/expected values

ast_ortg_stdres <- rstandard(ast_ortg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
ast_ortg_points <- 1:length(ast_ortg_stdres) # Gives the length of the variable
ast_ortg_labels <- if_else(abs(ast_ortg_stdres) >= 1.5, paste(ast_ortg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = ast_ortg_points, y = ast_ortg_stdres)) +
  geom_point() +
  geom_text(aes(label = ast_ortg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")  # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.
# Point 11 has a std res value of just over 2.8, may be considered an outlier or a high leverage high influence point. 

ast_ortg_hats <- hatvalues(ast_ortg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = ast_ortg_points, y = ast_ortg_hats)) +
  geom_point() # Shows the leverage

ast_ortg_cook <- cooks.distance(ast_ortg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = ast_ortg_points, y = ast_ortg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(ast_ortg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

ast_ortg_res <- residuals(ast_ortg_lm)
ast_ortg_fitted <- predict(ast_ortg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = ast_ortg_fitted, y = ast_ortg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = ast_ortg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


#Blocks v Defensive Rating
blk_drtg <- comb_team %>%
  ggplot(aes(BLK, DRtg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "magenta") # Testing for a linear relationship between Blocks and Defensive Rating

blk_drtg # Prints the above graph

cor(x = comb_team$BLK, y = comb_team$DRtg, method = "pearson") # checks the correlation

blk_drtg_lm <- lm(DRtg ~ BLK, data = comb_team)
summary(blk_drtg_lm) # Creates a linear regression model.

predict(blk_drtg_lm) # Obtain the predicted/expected values

blk_drtg_stdres <- rstandard(blk_drtg_lm) # Calculating the standardised residuals, which is the residual divided by their standard deviation
blk_drtg_points <- 1:length(blk_drtg_stdres) # Gives the length of the variable
blk_drtg_labels <- if_else(abs(blk_drtg_stdres) >= 1.5, paste(blk_drtg_points), "") # Will label the point on the graph if the residual is greater than 1.5 standard deviations.

ggplot(data = NULL, aes(x = blk_drtg_points, y = blk_drtg_stdres)) +
  geom_point() +
  geom_text(aes(label = blk_drtg_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed") # Create a graph with a straight line on the y-axis to show the limits where outliers are classified.

blk_drtg_hats <- hatvalues(blk_drtg_lm) # Measures the leverage of the points

ggplot(data = NULL, aes(x = blk_drtg_points, y = blk_drtg_hats)) +
  geom_point() # Shows the leverage

blk_drtg_cook <- cooks.distance(blk_drtg_lm) # Collective change in the coefficients when the ith point is deleted

ggplot(data = NULL, aes(x = blk_drtg_points, y = blk_drtg_cook))+
  geom_point() # Shows the collective change through a scatterplot graph.

car::durbinWatsonTest(blk_drtg_lm) # We begin to deal with the potential outliers by observing them. The Durbin Watson Test will determine if we have independence of observations. 

blk_drtg_res <- residuals(blk_drtg_lm)
blk_drtg_fitted <- predict(blk_drtg_lm) # Testing for homoscedasticity, and whether they have a constant variance across all x values.

ggplot(data = NULL, aes(x = blk_drtg_fitted, y = blk_drtg_res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") # Graphs the results of homoscedasticity

ggplot(data = NULL, aes(sample = blk_drtg_res)) +
  stat_qq() + stat_qq_line() # Normality of the residuals. 


## Z-score for Team Points and breakdown of points

comb_team %>%
  mutate(z_pts = round((PTS - mean(PTS)) / sd(PTS)),
         pts_per_game = PTS / G,
         z_x3p = round((x3P - mean(x3P)) / sd(x3P))) %>%
  ggplot() +
  stat_qq(aes(sample = pts_per_game)) +
  facet_wrap(~ z_x3p) # The Z-Score is used to standardise and compare individual performances to the mean performances across a group. It allows us to adjust performances based on a given mean and standard deviation. 
# Plotting the z-score on a graph (stat_qq) tests of the normality to ensure there is no evidence of heteroscedasticity. 

comb_team %>%
  mutate(z_pts = round((PTS - mean(PTS)) / sd(PTS)),
         pts_per_game = PTS / G) %>%
  ggplot() +
  stat_qq(aes(sample = pts_per_game)) +
  facet_wrap(~ z_pts)  # The Z-Score is used to standardise and compare individual performances to the mean performances across a group. It allows us to adjust performances based on a given mean and standard deviation. 
# Plotting the z-score on a graph (stat_qq) tests of the normality to ensure there is no evidence of heteroscedasticity. 


## Five Summary Statistics
comb_team %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 3 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the 3 point shots made.


comb_team %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 2 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the 2 point shots made.


comb_team %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Free Throw Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the Free Throw shots made.


comb_team %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Assists Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the Assists made.


comb_team %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Steals Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the Steals made.


comb_team %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Offensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the Offensive Rebounds made.


comb_team %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Defensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a team may make as well as the Defensive Rebounds made.

## Position Specific Data Frames
#New Data Frames for Specific Position

centres <- subset(p_stats, Pos == "C") # Creates a data frame specific for Centres

sg <- subset(p_stats, Pos == "SG") # Creates a data frame specific for Shooting Guards

sf <- subset(p_stats, Pos == "SF") # Creates a data frame specific for Small Forwards

pf <- subset(p_stats, Pos == "PF") # Creates a data frame specific for Power Forwards

pg <- subset(p_stats, Pos == "PG") # Creates a data frame specific for Point Guards

write_csv(x = pg, path = "data/processed/pg.csv") # Saves the new PG data into a new computer file.

write_csv(x = sg, path = "data/processed/sg.csv") # Saves the new SG data into a new computer file.

write_csv(x = sf, path = "data/processed/sf.csv") # Saves the new SF data into a new computer file.

write_csv(x = pf, path = "data/processed/pf.csv") # Saves the new Pf data into a new computer file.

write_csv(x = centres, path = "data/processed/centres.csv") # Saves the new C data into a new computer file.

## Linear Regression
#Point Guard

reg_pg_drb_ppg <- pg %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Defensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Point Guards. 

reg_pg_drb_ppg # Prints the above formula


reg_pg_orb_ppg <- pg %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Offensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Point Guards. 

reg_pg_orb_ppg # Prints the above formula


reg_pg_stl_ppg <- pg %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Steals Per Game' and 'Points Per Game' specific to players listed as Point Guards.  

reg_pg_stl_ppg # Prints the above formula


reg_pg_ast_ppg <- pg %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Assists Per Game' and 'Points Per Game' specific to players listed as Point Guards.  

reg_pg_ast_ppg # Prints the above formula


reg_pg_x3p_ppg <- pg %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '3 Points Made Per Game' and 'Points Per Game' specific to players listed as Point Guards.

reg_pg_x3p_ppg # Prints the above formula


reg_pg_x2p_ppg <- pg %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '2 Points Made Per Game' and 'Points Per Game' specific to players listed as Point Guards.

reg_pg_x2p_ppg # Prints the above formula


reg_pg_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Free Throws Made Per Game' and 'Points Per Game' specific to players listed as Point Guards.

reg_pg_ft_ppg # Prints the above formula


#Shooting Guard
  
reg_sg_drb_ppg <- sg %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Defensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Shooting Guards. 

reg_sg_drb_ppg # Prints the above formula


reg_sg_orb_ppg <- sg %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Offensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Shooting Guards.

reg_sg_orb_ppg # Prints the above formula


reg_sg_stl_ppg <- sg %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Steals Per Game' and 'Points Per Game' specific to players listed as Shooting Guards.

reg_sg_stl_ppg# Prints the above formula


reg_sg_ast_ppg <- sg %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Assists Per Game' and 'Points Per Game' specific to players listed as Shooting Guards.  

reg_sg_ast_ppg # Prints the above formula


reg_sg_x3p_ppg <- sg %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '3 Points Made Per Game' and 'Points Per Game' specific to players listed as Shooting Guards.

reg_sg_x3p_ppg # Prints the above formula


reg_sg_x2p_ppg <- sg %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '2 Points Made Per Game' and 'Points Per Game' specific to players listed as Shooting Guards.

reg_sg_x2p_ppg # Prints the above formula


reg_sg_ft_ppg <- psg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Free Throws Made Per Game' and 'Points Per Game' specific to players listed as Shooting Guards.

reg_sg_ft_ppg # Prints the above formula


#Small Forward
  
reg_sf_drb_ppg <- sf %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Defensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Small Forwards.

reg_sf_drb_ppg # Prints the above formula


reg_sf_orb_ppg <- sf %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Offensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Small Forwards.

reg_sf_orb_ppg # Prints the above formula


reg_sf_stl_ppg <- sf %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Steals Per Game' and 'Points Per Game' specific to players listed as Small Forwards.

reg_sf_stl_ppg # Prints the above formula


reg_sf_ast_ppg <- sf %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Assists Per Game' and 'Points Per Game' specific to players listed as Small Forwards.  

reg_sf_ast_ppg # Prints the above formula


reg_sf_x3p_ppg <- sf %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '3 Points Made Per Game' and 'Points Per Game' specific to players listed as Small Forwards.

reg_sf_x3p_ppg # Prints the above formula


reg_sf_x2p_ppg <- sf %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '2 Points Made Per Game' and 'Points Per Game' specific to players listed as Small Forwards.

reg_sf_x2p_ppg # Prints the above formula


reg_sf_ft_ppg <- sf %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Free Throws Made Per Game' and 'Points Per Game' specific to players listed as Small Forwards.

reg_sf_ft_ppg # Prints the above formula

#Power Forward
  
reg_pf_drb_ppg <- pf %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Defensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Power Forwards.

reg_pf_drb_ppg # Prints the above formula


reg_pf_orb_ppg <- pf %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Offensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Power Forwards.

reg_pf_orb_ppg # Prints the above formula


reg_pf_stl_ppg <- pf %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Steals Per Game' and 'Points Per Game' specific to players listed as Power Forwards.

reg_pf_stl_ppg # Prints the above formula


reg_pf_ast_ppg <- p_f %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Assists Per Game' and 'Points Per Game' specific to players listed as Power Forwards.  

reg_pf_ast_ppg # Prints the above formula


reg_pf_x3p_ppg <- pf %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '3 Points Made Per Game' and 'Points Per Game' specific to players listed as Power Forwards.

reg_pf_x3p_ppg # Prints the above formula


reg_pf_x2p_ppg <- pf %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '2 Points Made Per Game' and 'Points Per Game' specific to players listed as Power Forwards.

reg_pf_x2p_ppg # Prints the above formula


reg_pf_ft_ppg <- pf %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Free Throws Made Per Game' and 'Points Per Game' specific to players listed as Power Forwards.

reg_pf_ft_ppg # Prints the above formula


#Centres
  
reg_c_drb_ppg <- centres %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(drbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Defensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Centres. 

reg_c_drb_ppg # Prints the above formula


reg_c_orb_ppg <- centres %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  ggplot(aes(orbpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Offensive Rebounds Per Game' and 'Points Per Game' specific to players listed as Centres.

reg_c_orb_ppg # Prints the above formula


reg_c_stl_ppg <- centres %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  ggplot(aes(stlpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Steals Per Game' and 'Points Per Game' specific to players listed as Centres.

reg_c_stl_ppg # Prints the above formula


reg_c_ast_ppg <- centres %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  ggplot(aes(astpg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Assists Per Game' and 'Points Per Game' specific to players listed as Centres.  

reg_c_ast_ppg # Prints the above formula


reg_c_x3p_ppg <- centres %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x3ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '3 Points Made Per Game' and 'Points Per Game' specific to players listed as Centres.

reg_c_x3p_ppg # Prints the above formula


reg_c_x2p_ppg <- centres %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  ggplot(aes(x2ppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables '2 Points Made Per Game' and 'Points Per Game' specific to players listed as Centres.

reg_c_x2p_ppg # Prints the above formula


reg_c_ft_ppg <- centres %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  ggplot(aes(ftppg, ptspg)) +
  geom_point(alpha = 0.5) # Prints the relationship between the new variables 'Free Throws Made Per Game' and 'Points Per Game' specific to players listed as Centres.

reg_c_ft_ppg # Prints the above formula

## Position specific five summary statistics
#Point Guard
  
sum_stats_pg_drb_ppg <- pg %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Defensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the Defensive Rebounds made.


sum_stats_pg_orb_ppg <- pg %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Offensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the Offensive Rebounds made.


sum_stats_pg_stl_ppg <- pg %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Steals Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the Steals made.


sum_stats_pg_ast_ppg <- pg %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Assists Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the Assists made.


sum_stats_pg_x3p_ppg <- pg %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 3 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the 3 point shots made.


sum_stats_pg_x2p_ppg <- pg %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 2 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the 2 point shots made.


sum_stats_pg_ft_ppg <- pg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Free Throw Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Point Guard may make as well as the Free Throw shots made.


#Shooting Guard
  
sum_stats_sg_drb_ppg <- sg %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Defensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the Defensive Rebounds made.


sum_stats_sg_orb_ppg <- sg %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Offensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the Offensive Rebounds made.


sum_stats_sg_stl_ppg <- sg %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Steals Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the Steals made.


sum_stats_sg_ast_ppg <- sg %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Assists Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the Assists made.


sum_stats_sg_x3p_ppg <- sg %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 3 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the 3 point shots made.


sum_stats_sg_x2p_ppg <- sg %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 2 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the 2 point shots made.


sum_stats_sg_ft_ppg <- sg %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))# Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Free Throw Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Shooting Guard may make as well as the Free Throw shots made.


#Small Forward

sum_stats_sf_drb_ppg <- sf %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Defensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the Defensive Rebounds made.


sum_stats_sf_orb_ppg <- sf %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Offensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the Offensive Rebounds made.


sum_stats_sf_stl_ppg <- sf %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Steals Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the Steals made.


sum_stats_sf_ast_ppg <- sf %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Assists Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the Assists made.


sum_stats_sf_x3p_ppg <- sf %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 3 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the 3 point shots made.


sum_stats_sf_x2p_ppg <- sf %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 2 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the 2 point shots made.


sum_stats_sf_ft_ppg <- sf %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))# Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Free Throw Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Small Forward may make as well as the Free Throw shots made.


#Power Forward
  

sum_stats_pf_drb_ppg <- pf %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Defensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the Defensive Rebounds made.


sum_stats_pf_orb_ppg <- pf %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Offensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the Offensive Rebounds made.


sum_stats_pf_stl_ppg <- pf %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Steals Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the Steals made.


sum_stats_pf_ast_ppg <- pf %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Assists Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the Assists made.


sum_stats_pf_x3p_ppg <- pf %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 3 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the 3 point shots made.


sum_stats_pf_x2p_ppg <- pf %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 2 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the 2 point shots made.


sum_stats_pf_ft_ppg <- pf %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))# Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Free Throw Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Power Forward may make as well as the Free Throw shots made.


#Centres
  
sum_stats_c_drb_ppg <- centres %>%
  mutate(drbpg = DRB / G,
         ptspg = PTS / G) %>%
  summarize(avg_drb = mean(drbpg),
            s_drb = sd(drbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(drbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Defensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the Defensive Rebounds made.


sum_stats_c_orb_ppg <- centres %>%
  mutate(orbpg = ORB / G,
         ptspg = PTS / G) %>%
  summarize(avg_orb = mean(orbpg),
            s_orb = sd(orbpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(orbpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Offensive Rebounds Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the Offensive Rebounds made.


sum_stats_c_stl_ppg <- centres %>%
  mutate(stlpg = STL / G,
         ptspg = PTS / G) %>%
  summarize(avg_stl = mean(stlpg),
            s_stl = sd(stlpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(stlpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Steals Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the Steals made.


sum_stats_c_ast_ppg <- centres %>%
  mutate(astpg = AST / G,
         ptspg = PTS / G) %>%
  summarize(avg_ast = mean(astpg),
            s_ast = sd(astpg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(astpg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Assists Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the Assists made.


sum_stats_c_x3p_ppg <- centres %>%
  mutate(x3ppg = x3P / G,
         ptspg = PTS / G) %>%
  summarize(avg_3p = mean(x3ppg),
            s_3p = sd(x3ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x3ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 3 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the 3 point shots made.


sum_stats_c_x2p_ppg <- centres %>%
  mutate(x2ppg = x2P / G,
         ptspg = PTS / G) %>%
  summarize(avg_2p = mean(x2ppg),
            s_2p = sd(x2ppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(x2ppg, ptspg)) # Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team 2 Point Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the 2 point shots made.


sum_stats_c_ft_ppg <- centres %>%
  mutate(ftppg = FT / G,
         ptspg = PTS / G) %>%
  summarize(avg_ft = mean(ftppg),
            s_ft = sd(ftppg),
            avg_ptspg = mean(ptspg),
            s_ptspg = sd(ptspg),
            r = cor(ftppg, ptspg))# Creates a table visualising the summation of the five summary statistics. Mean and standard deviation of team Free Throw Makes Per Game and Points Per Game. It gives us the ability to predict the amount of points a Centres may make as well as the Free Throw shots made.

# Regression Lines

#Point Guards

regline_pg_drb_ppg <- sum_stats_pg_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_drb_ppg +
  geom_abline(intercept = regline_pg_drb_ppg$intercept, slope = regline_pg_drb_ppg$slope) # Plots the regression line on the graph


regline_pg_orb_ppg <- sum_stats_pg_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_orb_ppg +
  geom_abline(intercept = regline_pg_orb_ppg$intercept, slope = regline_pg_orb_ppg$slope) # Plots the regression line on the graph

regline_pg_stl_ppg <- sum_stats_pg_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_stl_ppg +
  geom_abline(intercept = regline_pg_stl_ppg$intercept, slope = regline_pg_stl_ppg$slope) # Plots the regression line on the graph

regline_pg_ast_ppg <- sum_stats_pg_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_ast_ppg +
  geom_abline(intercept = regline_pg_ast_ppg$intercept, slope = regline_pg_ast_ppg$slope) # Plots the regression line on the graph

regline_pg_x3p_ppg <- sum_stats_pg_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_x3p_ppg +
  geom_abline(intercept = regline_pg_x3p_ppg$intercept, slope = regline_pg_x3p_ppg$slope) # Plots the regression line on the graph

regline_pg_x2p_ppg <- sum_stats_pg_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_x2p_ppg +
  geom_abline(intercept = regline_pg_x2p_ppg$intercept, slope = regline_pg_x2p_ppg$slope) # Plots the regression line on the graph


regline_pg_ft_ppg <- sum_stats_pg_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft) # Use the previous five summary statistic and this formula to create the regression line

reg_pg_ft_ppg +
  geom_abline(intercept = regline_pg_ft_ppg$intercept, slope = regline_pg_ft_ppg$slope) # Plots the regression line on the graph


#Shooting Guard
  
regline_sg_drb_ppg <- sum_stats_sg_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_drb_ppg +
  geom_abline(intercept = regline_sg_drb_ppg$intercept, slope = regline_sg_drb_ppg$slope) # Plots the regression line on the graph


regline_sg_orb_ppg <- sum_stats_sg_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_orb_ppg +
  geom_abline(intercept = regline_sg_orb_ppg$intercept, slope = regline_sg_orb_ppg$slope) # Plots the regression line on the graph


regline_sg_stl_ppg <- sum_stats_sg_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_stl_ppg +
  geom_abline(intercept = regline_sg_stl_ppg$intercept, slope = regline_sg_stl_ppg$slope) # Plots the regression line on the graph


regline_sg_ast_ppg <- sum_stats_sg_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_ast_ppg +
  geom_abline(intercept = regline_sg_ast_ppg$intercept, slope = regline_sg_ast_ppg$slope) # Plots the regression line on the graph


regline_sg_x3p_ppg <- sum_stats_sg_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_x3p_ppg +
  geom_abline(intercept = regline_sg_x3p_ppg$intercept, slope = regline_sg_x3p_ppg$slope) # Plots the regression line on the graph


regline_sg_x2p_ppg <- sum_stats_sg_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_x2p_ppg +
  geom_abline(intercept = regline_sg_x2p_ppg$intercept, slope = regline_sg_x2p_ppg$slope) # Plots the regression line on the graph


regline_sg_ft_ppg <- sum_stats_sg_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft) # Use the previous five summary statistic and this formula to create the regression line

reg_sg_ft_ppg +
  geom_abline(intercept = regline_sg_ft_ppg$intercept, slope = regline_sg_ft_ppg$slope) # Plots the regression line on the graph


#Small Forwards
  

regline_sf_drb_ppg <- sum_stats_sf_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_drb_ppg +
  geom_abline(intercept = regline_sf_drb_ppg$intercept, slope = regline_sf_drb_ppg$slope) # Plots the regression line on the graph


regline_sf_orb_ppg <- sum_stats_sf_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_orb_ppg +
  geom_abline(intercept = regline_sf_orb_ppg$intercept, slope = regline_sf_orb_ppg$slope) # Plots the regression line on the graph


regline_sf_stl_ppg <- sum_stats_sf_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_stl_ppg +
  geom_abline(intercept = regline_sf_stl_ppg$intercept, slope = regline_sf_stl_ppg$slope) # Plots the regression line on the graph


regline_sf_ast_ppg <- sum_stats_sf_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_ast_ppg +
  geom_abline(intercept = regline_sf_ast_ppg$intercept, slope = regline_sf_ast_ppg$slope) # Plots the regression line on the graph


regline_sf_x3p_ppg <- sum_stats_sf_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_x3p_ppg +
  geom_abline(intercept = regline_sf_x3p_ppg$intercept, slope = regline_sf_x3p_ppg$slope) # Plots the regression line on the graph


regline_sf_x2p_ppg <- sum_stats_sf_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_x2p_ppg +
  geom_abline(intercept = regline_sf_x2p_ppg$intercept, slope = regline_sf_x2p_ppg$slope) # Plots the regression line on the graph


regline_sf_ft_ppg <- sum_stats_sf_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft) # Use the previous five summary statistic and this formula to create the regression line

reg_sf_ft_ppg +
  geom_abline(intercept = regline_sf_ft_ppg$intercept, slope = regline_sf_ft_ppg$slope) # Plots the regression line on the graph


#Centres
  

regline_c_drb_ppg <- sum_stats_c_drb_ppg %>%
  summarise(slope = r * s_ptspg / s_drb,
            intercept = avg_ptspg - slope * avg_drb) # Use the previous five summary statistic and this formula to create the regression line

reg_c_drb_ppg +
  geom_abline(intercept = regline_c_drb_ppg$intercept, slope = regline_c_drb_ppg$slope) # Plots the regression line on the graph


regline_c_orb_ppg <- sum_stats_c_orb_ppg %>%
  summarise(slope = r * s_ptspg / s_orb,
            intercept = avg_ptspg - slope * avg_orb) # Use the previous five summary statistic and this formula to create the regression line

reg_c_orb_ppg +
  geom_abline(intercept = regline_c_orb_ppg$intercept, slope = regline_c_orb_ppg$slope) # Plots the regression line on the graph


regline_c_stl_ppg <- sum_stats_c_stl_ppg %>%
  summarise(slope = r * s_ptspg / s_stl,
            intercept = avg_ptspg - slope * avg_stl) # Use the previous five summary statistic and this formula to create the regression line

reg_c_stl_ppg +
  geom_abline(intercept = regline_c_stl_ppg$intercept, slope = regline_c_stl_ppg$slope) # Plots the regression line on the graph


regline_c_ast_ppg <- sum_stats_c_ast_ppg %>%
  summarise(slope = r * s_ptspg / s_ast,
            intercept = avg_ptspg - slope * avg_ast) # Use the previous five summary statistic and this formula to create the regression line

reg_c_ast_ppg +
  geom_abline(intercept = regline_c_ast_ppg$intercept, slope = regline_c_ast_ppg$slope) # Plots the regression line on the graph


regline_c_x3p_ppg <- sum_stats_c_x3p_ppg %>%
  summarise(slope = r * s_ptspg / s_3p,
            intercept = avg_ptspg - slope * avg_3p) # Use the previous five summary statistic and this formula to create the regression line

reg_c_x3p_ppg +
  geom_abline(intercept = regline_c_x3p_ppg$intercept, slope = regline_c_x3p_ppg$slope) # Plots the regression line on the graph

regline_c_x2p_ppg <- sum_stats_c_x2p_ppg %>%
  summarise(slope = r * s_ptspg / s_2p,
            intercept = avg_ptspg - slope * avg_2p) # Use the previous five summary statistic and this formula to create the regression line

reg_c_x2p_ppg +
  geom_abline(intercept = regline_c_x2p_ppg$intercept, slope = regline_c_x2p_ppg$slope) # Plots the regression line on the graph

regline_c_ft_ppg <- sum_stats_c_ft_ppg %>%
  summarise(slope = r * s_ptspg / s_ft,
            intercept = avg_ptspg - slope * avg_ft) # Use the previous five summary statistic and this formula to create the regression line

reg_c_ft_ppg +
  geom_abline(intercept = regline_c_ft_ppg$intercept, slope = regline_c_ft_ppg$slope) # Plots the regression line on the graph


## Linear Relationship for Points 

#Points = 3-Point FG + 2-Point FG + Free Throw

x3pts_ppg <- ggplot(data = p_stats, aes(x = x3P, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") # 3pt FG makes to total points

x3pts_ppg # Prints the graph with a regression line

x2pts_ppg <- ggplot(data = p_stats, aes(x = x2P, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") # 2pt FG makes to total points

x2pts_ppg # Prints the graph with a regression line


ftpts_ppg <- ggplot(data = p_stats, aes(x = FT, y = PTS)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta") # FT makes to total points

ftpts_ppg # Prints the graph with a regression line


## Linear Modelling For PLayers

#Combined Players

overall_tidy_combined <- lm(PTSpm ~ MP + x3P + x2P + FT, data = p_stats)

tidy(overall_tidy_combined, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points per minute.

overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = p_stats)

tidy(overall_tidy_ppg, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points.

overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = p_stats)

tidy(overall_tidy_pts, conf.int = TRUE) # Values players based on successful actions and their relative contribution to external actions not involved with points. 


#Point Guard

pg_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = pg)

tidy(pg_overall_tidy, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points per minute, specific to Point Guards. 

pg_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = pg)

tidy(pg_overall_tidy_ppg, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points, specific to Point Guards

pg_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = pg)

tidy(pg_overall_tidy_pts, conf.int = TRUE) # Values players based on successful actions and their relative contribution to external actions not involved with points, specific to Point Guards.


#Shooting Guard

sg_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = sg)

tidy(sg_overall_tidy, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points per minute, specific to Shooting Guards.

sg_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = sg)

tidy(sg_overall_tidy_ppg, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points, specific to Shooting Guards.

sg_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = sg)

tidy(sg_overall_tidy_pts, conf.int = TRUE) # Values players based on successful actions and their relative contribution to external actions not involved with points, specific to Shooting Guards.


#Small Forward

sf_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = sf)

tidy(sf_overall_tidy, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points per minute, specific to Small Forwards.

sf_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = sf)

tidy(sf_overall_tidy_ppg, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points, specific to Small Forwards

sf_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = sf)

tidy(sf_overall_tidy_pts, conf.int = TRUE) # Values players based on successful actions and their relative contribution to external actions not involved with points, specific to Small Forwards


#Power Forward

pf_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = pf)

tidy(pf_overall_tidy, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points per minute, specific to Power Forwards.

pf_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = pf)

tidy(pf_overall_tidy_ppg, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points, specific to Power Forwards. 

pf_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = pf)

tidy(pf_overall_tidy_pts, conf.int = TRUE) # Values players based on successful actions and their relative contribution to external actions not involved with points, specific to Power Fowards


#Centres

c_overall_tidy <- lm(PTSpm ~ MP + x3P + x2P + FT, data = centres)

tidy(c_overall_tidy, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points per minute, specific to Centres.

c_overall_tidy_ppg <- lm(PPG ~ G + x3P + x2P + FT, data = centres)

tidy(c_overall_tidy_ppg, conf.int = TRUE) # Values players based on successful actions and their relative contribution to scoring points, specific to Centres.

c_overall_tidy_pts <- lm(PTS ~ AST + DRB + ORB + BLK + TOV, data = centres)

tidy(c_overall_tidy_pts, conf.int = TRUE) # Values players based on successful actions and their relative contribution to external actions not involved with points, specific to Centres


## Player Points Predictions by Position
#Point Guard

pg_ppg_pts_predict <- pg %>%
  filter(PPG >= 10) %>% # This will filter out Point Guards who score under 10 Points Per Game
  mutate(pg_r_hat = predict(pg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pg_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() # This formula will predict each Point Guards predicted Points Per Game against their actual output 

pg_ppg_pts_predict # Prints the output

#Shooting Guard
  
sg_ppg_pts_predict <- sg %>%
  filter(PPG >= 10) %>% # This will filter out Shooting Guards who score under 10 Points Per Game
  mutate(sg_r_hat = predict(sg_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sg_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() # This formula will predict each Shooting Guards predicted Points Per Game against their actual output 

sg_ppg_pts_predict # Prints the output

#Small Forward

sf_ppg_pts_predict <- sf %>%
  filter(PPG >= 10) %>% # This will filter out Small Forwards who score under 10 Points Per Game
  mutate(sf_r_hat = predict(sf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(sf_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() # This formula will predict each Small Forwards predicted Points Per Game against their actual output 

sf_ppg_pts_predict # Prints the output

#Power Forward

pf_ppg_pts_predict <- pf %>%
  filter(PPG >= 10) %>% # This will filter out Power Forwards who score under 10 Points Per Game
  mutate(pf_r_hat = predict(pf_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(pf_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() # This formula will predict each Power Forwards predicted Points Per Game against their actual output 

pf_ppg_pts_predict # Prints the output

#Centres

c_ppg_pts_predict <- centres %>%
  filter(PPG >= 10) %>% # This will filter out Centres who score under 10 Points Per Game
  mutate(c_r_hat = predict(c_overall_tidy_ppg, newdata = .)) %>%
  ggplot(aes(c_r_hat, PTS, label = player_name)) +
  geom_point() +
  geom_text(nudge_x = 2, cex = 2) +
  geom_smooth() # This formula will predict each Centres predicted Points Per Game against their actual output 

c_ppg_pts_predict # Prints the output

# Adding Salary values
#Point Guard

pg_pts_hat <- pg %>%
  mutate(pg_hat = predict(pg_overall_tidy_ppg, newdata = .))

pg_salary <- pg_pts_hat %>%
  ggplot(aes(Salary, pg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1, cex = 2) # Compares and plots individual Player Salaries against their predicted Points Per Game to identify value


pg_salary # Gives the predicted points average against the players salary, to see where the value is. We can select D'Angelo Russell or Kemba Walker at Point Guard. 


#Shooting Guard

sg_pts_hat <- sg %>%
  mutate(sg_hat = predict(sg_overall_tidy_ppg, newdata = .))

sg_salary <- sg_pts_hat %>%
  ggplot(aes(Salary, sg_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) # Compares and plots individual Player Salaries against their predicted Points Per Game to identify value


sg_salary # Gives the predicted points average against the players salary, to see where the value is. We select Donovan Mitchell or Devin Booker at Shooting Guard. 


#Small Forward

sf_pts_hat <- sf %>%
  mutate(sf_hat = predict(sf_overall_tidy_ppg, newdata = .))

sf_salary <- sf_pts_hat %>%
  ggplot(aes(Salary, sf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) # Compares and plots individual Player Salaries against their predicted Points Per Game to identify value


sf_salary # Gives the predicted points average against the players salary, to see where the value is. Depending on salary space, we either pick Kevin Durant or Kawhi Leonard at Small Forward as they stand out from the pack of lower priced players.


#Power Forward

pf_pts_hat <- pf %>%
  mutate(pf_hat = predict(pf_overall_tidy_ppg, newdata = .))

pf_salary <- pf_pts_hat %>%
  ggplot(aes(Salary, pf_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) # Compares and plots individual Player Salaries against their predicted Points Per Game to identify value


pf_salary # Gives the predicted points average against the players salary, to see where the value is. We select either Julius Randle or Giannis Antetokounmpo at the Power Forward position. 


#Centres

c_pts_hat <- centres %>%
  mutate(c_hat = predict(c_overall_tidy_ppg, newdata = .))

c_salary <- c_pts_hat %>%
  ggplot(aes(Salary, c_hat, colour = Pos, label = player_name)) +
  geom_point() +
  geom_text(nudge_y = 1.5, cex = 2) # Compares and plots individual Player Salaries against their predicted Points Per Game to identify value


c_salary # Gives the predicted points average against the players salary, to see where the value is. We select Karl-Anthony Towns at Centre. 


## Selected Team

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


# Final Team Selection

selected_team <- bind_rows(selected_player_c, selected_player_pf_2, selected_player_sf, selected_player_sg, selected_player_pg_2) # Combine above filtered players and combine to express the chosen side. 

selected_team # The table describes the ideal team. Selection based on predicted value and current value based on output per game. 

selected_team