
## Overview 

## This assessment task allows you to consolidate and apply the concepts and skills you've learnt throughout the semester. This assessment requires you to generate a **reproducible data analysis project.** 

## Your reproducible data analysis project will be hosted as a repository on GitHub and you are required to submit the URL to your GitHub repository.

## Scenario and Aim of the Data Analysis Project

## You are a data analyst with the Chicago Bulls competing in the NBA (national basketball association). In the most recent NBA season (2018-19), your team placed 27th out of 30 (for win-loss record). Your team's budget for player contracts next season  is $118 million, ranked 26th out of 30 (for the purpose of this assignment, next season is 2019-20). For context, the team with the highest payroll budget is Portland with $148 million, while the best performing team was Milwaukee Bucks (who clinched the best league record in 2018-19 [who clinched the best league record in 2018-29](https://www.espn.com/nba/standings/_/season/2019/group/league)) with $131 million. 

## You have been tasked by the general manager of Chicago Bulls to find the best five starting players [one from each position](https://en.wikipedia.org/wiki/Basketball_positions)) your team can afford. (Make sure you don't use up all of your money on just these five players, you still need to fill a full team roster, but are just focussed on finding five starting players here). You can choose players that are already playing for Chicago Bulls, you just need to prove that they are worth it.

## Load required packages
# include all required packages at the start
library(tidyverse) 
library(prettydoc)
library(broom)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(magrittr)
library(naniar)
library(stringi)
library(tidyr)
library(tinytex)


## Data source

##We have been provided the following data sets:
##1. 2018-19_nba_player-statistics.csv :  sourced from [basketball-reference.com](https://www.basketball-reference.com/leagues/NBA_2019_totals.html)

##2. 2018-19_nba_player-salaries.csv :  sourced from [hoopshype.com/salaries](https://hoopshype.com/salaries/)

##3. 2019-20_nba_team-payroll.csv : sourced from [hoopshype.com/salaries](https://hoopshype.com/salaries/)

##4. 2018-19_nba_team-statistics_1.csv : sourced from [basketball-reference.com](https://www.basketball-reference.com/leagues/NBA_2019.html)

##5. 2018-19_nba_team-statistics_2.csv : sourced from [basketball-reference.com](https://www.basketball-reference.com/leagues/NBA_2019.html)

## Set Working Directory
setwd("/Users/josh/NBA/nba-repo-project/repos/proj")

## Read Data (Correct)
##Read in the various files using the `read_csv()` function from the `readr` package. 
p_stats <- read_csv("data/raw/2018-19_nba_player_statistics.csv")
p_sal <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
team_stats <- read_csv("data/raw/2018-19_nba_team_statistics_1.csv")
team_stats_2 <- read_csv("data/raw/2018-19_nba_team_statistics_2.csv")
payroll <- read_csv("data/raw/2019-20_nba_team-payroll.csv")



## Tidying Data Process (Correct) 

##Rename the variables.
#Rename the variables to remove % and variables starting with numbers
p_stats <- rename(p_stats,
                  FGp = 'FG%', x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', x2PA = '2PA', x2Pp = '2P%', eFGp = 'eFG%', FTp = 'FT%') 
team_stats <- rename(team_stats,
                     x3PAr = '3PAr', TSp = 'TS%', eFGp = 'eFG%', TOVp = 'TOV%', ORBp = 'ORB%', DRBp = 'DRB%')
team_stats_2 <- rename(team_stats_2,
                       FGp = 'FG%', x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', x2PA = '2PA', x2Pp = '2P%', FTp = 'FT%')


## Dealing with NAs
#Replace the NAs found in shooting percentage of players who didn't attempt a particular shot
p_stats <- p_stats %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
#Remove the NA columns added at the end of the Salary and Team Stats variables

p_sal <- p_sal[,-(4:7), drop = FALSE] # Removed columns 4 through to 7 that displayed no data
team_stats <- team_stats[, -(23:25), drop = FALSE] # Removed columns 23 through to 25 that displayed no data


## Joining data frames
p_sal <- subset(p_sal, select = c(2:3)) # First remove player id as not required data

p_stats <- left_join(x = p_stats, y = p_sal) # This will join the salary to the respective Player

p_stats <- drop_na(p_stats) # We can see that some Player salaries are missing, so we must remove these from our dataset to ensure we are 100% confident that our picks will keep us under the salary cap. 

which(is.na(p_stats), arr.ind = TRUE) # Quick test to identify if any NAs remain in the dataset


## Removing accents and periods 
x <- stringi::stri_trans_general(p_stats$player_name, "Latin-ASCII")
# Saves the player name list in new variable. Removes the accents and saves as a character vector

x_new <- as.data.frame(x, row.names = NULL, optional = FALSE, stringsAsFactors = FALSE) # Converts the vector into a single column data frame

x_new <- stringr::str_replace_all(x_new$x, pattern = "\\.", replacement = "")
# Removes the periodsin players names.

x_new <- as.data.frame(x, row.names = NULL, optional = FALSE, stringsAsFactors = FALSE) # Converts the vector into a single column data frame

x_new <- rename(x_new, player_name = 'x') # Changes column name to merge data frames


## Combining Player Salary and Statistics
p_stats <- bind_cols(x = x_new, y = p_stats) # Combines the no accent name variable to the main data frame. 

p_stats <- subset(p_stats, select = -c(player_name1)) # Removes the player name variable that had the accents.

p_stats <- p_stats[, c(1,2,3,4,30,5:29)] # Move the salary variable into a more logical position in the table

p_stats <- rename(p_stats, Salary = 'salary') # Rename salary to Salary, to tidy up the appearance slightly.

## Combining Team Statistics
comb_team <- merge(team_stats, team_stats_2, by.x = "Team", by.y = "Team") # Combine the two sheets, matching by the Team names as the order of the two sheets is different.

comb_team <- subset(comb_team, select = -c(2, 23)) # This will remove the 'Ranking' columns that appeared twice. They aren't necessary in this analysis, so have been removed.

comb_team <- comb_team[, c(1, 2, 22:23, 3:21, 24:44)] # The next three pieces of code, will reorganise the data into an order that is preferable for me. 

comb_team <- comb_team[, c(1:15, 36:44, 16:35)]

comb_team <- comb_team[, c(1:24, 33:44, 25:32)]


## Removing of duplicate athletes, due to trades and filtering data
p_stats <- p_stats %>%
  group_by(player_name) %>%
  arrange(player_name, desc(G)) %>%
  distinct(player_name, .keep_all = TRUE) # Will remove any duplicate players based on the amount of games played in that row. The highest amount of games played for the duplicated remains. Team variable 'TOT' stands for Two or More Teams so that is the row we want to keep.

p_stats <- p_stats %>%
  filter(G >= 20, MP >= 100) # Filter out and remove players that haven't played enough and who's data could influence decisions unnecessarily. 


## Transforming the Data 
#### New variables
  
p_stats <- p_stats %>%
  group_by(Pos) %>%
  mutate(PTSpm = PTS / MP,
         FTpm = FT / MP,
         BLKpm = BLK / MP,
         ASTpm = AST / MP,
         STLpm = STL / MP,
         TOVpm = TOV / MP,
         x3Ppm = x3P / MP,
         PPG = PTS / G,
         APG = AST / G,
         RPG = TRB / G) # Creates new variables at the end of our data frame

p_stats <- arrange(p_stats, Pos) # Arranges the data frame in order of Position

p_stats <- p_stats %>%
  mutate_if(is.numeric, round, digits = 3)

kable(cbind(p_stats)) %>%
  kable_styling("bordered") %>%
  scroll_box(width = "100%", height = "200px") # Formatting the table with a scroll box so that it doesn't take up a considerable amount of the page. 

