# Data being pulled from The Lahmen Database

library(Lahman)
library(readr)
library(tidyverse)

#Get information for the last X World Series champions
num_champs <- 16

Season <- c((max(Batting$yearID) - num_champs + 1):max(SeriesPost$yearID))
Winner <- NULL
for(i in Season){
      temp <- SeriesPost %>% 
            filter(yearID == i) %>% 
            filter(round == 'WS')
      Winner <- c(Winner, as.character(temp$teamIDwinner))
}
TeamId <- paste(Winner,Season,sep = "_")

#Remove factors from teamID and lgID field in Batting data
Batting <- as.data.frame(Batting, stringsAsFactors=FALSE) %>% 
      filter(Batting$yearID >= min(Season))

Pitching <- as.data.frame(Pitching, stringsAsFactors=FALSE) %>% 
      filter(Pitching$yearID >= min(Season))

#WS Champs data frame
Champs <- data.frame(TeamId,Season,Winner, stringsAsFactors = FALSE)

battingStats <- NULL
pitchingStats <- NULL

#Loop to create the 16 teams
for (i in seq_along(Champs$TeamId)){
      temp <- Batting %>% 
            filter(yearID == Champs$Season[i]) %>% 
            filter(teamID == Champs$Winner[i]) %>% 
            arrange(-AB) %>% 
            top_n(wt = AB, 9)
      
      battingStats[[i]] <- temp
      
      temp <- Pitching %>% 
            filter(yearID == Champs$Season[i]) %>% 
            filter(teamID == Champs$Winner[i]) %>% 
            arrange(-GS) %>% 
            top_n(wt = GS, 4)
      
      pitchingStats[[i]] <- temp
}







# the 2017 world series featured the los angeles dodgers and the houston astros

# first we need to obtain data about the players

astros_hitting <- read_csv("astros_reg_season_hitting.csv")
astros_pitching <- read_csv("astros_reg_season_pitching.csv")
dodgers_hitting <- read_csv("dodgers_reg_season_hitting.csv")
dodgers_pitching <- read_csv("dodgers_reg_season_hitting.csv")

# we will assume that the 9 hitters with the most regular season at bats are in the starting lineup
astros_hitting <- astros_hitting %>% 
      arrange(-AB) %>% 
      top_n(wt = AB, 9) %>%
      mutate(team = "astros")

dodgers_hitting <- dodgers_hitting %>% 
      arrange(-AB) %>% 
      top_n(wt = AB, 9) %>%
      mutate(team = "dodgers")

# calculate variables for simulation
hitting <- rbind(astros_hitting, dodgers_hitting) %>%
      mutate(on_base_count = H + BB,
             ob_pct = on_base_count/AB,
             on_base_1B = (on_base_count - `2B` - `3B` - `HR`),
             on_base_2B = (on_base_count - on_base_1B - `3B` - `HR`),
             on_base_3B = (on_base_count - on_base_1B - `2B` - `HR`),
             on_base_HR = (on_base_count - on_base_1B - `2B` - `3B`),
             obc = on_base_count) %>%
      mutate_at(vars(starts_with("on_base")), funs(. / obc))

astros_hitting <- hitting %>% filter(team == "astros")
dodgers_hitting <- hitting %>% filter(team == "dodgers")

#----------------------------
# build helper functions
#----------------------------

# initialize vector of possible on-base states
on_base_states <- c(1, 2, 3, 4)

# simulate half inning of a game
half_inning <- function(team_df) {
      outs <- 0
      runs <- 0
      i <- lineup_pos
      base_outcome <- NULL
      # set up base runner states
      b1 <- 0
      b2 <- 0
      b3 <- 0
      hitter <- 0
      # while loop to cycle through the batting order until 3 outs are reached
      while(outs < 3){
            b3 <- b2
            b2 <- b1
            b1 <- hitter
            hitter <- 0
            i <- ifelse(i < 8, i + 1, 1)
            batter <- team_df[i,]
            at_bat_outcome <- rbinom(1, 1, batter$ob_pct)
            # if the at bat results in getting on base, how many bases?
            if(at_bat_outcome == 1){
                  base_outcome <- sample(on_base_states, 1, replace = T, prob = c(batter$on_base_1B,
                                                                                  batter$on_base_2B,
                                                                                  batter$on_base_3B,
                                                                                  batter$on_base_HR))
                  # set field state based on base_outcome
                  hitter <- base_outcome
                  b1 <- b1 + base_outcome
                  b2 <- b2 + base_outcome
                  b3 <- b3 + base_outcome
                  # increment runs if any players reached home (total of 4 or greater)
                  runs <- ifelse(hitter > 3, runs + 1, runs)
                  runs <- ifelse(b1 > 3, runs + 1, runs)
                  runs <- ifelse(b2 > 3, runs + 1, runs)
                  runs <- ifelse(b3 > 3, runs + 1, runs)
                  # reset positions to 0 if a player reached home
                  hitter <- ifelse(hitter > 3, 0, hitter)
                  b1 <- ifelse(b1 > 3, 0, b1)
                  b2 <- ifelse(b2 > 3, 0, b2)
                  b3 <- ifelse(b3 > 3, 0, b3)
            } else {
                  outs <- outs + 1
            }
      }
      lineup_pos <- i
      return(runs)
}

lineup_pos <- 0
half_inning(astros_hitting)

# function to simulate an entire game
game_sim <- function(away_team_df, home_team_df){
      winner <- NULL
      lineup_pos <- 0
      extra_innings <- 0
      # simulate away team score through 9 innings
      away_score <- 0
      for(i in 1:9){
            away_score <- away_score + half_inning(away_team_df)
      }
      away_lineup_pos <- lineup_pos
      lineup_pos <- 0
      # simulate home team score through 8 innings
      home_score <- 0
      for(i in 1:8){
            home_score <- home_score + half_inning(home_team_df)
      }
      # check if bottom of the 9th is played
      if(home_score > away_score){
            winner <- "Home Team"
      } else {
            home_score <- home_score + half_inning(home_team_df)
      }
      # need to handle ties (extra innings)
      if(home_score > away_score){
            winner <- "Home Team"
      } else if(away_score > home_score){
            winner <- "Away Team"
      } else {
            tied <- TRUE
            while(tied == TRUE){
                  away_score <- away_score + half_inning(away_team_df)
                  home_score <- home_score + half_inning(home_team_df)
                  extra_innings <- extra_innings + 1
                  tied <- ifelse(away_score == home_score, TRUE, FALSE)
            }
            winner <- ifelse(home_score > away_score, "Home Team", "Away Team")
      }
      total_innings <- 9 + extra_innings
      game_info <- data.frame(cbind(away_score, home_score, winner, total_innings, extra_innings))
      return(game_info)
}

# simulate one game
game_sim(astros_hitting, dodgers_hitting)

# what is the probability that the astros beat the dodgers in a single game if they are the away team?
outcomes <- NULL
for(i in 1:100){
      outcomes <- rbind(outcomes, game_sim(astros_hitting, dodgers_hitting))
}

outcomes %>% group_by(winner) %>% summarize(n = n(), pct = n/100)

# Result: Away Team (Astros) win ~45.8% of the time...

#-----------------------------------------------
# Function to simulate home and home round robin
#-----------------------------------------------









