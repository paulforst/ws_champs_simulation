# Data being pulled from The Lahmen Database

library(Lahman)
library(readr)
library(tidyverse)
source("game_sim.R")

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
            mutate(TeamId = paste(teamID,Season,sep = "_")) %>% 
            top_n(wt = AB, 9)
      
      battingStats[[i]] <- temp
      
      temp <- Pitching %>% 
            filter(yearID == Champs$Season[i]) %>% 
            filter(teamID == Champs$Winner[i]) %>% 
            arrange(-GS) %>% 
            mutate(TeamId = paste(teamID,Season,sep = "_")) %>% 
            top_n(wt = GS, 4)
      
      pitchingStats[[i]] <- temp
}


#-----------------------------------------------
# Function to simulate home and home round robin
#-----------------------------------------------

#Select team to be away and play all other teams
for(i in length(Season)){
      #Get away team and all opponents
      away <- TeamId[i]
      opponents <- TeamId[-i]
      
      #Gather away team stats
      away_batting <- as.data.frame(battingStats[i])
      away_pitching <- as.data.frame(pitchingStats[i])
      
      for(j in opponents) {
            #Gather home team stats
            index <- which(TeamId == j)
            home_batting <- as.data.frame(battingStats[index])
            home_pitching <- as.data.frame(pitchingStats[index])
            
            game_sim(away_batting, home_batting, away_pitching, home_pitching)
            
            
            
      }
}







