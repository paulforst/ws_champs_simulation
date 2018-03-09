# Data being pulled from The Lahmen Database

library(Lahman)
library(readr)
library(tidyverse)
source("game_sim.R")

#Set global options
options(stringsAsFactors = FALSE)

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

#Remove factors from teamID and lgID field in Batting and Pitching data
Batting <- as.data.frame(Batting, stringsAsFactors = FALSE) %>% 
      filter(Batting$yearID >= min(Season)) %>% 
      mutate(teamID = as.character(teamID))

Pitching <- as.data.frame(Pitching, stringsAsFactors=FALSE) %>% 
      filter(Pitching$yearID >= min(Season)) %>% 
      mutate(teamID = as.character(teamID))

#WS Champs data frame
Champs <- data.frame(TeamId,Season,Winner, stringsAsFactors = FALSE)

#Determine pitching OBP for each year
yearly_obp <- Pitching %>% 
      mutate(obp = ((H + BB + HBP + IBB + SH + SF)/(BFP))) %>% 
      group_by(yearID) %>% 
      summarise(obp_avg = mean(obp))

Pitching <- merge(Pitching, yearly_obp, by = "yearID")

battingStats <- NULL
pitchingStats <- NULL

#Loop to create the 16 teams
for (i in seq_along(Champs$TeamId)){
      temp <- Batting %>% 
            filter(yearID == Champs$Season[i]) %>% 
            filter(teamID == Champs$Winner[i]) %>% 
            arrange(-AB) %>% 
            mutate(TeamId = paste(teamID,yearID,sep = "_")) %>% 
            top_n(wt = AB, 9)
      
      battingStats[[i]] <- temp
      
      temp <- Pitching %>% 
            filter(yearID == Champs$Season[i]) %>% 
            filter(teamID == Champs$Winner[i]) %>% 
            arrange(-GS) %>% 
            mutate(obp = ((H + BB + HBP + IBB + SH + SF)/(BFP)), 
                   TeamId = paste(teamID,yearID,sep = "_"))%>% 
            top_n(wt = GS, 4)
      
      pitchingStats[[i]] <- temp
}


#-----------------------------------------------
# Function to simulate home and home round robin
#-----------------------------------------------

final_results <- NULL

#Select team to be away and play all other teams
for(i in 1:length(Season)){
      #Get away team and all opponents
      away <- TeamId[i]
      opponents <- TeamId[-i]
      
      print(i)
      
      #Gather away team stats
      away_batting <- as.data.frame(battingStats[i])
      away_pitching <- as.data.frame(pitchingStats[i])
      
      team_results <- NULL
      
      for(j in opponents) {
            print(j)
            #Gather home team stats
            index <- which(TeamId == j)
            home_batting <- as.data.frame(battingStats[index])
            home_pitching <- as.data.frame(pitchingStats[index])
            
            temp <- game_sim(away_batting, home_batting)#, away_pitching, home_pitching)
            team_results <- rbind(team_results, temp)
            
            
      }
      
      final_results <- rbind(final_results, team_results)
}







