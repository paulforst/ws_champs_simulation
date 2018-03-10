#############
#Fucntions to simulate a single game
#############

# simulate half inning of a game
half_inning <- function(team_bat, team_pitch = NULL) {
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
            batter <- team_bat[i,]
            
            #Check for pitching factor and determine batting percentage
            #if(is.null(team_pitch)) {
                  batting_chance <- batter$ob_pct
            #} else {
            #      batting_chance <- batter$ob_pct * (1 + team_pitch)
            #}
                  
            at_bat_outcome <- rbinom(1, 1, batting_chance)

            #if the at bat results in getting on base, how many bases?
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

# function to simulate an entire game
game_sim <- function(away_team_bat, home_team_bat, away_team_pitch = NULL, home_team_pitch = NULL){
      
      # initialize global vector of possible on-base states
      on_base_states <<- c(1, 2, 3, 4)
      winner <<- NULL
      lineup_pos <<- 0
      extra_innings <<- 0
      
      away_team <- away_team_bat$TeamId[1]
      home_team <- home_team_bat$TeamId[1]
      
      # calculate variables for batting
      hitting <- rbind(away_team_bat, home_team_bat) %>%
            mutate(on_base_count = H + BB,
                   ob_pct = on_base_count/AB,
                   on_base_1B = (on_base_count - `X2B` - `X3B` - `HR`),
                   on_base_2B = (on_base_count - on_base_1B - `X3B` - `HR`),
                   on_base_3B = (on_base_count - on_base_1B - `X2B` - `HR`),
                   on_base_HR = (on_base_count - on_base_1B - `X2B` - `X3B`),
                   obc = on_base_count) %>%
            mutate_at(vars(starts_with("on_base")), funs(. / obc))
      
      away_hitting <- hitting %>% filter(TeamId == away_team)
      home_hitting <- hitting %>% filter(TeamId == home_team)
      
      # simulate away team score through 9 innings
      away_score <- 0
      for(i in 1:9){
            away_score <- away_score + half_inning(away_hitting, home_pitching)
      }
      away_lineup_pos <- lineup_pos
      lineup_pos <- 0
      # simulate home team score through 8 innings
      home_score <- 0
      for(i in 1:8){
            home_score <- home_score + half_inning(home_hitting, away_pitching)
      }
      
      # check if bottom of the 9th is played
      if(home_score > away_score){
            winner <- "H"
      } else {
            home_score <- home_score + half_inning(home_hitting, away_pitching)
      }
      
      # need to handle ties (extra innings)
      if(home_score > away_score){
            winner <- "H"
      } else if(away_score > home_score){
            winner <- "A"
      } else {
            tied <- TRUE
            while(tied == TRUE){
                  away_score <- away_score + half_inning(away_hitting, home_pitching)
                  home_score <- home_score + half_inning(home_hitting, away_pitching)
                  extra_innings <- extra_innings + 1
                  tied <- ifelse(away_score == home_score, TRUE, FALSE)
            }
            if(home_score > away_score) {
                  winner <- "H"
            } else {
                  winner <- "A"
            }
      }
      total_innings <- 9 + extra_innings
      game_info <- data.frame(cbind(away_score, home_score, away_team, home_team, winner))
      return(game_info)
}
