library(dplyr)
library(tidyr)

# Read in data and count players
data = read.csv("data/CrowdstormingDataJuly1st.csv", stringsAsFactors = FALSE)
original.count = nrow(data)



# Initial Data Wrangling


# Replace skin tone ratings with the average
data = data %>% mutate(rating = (rater1 + rater2) / 2) # %>% select(-rater1, -rater2)
# Remove players that don't have skin tone ratings from both raters
data = data %>% filter(is.na(rater1) == FALSE & is.na(rater2) == FALSE)
omit.na.ratings.count = nrow(data)

# Add column of total red cards (sum of red cards and yellow-red cards)
data = data %>% mutate(totalReds = redCards + yellowReds)

# Add BMI column
data = data%>% mutate(BMI = weight / (height/100)^2)


num.omitted = original.count - omit.na.ratings.count
# Omitted 21407 players who did not have skin color ratings



# Remove and rename columns

# Drop photoID column (we don't have access to the image files anyways)
data = data %>% select(-photoID)

# Rename columns
names(data)[19] = "refID"
names(data)[20] = "refCountryID"



# Number of records where the sum of the victories, ties and defeats 
# does NOT equal the total number of games
v.t.d.not.total.games = nrow(filter(data, ties + victories + defeats != games))

player.win.loss.data = data %>% group_by(player) %>% summarise(games.won = sum(victories), games.lost = sum(defeats), games.tied = sum(ties), total.games = sum(games))

# Create new data frame with total red cards for each player and their position.
player.red.cards = data %>% group_by(player) %>% summarise(playerName = first(player), playerPosition = first(position), totalReds = sum(totalReds))

# Remove players with NA positions
player.red.cards = player.red.cards %>% filter(is.na(playerPosition) == FALSE)

# How the player-ref dyad works
player.ref.games = select(data, player, refID, games)






get.data = function(){
  return(data)
}

get.player.total.reds = function(){
  return(player.red.cards)
}

get.player.game.data = function() {
  return(player.win.loss.data)
}





