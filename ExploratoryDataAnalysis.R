source("DataPreparation.R")
library(ggplot2)
library(dplyr)

exploratory.data = get.data()
player.data = get.player.game.data()

get.data = function(){
  return(exploratory.data)
}

get.sample.player.data = function(){
  return(slice(player.data, c(90,199,615,1042)))
}

get.variance = function(){
  variance = var(exploratory.data$totalReds)
  return(round(variance, 4))
}

get.mean = function(){
  mean = mean(exploratory.data$totalReds)
  return(round(mean, 4))
}

get.difference = function(){
  difference = round(abs(get.variance() - get.mean()), 4)
  return(format(difference, scientific=FALSE))
}

graph.histogram.rater.comparison = function(){
  g = ggplot(exploratory.data) + geom_histogram(aes(x = rater1), binwidth = 0.25, fill="red", alpha=0.5) +
    geom_histogram(aes(x = rater2), binwidth = 0.25, fill="blue", alpha=0.5) + 
    xlab("Skin tone rating")
  return(g)
}

graph.histogram.average.ratings = function(){
  g = ggplot(exploratory.data) + geom_histogram(aes(x = rating), binwidth = 0.125, fill="green", alpha=0.5) +
    xlab("Average skin tone rating")
  return(g)
}

graph.scatter.games.vs.won = function(){
  g = ggplot(player.data) + geom_point(aes(x = total.games, y = games.won))
  return(g)
}

#thinking it might be good to add another graph with card data. Thoughts?

