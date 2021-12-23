rm(list=ls())


load("../data/GPDDFiltered.RData")
require(maps)

map()
points(x = gpdd$long, y = gpdd$lat, col = "red1")


# Data is biased to the North hemisphere specifically west cost North America, the UK and Europe.
# This would also indicate a bias of climate 