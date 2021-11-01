rm(list=ls())
getwd()
setwd("/home/frcovell/CMEECourseWork/week3/code")

load("../data/GPDDFiltered.RData")
require(maps)

map()
map(wrap = c(0,360), fill = TRUE, col = 2) # pacific-centered map of the world
map(wrap = c(0, 360, NA), fill = TRUE, col = 2) # idem, without Antarctica
map("state", xlim = range(gpdd$lat), ylim = range(gpdd$long))
