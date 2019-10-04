#----Importing the data----
setwd("C:/Users/ptpar/OneDrive/Documents/Projects/R/P7-Weather Analysis/Data/")

Chicago <- read.csv("Chicago-F.csv", row.names = 1)
NewYork <- read.csv("NewYork-F.csv", row.names = 1)
Houston <- read.csv("Houston-F.csv", row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names = 1)

#----Converting all data into a matrix as data is numeric----
Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
Houston <- as.matrix(Houston)
SanFrancisco <- as.matrix(SanFrancisco)

#----Putting all the data into a list----
Weather <- list(Chicago = Chicago, NewYork = NewYork, Houston = Houston, SanFrancisco = SanFrancisco)

#Check
Weather$Houston # Successful

#----Working out the mean using apply----
round(sapply(Weather, rowMeans), 2)

#----Working out the Weather Fluctuation----
sapply(Weather, function(z) round((z[1, ] - z[2, ])/z[2, ], 2))

#----Working out the maximum and minimums
sapply(Weather, apply, 1, max)
sapply(Weather, apply, 1, min)

#----Getting the months with the maximum and minimum temperatures---
sapply(Weather, apply, 1, function(x) names(which.max(x)))
sapply(Weather, apply, 1, function(y) names(which.min(y)))

