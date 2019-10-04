#Delivarables

#Character: Machine names
#Vector: (min, mean, max) utilisation for the month (excluding unknown hours)
#Logical: Has utilisation ever fallen below 90%
#Vector: All hours where utilisation is unknown
#Dataframe: For RL1 machine
#Plot: Time Series plot for all machines against their utilisation

#----Importing the Dataset----
getwd()
setwd("C:/Users/ptpar/OneDrive/Documents/Projects/R/P6-Machine Utilisation/")

mac.util.data <- read.csv("P6-Machine-Utilization-Data.csv")

#Exploration
str(mac.util.data)
head(mac.util.data, 15)

#----Deriving the Percent Utilisation: 1 - Percent Idle----
mac.util.data$Percent.Utilisation <- 1 - mac.util.data$Percent.Idle
head(mac.util.data, 15)

#----Handling datetime in R----
#Changing to machine-readable time for timeseries analysis
mac.util.data$Timestamp <- as.POSIXct(mac.util.data$Timestamp, format = "%d/%m/%Y %H:%M")
head(mac.util.data)

summary(mac.util.data)


#----Arranging data into a list----
#Starting by segregating the RL1 machine 
RL1 <- mac.util.data[mac.util.data$Machine == "RL1", ]
str(RL1)
#Factoring the RL1 machine as others have translated on to the new Data Frame
RL1$Machine <- factor(RL1$Machine)
str(RL1)


#Constructing the (min, mean, max) vector for the RL1 machine
util.stats.rl1 <- c(min(RL1$Percent.Utilisation, na.rm = T),
                    mean(RL1$Percent.Utilisation, na.rm = T),
                    max(RL1$Percent.Utilisation, na.rm = T))

util.stats.rl1 #We can see that for our logical part, RL1 has performed under 90%

#Checking where the machine has fallen under 90%
length(which(RL1$Percent.Utilisation < 0.9)) > 0

util.under.90.flag <- length(which(RL1$Percent.Utilisation < 0.9)) > 0

#Constructing the list
list_RL1 <- list(Machine="RL1", Stats=util.stats.rl1, Low_Threshold = util.under.90.flag)

list_RL1

#Adding the Unknown Hours to the list
list_RL1$UnknownHours <- RL1[is.na(RL1$Percent.Utilisation), "Timestamp"]

#Adding the RL1 Dataframe into the list
list_RL1$Data <- RL1
summary(list_RL1)

#----Visualisation----
library(ggplot2)
machine.plot.base <- ggplot(mac.util.data)
machine.plot.base + geom_line(aes(x=Timestamp, y=Percent.Utilisation, colour = Machine), size = 1.2) +
  facet_grid(Machine~.) +
  geom_hline(yintercept = 0.9,
             colour = "Gray", size = 1.2, linetype = 3)

machine.plot <- machine.plot.base + geom_line(aes(x=Timestamp, y=Percent.Utilisation, colour = Machine), size = 1.2) +
  facet_grid(Machine~.) +
  geom_hline(yintercept = 0.9,
             colour = "Gray", size = 1.2, linetype = 3) +
  labs(x = "Time", y = "Utilisation (%)", title = "Machine Utilisation in Time") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20, colour = "Black"),
        
        title = element_text(size = 25, colour = "Black", face = "bold", hjust = 0.5))


#Adding plot to the list
list_RL1$Utilisation.plot <- machine.plot


#----Final Output----
summary(list_RL1)
list_RL1
