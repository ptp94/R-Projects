#----Importing the dataset----
getwd()
setwd("C:/Users/ptpar/OneDrive/Documents/Projects/R/P4 - Movie Rating/")

movie.dat <- read.csv("P4_1-Movie_Domestic_Percentage_Gross_Data.csv")
colnames(movie.dat) <- c("Day", "Director", "Genre", "MovieTitle", "ReleaseDate", "Studio",
                         "AdjustedGross", "Budget($M)", "Gross", "IMDBRating", "MovieLensRating",
                         "ProfitOverseas", "PercentOverseas", "Profit", "PercentProfit", "Runtime",
                         "ProfitUS", "PercentGrossUS")
#Data Exploration
head(movie.dat)
str(movie.dat)
levels(movie.dat$Genre)
colnames(movie.dat)
levels(movie.dat$Studio)

#Filtering
filter1 <- movie.dat$Genre %in% c("action", "adventure", "animation", "comedy", "drama")
filter2 <- movie.dat$Studio %in% c("Buena Vista Studios", "Fox", "Paramount Pictures", "Sony", "Universal", "WB")

movie.dat2 <- movie.dat[filter1 & filter2, ]

#Cool Insight
library(ggplot2)
library(extrafont)
extrafont::font_import()
ggplot(data = movie.dat, aes(x=Day)) + geom_bar()

head(movie.dat2)

plot.boxplot.base <- ggplot(data = movie.dat2, aes(x = Genre, y = PercentGrossUS))
plot.boxplot <- plot.boxplot.base + geom_jitter(aes(size = 'Budget', colour = Studio)) + geom_boxplot(size = 1.5, alpha = 0.4)
plot.boxplot + 
  labs(x = "Genre", y = "Gross % US", title = "Domestic Gross % by Genre") +
  theme(axis.title.x = element_text(colour = "Blue", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Blue", size = 20, face = "bold"),
        
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size = 15),
        
        legend.title = element_text(colour = "Black", size = 20),
        legend.text = element_text(colour = "Black", size = 15),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5, family = "Comic Sans"))

rlang::last_error()
