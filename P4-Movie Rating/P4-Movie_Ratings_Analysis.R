#----Importing Data----
getwd()
setwd("C:/Users/ptpar/OneDrive/Documents/Projects/R/P4 - Movie Rating/")
movrat_data <- read.csv("P4-Movie_Ratings_Data.csv")
colnames(movrat_data) <- c("Film", "Genre", "CriticRatings", "AudienceRatings", "BudgetMillions", "Year")
#Explore some data
head(movrat_data, 10)
str(movrat_data) #Year is identified as Integer
summary(movrat_data)

#Converting Year into a Factor
movrat_data$Year <- factor(movrat_data$Year)
str(movrat_data)
summary(movrat_data)
levels(movrat_data$Year)

#----Visualisation----
library(ggplot2)

#Scatter 1 - Deliverable
plt.scatter.base <- ggplot(data = movrat_data, aes(x = CriticRatings, y = AudienceRatings, colour = Genre))

#Adding Facets
plt.scatter.base + geom_point(aes(size = BudgetMillions)) +
  geom_smooth() +
  facet_grid(Genre~Year)

#Adding Zoom as confidence Interval deviates from -50 to 100
plt.scatter <- plt.scatter.base + geom_point(aes(size = BudgetMillions), alpha = I(0.4)) +
  geom_smooth(fill = NA) +
  facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0, 100))

plt.scatter +
  xlab("Critic Rating") +
  ylab("Audience Rating") +
  ggtitle("Audience Rating vs. Critic Rating by Genre") +
  theme(axis.title.x = element_text(colour = "Black", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 20, face = "bold"),
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size =15),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "right",
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5,
                                  face = "bold"))
#Histrogram - Deliverable 2
#Creating a base theme of the histogram
base.hist <- ggplot(data = movrat_data, aes(x = BudgetMillions))
plot.hist <- base.hist + 
  geom_histogram(binwidth = 10, aes(fill = Genre), colour = "Black") +
  facet_grid(.~Year)

#Now adding all the titles by using the theme function
plot.hist + 
  xlab("Budget ($M)") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution by Year") +
  theme(axis.title.x = element_text(colour = "Black", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 20, face = "bold"),
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size =15),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "right",
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5,
                                  face = "bold"))
#BoxPlot - Deliverable 3
plot.box.base <- ggplot(data = movrat_data, aes(x = Genre, y = AudienceRatings, colour = Genre))
plot.box <- plot.box.base + 
            geom_jitter() +
            geom_boxplot(size = 1.5, alpha = 0.5)


#Adding the Themes
plot.box +
  xlab("Genre") +
  ylab("Audience Rating") +
  ggtitle("Audience vs. Genre Box Plot") +
  theme(axis.title.x = element_text(colour = "Black", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 20, face = "bold"),
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size =15),
        
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.position = "none",
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5,
                                  face = "bold"))


head(movrat_data)

#Genre vs. Budget Millions - Deliverable 4
plot.box2.base <- ggplot(data = movrat_data, aes(x = Genre, y =  BudgetMillions, colour = Genre))
plot.box2 <- plot.box2.base + 
             geom_boxplot(size = 1.2, alpha = 0.4) + 
             geom_jitter()

plot.box2

#Adding Theme
plot.box2 +
  xlab("Genre") +
  ylab("Budget ($Ms)") +
  ggtitle("Genre vs. Budget of Movie") +
  theme(axis.title.x = element_text(colour = "Black", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 20, face = "bold"),
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size =15),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "right",
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5,
                                  face = "bold"))

# Box Critic Ratings vs Genre sized by Budget- Deliverable 5
plot.box3.base <- ggplot(data = movrat_data, aes(x = Genre, y = CriticRatings, colour = Genre))
plot.box3 <- plot.box3.base + geom_jitter(aes(size = BudgetMillions, alpha = I(0.4))) + 
                 geom_boxplot(size = 1.4, alpha = 0.3)

#Adding the Theme
plot.box3 + 
  ggtitle("Critic Ratings vs. Genre Box Plot") +
  theme(axis.title.x = element_text(colour = "Black", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 20, face = "bold"),
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size =15),
        
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5,
                                  face = "bold"))

#Relationship between Budget Million and Years - Deliverable 6
plot.violin.base <- ggplot(data = movrat_data, aes(x = Year, y = BudgetMillions))
plot.violin <- plot.violin.base + 
               geom_violin(fill = "DarkBlue")
plot.violin +
  labs(x = "Year", y = "Budget ($Ms)", title = "Budget Outlook in Time") +
  theme(axis.title.x = element_text(colour = "Black", size = 20, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 20, face = "bold"),
        axis.text.x = element_text(colour = "Black", size = 15),
        axis.text.y = element_text(colour = "Black", size =15),
        
        plot.title = element_text(colour = "Black", size = 20, hjust = 0.5,
                                  face = "bold"))




