#----Importing the dataset----
getwd()
setwd("C:/Users/ptpar/OneDrive/Documents/Projects/R/P5-Financial Review/")
fin_data <- read.csv("P5-Future-500-The-Dataset.csv", na.strings = c(""))

#Some Exploration
head(fin_data, 20)
ncol(fin_data)
nrow(fin_data)
str(fin_data)

#-----Converting Variables to their factors or numerics respectively----
#Converting Time of Inception into a Factor
fin_data$Inception <- factor(fin_data$Inception)
str(fin_data$Inception)

#Converting ID into a Factor
fin_data$ID <- factor(fin_data$ID)
str(fin_data$ID)

#Converting Expenses into Numeric
#Removing the "Dollars" and commas in the rows
fin_data$Expenses <- gsub(" Dollars", "", fin_data$Expenses)
fin_data$Expenses <- gsub(",", "", fin_data$Expenses)
#Check
head(fin_data$Expenses)
#Now converting the column into a numeric
fin_data$Expenses <- as.numeric(fin_data$Expenses)
str(fin_data$Expenses)

#Converting Growth into a numeric variable
fin_data$Growth <- gsub("\\%", "", fin_data$Growth)
#Check
head(fin_data$Growth)
fin_data$Growth <- as.numeric(fin_data$Growth)

#Converting Revenue into a numeric
fin_data$Revenue <- gsub("\\$","", fin_data$Revenue)
fin_data$Revenue <- gsub(",", "", fin_data$Revenue)
fin_data$Revenue <- as.numeric(fin_data$Revenue)

#Re-check on the data 
str(fin_data)

#Creating a backup 
fin_backup <- fin_data
fin_data <- fin_backup
#----Locating the Missing Data----
fin_data[!complete.cases(fin_data), ]
nrow(fin_data[!complete.cases(fin_data), ]) # 12 rows with missing data

#Filtering the missing data
fin_data[is.na(fin_data$Expenses), ] # 1 rows missing expenses and 2 rows missing all Revenue, Profit and Expenses
fin_data[is.na(fin_data$Industry), ] # 2 rows missing Industry

#Removing data with missing Industry as we do not have any information of the Company
fin_data <- fin_data[!is.na(fin_data$Industry), ]
#Check
fin_data[is.na(fin_data$Industry), ] #None, Clean removal

#Resetting the Dataset Index
rownames(fin_data) #14,15 are missing after removal above
rownames(fin_data) <- 1:nrow(fin_data)


#Rechecking for missing
fin_data[!complete.cases(fin_data), ]

#----Imputing States for the known Cities New York and San Francisco----
fin_data[is.na(fin_data$State) & fin_data$City == "New York", "State"] <- "NY"
fin_data[is.na(fin_data$State) & fin_data$City == "San Francisco", "State"] <- "CA"

#Checking for the Imputation
fin_data[c(11,377,82,265), ] #Successfull

fin_data[!complete.cases(fin_data), ]

#----Imputing by Median----
#Mean can be skewed by any present outliers

#Imputing for the Employees in the Retail Industry
median.emp.retail <- median(fin_data[fin_data$Industry == "Retail", "Employees"], na.rm = T)
fin_data[is.na(fin_data$Employees) & fin_data$Industry == "Retail", "Employees"] <- median.emp.retail
#Check
fin_data[3, ] #Successful

#Imputing for the Employes in the Financial Services
median.emp.finserv <- median(fin_data[fin_data$Industry == "Financial Services", "Employees"], na.rm = T)
fin_data[is.na(fin_data$Employees) & fin_data$Industry == "Financial Services", "Employees"] <- median.emp.finserv

#Check
fin_data[330, ] #Successful

#Imputing the Revenue and Expenses for the Rows 8 and 42 
#In this case both belong to the Construction Industry
median.rev.cons <- median(fin_data[fin_data$Industry == "Construction", "Revenue"], na.rm = T)
median.exp.cons <- median(fin_data[fin_data$Industry == "Construction", "Expenses"], na.rm = T)

#Imputing the medians
fin_data[is.na(fin_data$Revenue) & is.na(fin_data$Profit), "Revenue"] <- median.rev.cons
fin_data[is.na(fin_data$Expenses) & is.na(fin_data$Profit), "Expenses"] <- median.exp.cons
#Check
fin_data[c(8, 42), ] #Imputed Successully

#Creating a backup
fin_backup1 <- fin_data

#Calculating and Imputing Profit: Revenue - Expenses
profit.cons <- median.rev.cons - median.exp.cons
fin_data[is.na(fin_data$Profit), "Profit" ] <- profit.cons

#Check
fin_data[c(8, 42), ] #Successful

#Calculating median for Growth in the Construction Industry and Imputing it for Row 8
median.growth.cons <- median(fin_data[fin_data$Industry == "Construction", "Growth"], na.rm = T)
fin_data[is.na(fin_data$Growth), "Growth"] <- median.growth.cons

#Check:
fin_data[8, ] #Successful 


#----Checking for Missing Data----
fin_data[!complete.cases(fin_data), ] #Row 15 and Row 20

#Row20 cannot be imputed as we do not have information about the inception of the company

#----Imputation of Expense for row 15----
profit.row15 <- fin_data[15, "Revenue"] - fin_data[15, "Profit"]
fin_data[15, "Expenses"] <- profit.row15

#Check
fin_data[15, ] #Successful


#Final Check for missing data
fin_data[!complete.cases(fin_data), ]

#-----Visualisation----
library(ggplot2)

#A scatterplot classified by industry showing revenue, expenses and profits 
plot1.base <- ggplot(data = fin_data)
plot1 <- plot1.base + geom_point(aes(x = Revenue, y = Expenses,
                                     colour = Industry, size = Profit))
plot1

#Setting the theme
plot1 + 
  labs(title = "Expenses vs. Revenue of Industries") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        
        title = element_text(size = 25, face = "bold", hjust = 0.5))

#A scatter that includes industry trends for the expenses and revenue
plot2 <- ggplot(data = fin_data, aes(x = Revenue, y = Expenses, colour = Industry))
plot2 + geom_point() +
  geom_smooth(fill = NA) +
  labs(title = "Expenses vs. Revenue Trend") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        
        title = element_text(size = 25, face = "bold", hjust = 0.5))

#A boxplot showing Industr and Growth categorised by Industry
plot3 <- ggplot(data = fin_data, aes(x = Industry, y = Growth, colour = Industry))
plot3 + geom_jitter() + geom_boxplot(size = 1, alpha = 0.5, outlier.colour = NA) +
  labs(title = "Growth vs. Industry") +
  theme(axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        
        legend.position = "",
        
        title = element_text(size = 25, face = "bold", hjust = 0.5))
  
  
