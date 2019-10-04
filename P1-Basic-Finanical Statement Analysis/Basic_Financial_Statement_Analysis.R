#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#pro???t for each month
profit <- revenue - expenses
#pro???t after tax for each month (the tax rate is 30%) 
profit.after.tax <- round(profit * 0.3, 2)
profit.after.tax
#pro???t margin for each month - equals to pro???t a after tax divided by revenue 
profit.margin <- round(profit.after.tax / revenue, 2) * 100
profit.margin
#good months - where the pro???t after tax was greater than the mean for the year
good.months <- profit.after.tax > mean(profit.after.tax)
good.months
#bad months - where the pro???t after tax was less than the mean for the year
bad.months <- !good.months
bad.months
#the best month - where the pro???t after tax was max for the year
best.month <- profit.after.tax == max(profit.after.tax)
#the worst month - where the pro???t after tax was min for the year
worst.month <- profit.after.tax == min(profit.after.tax)

#Converting all the numerical outcomes to 1000's 
revenue.1000 <- round(revenue/1000, 0)
expenses.1000 <- round(expenses/1000, 0)
profit.1000 <- round(profit/1000, 0)
profit.after.tax.1000 <- round(profit.after.tax/ 1000, 0)

#Putting it into a matrix
Results <- rbind(
  Revenue = revenue.1000,
  Expenses = expenses.1000,
  Profit = profit.1000,
  Profit_after_Tax = profit.after.tax.1000,
  Profit_Margin = profit.margin,
  Good_Months = good.months,
  Bad_Months = bad.months,
  Best_Month = best.month,
  Worst_Month = worst.month
)

colnames(Results) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                       "October", "November", "December")
Results
