# First we read the data set into an object
revenuedata = read.csv("Revenue.csv")

# Now we view the data to get a feel for it
View(revenuedata)

# Next we view a summary of the data
summary(revenuedata)

# Result:
# SalesAmt  Employee.turnover  No.Employees   No.SalesCampaigns
# 1,038,452: 1   4.97%  : 2        Min.   : 9.00   Min.   : 3.000   
# 1,078,508: 1   2.24%  : 1        1st Qu.:13.25   1st Qu.: 6.000   
# 1,323,851: 1   2.47%  : 1        Median :16.00   Median : 7.000   
# 1,506,106: 1   2.57%  : 1        Mean   :15.73   Mean   : 6.962   
# 1,552,170: 1   2.64%  : 1        3rd Qu.:18.00   3rd Qu.: 8.000   
# 1,610,251: 1   2.73%  : 1        Max.   :22.00   Max.   :10.000   
# (Other)  :20   (Other):19                                         
# RDExpense  No.NewProdToMarket  MaterialCost No.CustComplaints    No.Cust  
# 11,673 : 1   Min.   :1.000      30.8%  : 2    Min.   :26.00     1,687  : 1  
# 11,826 : 1   1st Qu.:3.000      32.8%  : 2    1st Qu.:34.00     1,731  : 1  
# 12,432 : 1   Median :5.000      33.9%  : 2    Median :41.50     1,851  : 1  
# 13,402 : 1   Mean   :4.269      28.6%  : 1    Mean   :43.27     2,209  : 1  
# 13,452 : 1   3rd Qu.:5.000      28.8%  : 1    3rd Qu.:50.75     2,341  : 1  
# 13,509 : 1   Max.   :8.000      28.9%  : 1    Max.   :64.00     2,372  : 1  
# (Other):20                      (Other):17                      (Other):20 


# Before we continue, we graph using the pairs() command in order to
# see relationships within the data set 
pairs(revenuedata, pch=20)

# From the pairs plot we can determine that the variables which may have 
# a positive correlation with a higher sales amount will be No.Employees,
# No.SalesCampaign, RDExpense, and No.Cust.

# First we are going to plot the No.Employees vs the SalesAmt
plot(revenuedata$No.Employees, revenuedata$SalesAmt, ylab="Sales Amount",
     xlab="Number of Employees", main="Number of Employees vs Sales")

# Next we are going to create a linear model of the No.Employees vs SalesAmt
# On the following linear models, I will be placing them in variables for 
# cleaner code
lm(as.numeric(revenuedata$SalesAmt)~as.numeric(revenuedata$No.Employees))

# Result:
# Coefficients:
# (Intercept)  as.numeric(revenuedata$No.Employees)  
# 8.1006                                0.3432  

# Use this command to overlay the two graphs
par(new=TRUE)

# Now we use the abline(lm()) commands to plot a line of best fit 
# to show the correlation between the No.Employees and the SalesAmt
abline(lm(revenuedata$SalesAmt~revenuedata$No.Employees), col='red', lwd=4,lty=6)

# Now we print a summary of the data we previously plotted
summary(lm(as.numeric(revenuedata$SalesAmt)~as.numeric(revenuedata$No.Employees)))

# Result: 
# Call: lm(formula = as.numeric(revenuedata$SalesAmt) ~ as.numeric(revenuedata$No.Employees))
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -10.5330  -6.0850  -0.7789   4.5272  14.4670 

# Coefficients:
#                                       Estimate Std. Error t value Pr(>|t|)
# (Intercept)                            8.1006     6.3574   1.274    0.215
# as.numeric(revenuedata$No.Employees)   0.3432     0.3926   0.874    0.391

# Residual standard error: 7.685 on 24 degrees of freedom
# Multiple R-squared:  0.03086,	Adjusted R-squared:  -0.009519 
# F-statistic: 0.7643 on 1 and 24 DF,  p-value: 0.3907


# Second we are going to plot the No.SalesCampaigns vs SalesAmt
# Here we see a slight increasing trend between the higher the number of 
# sales campaigns and the numebr of sales increasing with a few
# outliers sitting at 3 and 4 employees.
plot(revenuedata$No.SalesCampaigns, revenuedata$SalesAmt, ylab="Sales Amount",
     xlab="Number of Sales Campaigns", main="Number of Sales Campaigns vs Sales")

# Now we create a linear model of the No.SalesCampaigns vs SalesAmt and place
# it in a variable named SCvsSaleAmt
SCvsSaleAmt=lm(as.numeric(revenuedata$SalesAmt)~as.numeric(revenuedata$No.SalesCampaigns))

# Use the par command to overlay the two graphs
par(new=TRUE)

# here we use the abline command and the lm command on the SCvsSaleAmt
# variable to create a line of best fit for the graph
abline(lm(SCvsSaleAmt), lwd=4, lty=6, col='green')

# now we go over a summary of the relationship to see if there is any 
# correlation between the two variables
summary(SCvsSaleAmt)

# Result:
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -11.767  -6.260  -0.257   5.745  13.233 

# Coefficients:
#                                           Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                11.7772     5.7715   2.041   0.0524
# as.numeric(revenuedata$No.SalesCampaigns)   0.2475     0.7995   0.310   0.7596

# (Intercept)                               .
# as.numeric(revenuedata$No.SalesCampaigns)  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 7.791 on 24 degrees of freedom
# Multiple R-squared:  0.003976,	Adjusted R-squared:  -0.03752 
# F-statistic: 0.09582 on 1 and 24 DF,  p-value: 0.7596

# now we create a plot of the RDExpenses vs the SalesAmt
# This graph shows the most promise for having a close correlation.
plot(as.numeric(revenuedata$RDExpense), revenuedata$SalesAmt, ylab="Sales Amount",
     xlab="R&D Expenses", main="R&D expenses vs Sales")

# Here we hold our linear model within a variable
RDvsSale = lm(as.numeric(revenuedata$SalesAmt)~as.numeric(revenuedata$RDExpense))

# Use this command to overlay the two graphs
par(new=TRUE)

# Now we create a line of best fit for the graph and color it purple
abline(lm(RDvsSale), lwd=4, lty=6, col='purple')

# On this command we create a summary of the linear model correlating the 
# RDExpenses and the Sales amount
summary(RDvsSale)

# Results:
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -15.3159  -4.9097  -0.4085   5.2061  16.0427 
#
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                         8.5938     2.9368   2.926  0.00739 **
# as.numeric(revenuedata$RDExpense)   0.3634     0.1902   1.911  0.06801 . 
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 7.273 on 24 degrees of freedom
# Multiple R-squared:  0.1321,	Adjusted R-squared:  0.09591 
# F-statistic: 3.652 on 1 and 24 DF,  p-value: 0.06801

# Fourth we create a plot to see the relationship of No.Cust vs SalesAmt
# Here we can see that although we originally thought there would be a 
# correlation, after modeling the data we can see that there is a weak correlation
# between the variables
plot(as.numeric(revenuedata$No.Cust), revenuedata$SalesAmt, ylab="Sales Amount",
     xlab="Number of Customers", main="Number of Customers vs Sales")

# hold the linear model within the CustVsSale variable
CustVsSale=lm(as.numeric(revenuedata$SalesAmt)~as.numeric(revenuedata$No.Cust))

# Use this command to overlay the two graphs
par(new=TRUE)

# create the line of best fit for the scatterplot we created above
abline(lm(CustVsSale), lwd=4, lty=6, col='pink')

# print a summary of the linear model of the No.Cust vs SalesAmount variables
summary(CustVsSale)

# Result:
#Residuals:
#     Min       1Q   Median       3Q      Max 
# -11.9576  -6.1501  -0.3997   5.7647  12.6998 

# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     12.72923    3.14724   4.045 0.000471 ***
#  as.numeric(revenuedata$No.Cust)  0.05709    0.20379   0.280 0.781756    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 7.794 on 24 degrees of freedom
# Multiple R-squared:  0.00326,	Adjusted R-squared:  -0.03827 
# F-statistic: 0.07849 on 1 and 24 DF,  p-value: 0.7818


# Based on our findings, the strongest predictor of Sales is the RDExpenses.
# This was determined through a scatter plot containing a line of best fit,
# and a summary describing all aspects of the variables.
# The Standard Error of the slope was 0.1902. This number is greater than
# 0.05 so we would reject this from the data set, however this was the 
# strongest relationship within the data set. 
# The Standard Error of the model was 2.9368. This value lies just within
# three standard deviations away from the mean, indicating that there is 
# a relationship between the variables but it is a weak one. 
# The r^2 (strength of relationship) is 0.09591, indicating that this
# relationship is fairly weak when looking at the facts. Again, this was
# the strongest relationship within the data set which is why we chose to 
# use it.




