###### Multiple Regression Assignment - Andrew Reeves & Brett Voglesang ######

#Read in the data 
rev = read.csv("Revenue.csv")

#View a summary of the data 
summary(rev)
# Result:
#    SalesAmt       Employee.turnover  No.Employees   No.SalesCampaigns   RDExpense    
# Min.   : 809131   Min.   :0.02240   Min.   : 9.00   Min.   : 3.000    Min.   : 8175  
# 1st Qu.:1369415   1st Qu.:0.02882   1st Qu.:13.25   1st Qu.: 6.000    1st Qu.:12674  
# Median :1656316   Median :0.03300   Median :16.00   Median : 7.000    Median :14766  
# Mean   :1644954   Mean   :0.03592   Mean   :15.73   Mean   : 6.962    Mean   :14846  
# 3rd Qu.:1990783   3rd Qu.:0.04105   3rd Qu.:18.00   3rd Qu.: 8.000    3rd Qu.:17874  
# Max.   :2302219   Max.   :0.05610   Max.   :22.00   Max.   :10.000    Max.   :21761  
# No.NewProdToMarket  MaterialCost    No.CustComplaints    No.Cust    
# Min.   :1.000      Min.   :0.2860   Min.   :26.00     Min.   :1687  
# 1st Qu.:3.000      1st Qu.:0.3015   1st Qu.:34.00     1st Qu.:2414  
# Median :5.000      Median :0.3135   Median :41.50     Median :3065  
# Mean   :4.269      Mean   :0.3150   Mean   :43.27     Mean   :2933  
# 3rd Qu.:5.000      3rd Qu.:0.3297   3rd Qu.:50.75     3rd Qu.:3396  
# Max.   :8.000      Max.   :0.3400   Max.   :64.00     Max.   :3851


#Show a quick graph of the different relationships between the data
pairs(rev)

#Show the headings of the data to see what values to include in the model
head(rev)
# Result:
# SalesAmt Employee.turnover No.Employees No.SalesCampaigns RDExpense No.NewProdToMarket MaterialCost
# 1  1667331            0.0310           16                 7     15759                  5        0.330
# 2   993389            0.0476           11                 4     11673                  1        0.331
# 3  1820545            0.0284           16                 8     17642                  4        0.288
# 4  2013815            0.0264           19                 8     21761                  4        0.328
# 5  1038452            0.0497           10                 4     14532                  2        0.339
# 6  1995790            0.0320           18                 8     17941                  6        0.303
# No.CustComplaints No.Cust
# 1                43    3129
# 2                60    1851
# 3                39    3405
# 4                34    3338
# 5                59    2209
# 6                33    3329


# Here is the first step of the stepwise regression model
#create a linear model for sales amount based on all the variables
rev.lm = lm(SalesAmt ~ Employee.turnover + No.Employees + 
              No.SalesCampaigns + RDExpense + No.NewProdToMarket + 
              MaterialCost + No.CustComplaints + No.Cust, data=rev)

#see a summary of the model to determine which variables have no significant predictive power
summary(rev.lm)
# Result:
# Call:
#lm(formula = SalesAmt ~ Employee.turnover + No.Employees + No.SalesCampaigns + 
#     RDExpense + No.NewProdToMarket + MaterialCost + No.CustComplaints + 
#     No.Cust, data = rev)
#
# Residuals:
# Min     1Q Median     3Q    Max 
# -51815 -20635  -2089  20573  83036 
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.354e+06  6.032e+05   3.902 0.001147 ** 
#  Employee.turnover  -5.309e+06  3.010e+06  -1.764 0.095658 .  
#  No.Employees        2.296e+04  1.165e+04   1.971 0.065253 .  
#  No.SalesCampaigns   2.378e+04  2.578e+04   0.923 0.369108    
#  RDExpense           2.092e+00  3.971e+00   0.527 0.605201    
#  No.NewProdToMarket  2.235e+03  6.329e+03   0.353 0.728314    
#  MaterialCost       -4.364e+05  4.412e+05  -0.989 0.336456    
#  No.CustComplaints  -2.149e+04  5.055e+03  -4.252 0.000538 ***
#  No.Cust            -6.103e+00  1.815e+01  -0.336 0.740733    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 34660 on 17 degrees of freedom
# Multiple R-squared:  0.9959,	Adjusted R-squared:  0.994 
# F-statistic: 514.9 on 8 and 17 DF,  p-value: < 2.2e-16

# In this linear model we can see that the No.Cust is NOT a good predictor of Sales 
# We will then take this variable out and move to the next step.

# This is the second step in the regression model
# Here we are creating a linear model of the data after removing the Number of Customers
rev.lm = lm(SalesAmt ~ Employee.turnover + No.Employees + 
              No.SalesCampaigns + RDExpense + No.NewProdToMarket + 
              MaterialCost + No.CustComplaints, data=rev)

#see a summary of the model to determine which variables have no significant predictive power
summary(rev.lm)
# Result:
# Call:
# lm(formula = SalesAmt ~ Employee.turnover + No.Employees + No.SalesCampaigns + 
#     RDExpense + No.NewProdToMarket + MaterialCost + No.CustComplaints, 
#   data = rev)

# Residuals:
#  Min     1Q Median     3Q    Max 
# -56730 -20115  -2615  19528  79070 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         2.325e+06  5.824e+05   3.993 0.000854 ***
# Employee.turnover  -4.987e+06  2.782e+06  -1.793 0.089835 .  
#No.Employees        2.237e+04  1.123e+04   1.992 0.061766 .  
#No.SalesCampaigns   2.530e+04  2.475e+04   1.022 0.320284    
#RDExpense           2.033e+00  3.869e+00   0.526 0.605639    
#No.NewProdToMarket  2.109e+03  6.160e+03   0.342 0.736039    
#MaterialCost       -4.419e+05  4.298e+05  -1.028 0.317502    
#No.CustComplaints  -2.148e+04  4.928e+03  -4.358 0.000379 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 33800 on 18 degrees of freedom
# Multiple R-squared:  0.9959,	Adjusted R-squared:  0.9943 
# F-statistic:   619 on 7 and 18 DF,  p-value: < 2.2e-16


# In this linear model we can see that the No.NewProdToMarket is NOT a good predictor of Sales
# We will then take this variable out and move to the next step.

# This is our third step in the regression model. 
# Here we are creating a linear model of the data after removing the No.NewProdToMarket and No.Cust
# variables.
rev.lm = lm(SalesAmt ~ Employee.turnover + No.Employees + 
              No.SalesCampaigns + RDExpense + 
              MaterialCost + No.CustComplaints, data=rev)

#see a summary of the model to determine which variables have no significant predictive power
summary(rev.lm)
# Result:
# Residuals:
# Min     1Q Median     3Q    Max 
# -55766 -19079  -3148  20097  79194 
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        2.357e+06  5.617e+05   4.195 0.000491 ***
#  Employee.turnover -5.020e+06  2.715e+06  -1.849 0.080054 .  
# No.Employees       2.139e+04  1.060e+04   2.017 0.058002 .  
#No.SalesCampaigns  2.710e+04  2.362e+04   1.147 0.265506    
#RDExpense          1.610e+00  3.580e+00   0.450 0.657994    
#MaterialCost      -4.333e+05  4.190e+05  -1.034 0.314085    
#No.CustComplaints -2.181e+04  4.717e+03  -4.624 0.000185 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 33000 on 19 degrees of freedom
#Multiple R-squared:  0.9958,	Adjusted R-squared:  0.9945 
#F-statistic: 757.3 on 6 and 19 DF,  p-value: < 2.2e-16


# In this linear model we can see that the RDExpense is NOT a good predictor of Sales.
# We will then take this variable out and move to the next step. 

# This is our fourth step in the regression model. 
# Here we are creating a linear model of the data after removing the RDExpense, No.NewProdToMarket and
# No.Cust variables.
rev.lm = lm(SalesAmt ~ Employee.turnover + No.Employees + 
              No.SalesCampaigns + MaterialCost + No.CustComplaints, data=rev)

#see a summary of the model to determine which variables have no significant predictive power
summary(rev.lm)
# Result:
# Residuals:
#Min     1Q Median     3Q    Max 
#-55487 -19176  -4691  19878  80677 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        2511785     434332   5.783 1.17e-05 ***
#  Employee.turnover -5638369    2294267  -2.458   0.0232 *  
#  No.Employees         19474       9514   2.047   0.0540 .  
#No.SalesCampaigns    23208      21535   1.078   0.2940    
#MaterialCost       -437900     410450  -1.067   0.2987    
#No.CustComplaints   -22978       3861  -5.952 8.05e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 32340 on 20 degrees of freedom
#Multiple R-squared:  0.9958,	Adjusted R-squared:  0.9947 
#F-statistic: 946.5 on 5 and 20 DF,  p-value: < 2.2e-16

# In this linear model we can see that the MaterialCost is NOT a good predictor of Sales. 
# We will then take this variable out and move to the next step.

# This is our fifth step in the regression model. 
# Here we are creating a linear model of the data after removing the MaterialCost, RDExpense, 
# No.NewProdToMarket, and No.Cust variables.
rev.lm = lm(SalesAmt ~ Employee.turnover + No.Employees + 
              No.SalesCampaigns + No.CustComplaints, data=rev)

#see a summary of the model to determine which variables have no significant predictive power
summary(rev.lm)
#Results:
#Residuals:
#Min     1Q Median     3Q    Max 
#-57227 -17630  -2611  17246  85368 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        2268888     371094   6.114 4.57e-06 ***
#  Employee.turnover -5591504    2301384  -2.430   0.0242 *  
#  No.Employees         18565       9507   1.953   0.0643 .  
#No.SalesCampaigns    32854      19609   1.675   0.1087    
#No.CustComplaints   -21813       3715  -5.871 7.92e-06 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 32440 on 21 degrees of freedom
#Multiple R-squared:  0.9956,	Adjusted R-squared:  0.9947 
#F-statistic:  1175 on 4 and 21 DF,  p-value: < 2.2e-16


# In this linear model we can see that the No.SalesCampaigns is NOT a good predictor of Sales.
# We will then take this variable out and move to the next step. 

# This is our sixth step in the regression model.
# Here we are creating a linear model of the data after removing the No.SalesCampaigns, MaterialCost,
# RDExpense, No. NewProdToMarket, and No.Cust variables.
rev.lm = lm(SalesAmt ~ Employee.turnover + No.Employees + No.CustComplaints, data=rev)

#see a summary of the model to determine which variables have no significant predictive power
summary(rev.lm)
# Results:
# Residuals:
#Min     1Q Median     3Q    Max 
#-63963 -18036  -1142  10017  93440 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        2669488     295223   9.042 7.29e-09 ***
#  Employee.turnover -6862587    2260213  -3.036  0.00606 ** 
#  No.Employees         20629       9807   2.104  0.04707 *  
#  No.CustComplaints   -25481       3123  -8.160 4.24e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 33750 on 22 degrees of freedom
#Multiple R-squared:  0.995,	Adjusted R-squared:  0.9943 
#F-statistic:  1447 on 3 and 22 DF,  p-value: < 2.2e-16


# In this linear model we can see that all three variables: No.Employees, Employee.turnover, and 
# No.CustComplaints are all good predictors of Sales.
#
# However, after paying closer attention we can see that we have some multiple colinearity
# going on between the No.CustComplaints variable and the Employee.turnover variable.


#Here we look at the correlation between Employee.turnover and Sales
cor(rev$Employee.turnover, rev$SalesAmt)
# Result:
# [1] -0.9585797

# Here we look at the correlation between No.CustComplaints and Sales
cor(rev$No.CustComplaints, rev$SalesAmt)
# Result: [1] -0.9951712

# By looking at the confidence interval we see some weird relationships, like loosing
# a BUNCH of money during employee.turnover periods. We should look at this relationship some more. 
confint(rev.lm)
# REsults:
#                           2.5 %      97.5 %
# (Intercept)        2.057233e+06  3281742.48
# Employee.turnover -1.154998e+07 -2175193.22
# No.Employees       2.913717e+02    40967.32
# No.CustComplaints -3.195644e+04   -19004.70


# Now we are going to create some new models in order to compare the F-stats
# Here we compare the No.CustComplaints and Sales
ModelA=lm(SalesAmt ~ No.CustComplaints, data=rev)

# Here we compare the No.CustComplaints + Employee.turnover to Sales
ModelB=lm(SalesAmt ~ No.CustComplaints + Employee.turnover, data=rev)

# Now we complete an analysis of variance and determine that ModelB is the best model we 
# can use due to its F-stat being the highest when compared to other models.
anova(ModelA, ModelB)
# Result:
# Analysis of Variance Table
# Model 1: SalesAmt ~ No.CustComplaints
# Model 2: SalesAmt ~ No.CustComplaints + Employee.turnover
# Res.Df        RSS Df  Sum of Sq      F   Pr(>F)   
# 1     24 4.7873e+10                                 
# 2     23 3.0097e+10  1 1.7776e+10 13.585 0.001223 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Now we are going to test with a Non-nested model to see any correlations.
# We are not looking for any variables squared because we can infer from the pairs graph
# that each of these variables has a fairly linear realtionship with Sales
ModelC = lm(SalesAmt ~ No.Employees + Employee.turnover, data=rev)

# Now preform the AIC command to see which model is the best out of our set. 
AIC(ModelA, ModelB, ModelC)
# Result: 
#   df      AIC
# ModelA  3 634.4616
# ModelB  4 624.3941
# ModelC  4 655.8454

# It turns out that ModelB is our best model to use. 

# Now lets look at a summary of ModelB to better understand why it is a good model. 
summary(ModelB)
# Result:
# Call:
# lm(formula = SalesAmt ~ No.CustComplaints + Employee.turnover, 
#   data = rev)
#
# Residuals:
#   Min     1Q Median     3Q    Max 
# -70763 -22718  -1272  19233 110708 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        3288034      28249 116.395  < 2e-16 ***
#  No.CustComplaints   -30975       1835 -16.884 1.87e-14 ***
#  Employee.turnover -8430098    2287215  -3.686  0.00122 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 36170 on 23 degrees of freedom
# Multiple R-squared:  0.9939,	Adjusted R-squared:  0.9934 
# F-statistic:  1887 on 2 and 23 DF,  p-value: < 2.2e-16


# By looking at this summary we can be sure that the error will only be +/- 36,170.
# And we are 99.34% sure of this. 


# Here we are renaming ModelB to use for future analysis.
rev.lm=lm(SalesAmt~No.CustComplaints + Employee.turnover, data=rev)

# Now we are going to do some predictions with our new model.
# Here we will create a new data frame containing our data we want to predict.
new.obs=data.frame(No.CustComplaints=40, Employee.turnover=0.0400)

# now we are going to view the new data frame
new.obs

# 95% prediction interval for the observation with given features
predict(rev.lm, newdata=new.obs, interval="predict")
# Result:
#       fit     lwr     upr
# 1 1711848 1629430 1794265

# We are 95% sure that with these given features, the Sales will at least be $1,629,430 
# and the Sales will at most be $1,794,265. Generally most people will float between the two points
# at the $1,711,848 region. 


# Here we will create a new data frame containing our data we want to predict.
new.obs=data.frame(No.CustComplaints=50, Employee.turnover=0.0500)

# now we are going to view the new data frame
new.obs

# 95% prediction interval for the observation with given features
predict(rev.lm, newdata=new.obs, interval="predict")
# Result: 
#       fit     lwr     upr
# 1 1317801 1230085 1405517

# We are 95% sure that with these given features, the Sales will at least be $1,230,085.
# and the Sales will at most be $1,405,517. Generally most sales will float between the two points
# at the $1,317,801 region.
