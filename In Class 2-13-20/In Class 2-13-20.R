# We read in ice cream data
Icecream = read.csv("icecream.csv") 

# We can now look at the summarized information about the data frame
summary(Icecream)

# We can also run the regression and store the properties in a data frame
# using this nwe syntax: 
icecream.lm = lm(cons ~ temp, data=Icecream)

# Then we can view the model in the data frame using the summary command:
summary(icecream.lm)

# We can see the coefficients as a vector
# For the sample
coef(icecream.lm)  

# Or we can see the confidence interval estimate for the intercept and slope:
# For the population
confint(icecream.lm)

# The we can create a simple plot of ice cream consumption versus 
# temperature
plot(Icecream$temp, Icecream$cons,
     xlab="Temperature", ylab="Ice cream consumption")

# and use the coefficients as an argument to the abline function
# to place the model onto the plot
abline(coef(icecream.lm), col="green", lwd=3, lty=3) 

# Now let us predict the ice cream sales when temperature is 50 degrees
# Since we want to be able to use this over and over again with different
# degrees, we are going to crate a new variable to store the input in
new.obs = data.frame(temp=80)

# Now we can call the variable called 'new.obs' and use it to predict the
# ice cream sale regardless of what what value it has:

# First we can do a simple point predicton of sales 
# 0.36223 pints of ice cream per person
predict(icecream.lm, newdata=new.obs) 

# We can also predict sales with a 95% confidence interval
# "i'm 95% sure that we will at least sell .3623 pints per person
# and at most we will sell 0.5485 pints per person"
predict(icecream.lm, newdata=new.obs, interval="predict") 

# To get started, we download the Real_Estate_sample.cvs
# file and load it into a data frame we call 're'
re = read.csv("Real_Estate_Sample.csv")
head(re)

# Then we can create a linear model between price and
# living area 
HouseSizeModel = lm(Price ~ Living.area, data=re)

# And we can look at the relationship between living 
# area and price and plot the model on this graph
plot(re$Living.area, re$Price)
abline(coef(HouseSizeModel), col="blue", lwd=4, lty=5)

# We can also see the model and/or coefficients:
summary(HouseSizeModel)
coef(HouseSizeModel)

# Now we re-run the model with a new quadratic formula
# to see if the relationship in non-linear
HouseSizeModel2 = lm(Price ~ Living.area+ I(Living.area^2), data=re)

# after looking at the summary, we want to throw away the Living.area
# variable and accept the I(Living.area^2) variable instead
summary(HouseSizeModel2)

# We can also graph the data and the predictive price for
# houses between 500 and 5000 square feet
plot(re$Living.area, re$Price)
new.obs = data.frame(Living.area=500:5000)
lines(500:5000, predict(HouseSizeModel2, newdata = new.obs), col="red", lwd=4, lty=5)

# ----------------------------------------------------
#############  MULTIPLE REGRESSION #################

# Let us work with a new set of data. Download the file called 
# Real_Estate_Sample.csv to your project folder and read in the file
# to a new data frame we will call 're'
re = read.csv("Real_Estate_Sample.csv")

# Now, lets look at the summary data
summary(re) 

# We will also look at the relationships in the data set with 
# the 'pairs() command that we covered before:
pairs(re, pch='.')  

# Now we can try crating a new regression model to predict the house price 
# based on all the other variables and see if they are good predictors
re.lm = lm(Price ~ Living.area + bedrooms + bathrooms + year + garage, data=re)
summary(re.lm)

# Now we have removed the ‘poor’ predictors that had 
# p values over 0.05 (we are building at a 95% confidence level). 
# So we removed the vairables ‘year’ and garage’ from the regression model.
re.lm = lm(Price ~ Living.area + bedrooms + bathrooms, data=re)

# Bedrooms estimate being a negative value is a sign of colinearity in the data
summary(re.lm)

# Now we want to look at the correlation between Price and # of Bedrooms 
# with negative estimated coefficient - just to check
cor(re$bedrooms, re$Price)

# Positive correlated items should not have a negative coefficient

# confidence intervals for coefficients
confint(re.lm)

# Lets use the model and create a prediction and prediction intervals 
# for a house we want to buy
new.obs = data.frame(Living.area=3000, bedrooms=3, bathrooms=2)

# see new data
new.obs

# 95% prediction interval for observation with given features
predict(re.lm, newdata = new.obs, interval="predict")

# -----------------------------------------
#######  COMPARING MODELS #####

# First we create two regression models with different
# number of predictor vaiables
modelA = lm(Price~bedrooms, data=re)
modelB = lm(Price~bedrooms + bathrooms+ Living.area, data=re)

# Since Model B is a nested model, we should compare them using F statistics
anova(modelA, modelB)

# Now we add a third model with some variables we think are 'non-linear'
modelC = lm(Price~ Living.area +I(Living.area^2) +bathrooms +I(bathrooms^2), data=re)

# Since Model C is a not a nested model, we should compare the models using AIC statistics
# The lowest AIC score indicates the best model
AIC(modelA, modelB, modelC)



