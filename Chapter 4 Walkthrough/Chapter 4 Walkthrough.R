# Read in the data and save it to the adverts object

adverts = read.csv("Ch4_marketing.csv")

# view the data using the str() command

str(adverts)

# Run the pairs commmand to view relationships amongst the variables

pairs(adverts)

# Create a scatterplot to view the marketing_total and revenues relationship

plot(adverts$marketing_total, adverts$revenues, ylab='Revenues', xlab='marketing total',
     main='revenues and marketing')

# Create the first simple linear regression on the above variables

model1 = lm(revenues~marketing_total, data=adverts)

# The provided number under intercept is the first intercept in the graph 
# the provided number under marketing_total is a slope for the data to follow

model1


# Keep this prediction equation in mind:
#
# Revenue = 32.0067 + (0.05193 * marketing_total)


# Now create graphs to help with the test for normality
# We look at the residuals of the model to complete these tests

par(mfrow = c(1, 2))
hist(model1$residuals, xlab='Residuals', ylab='Frequency', col='gray',
     main='Residuals Distribution')
qqnorm(model1$residuals, main='Q-Q Plot of Residuals')
qqline(model1$residuals)


# Create a graph that tests for equal variance
# Place the reference line at the mean: 0. And place the bounds at 6.5, show this by using a dotted line

plot(model1$fitted.values, model1$residuals, ylab='Residuals', xlab='Fitted Values',
     main='Residuals Distribution')
abline(0, 0, lwd = 3)
abline(h = c(-6.5, 6.5), lwd = 3, lty = 3)


# interpret the model's output

summary(model1)

# We are now able to predict revenues within the marketing total's range

range(adverts$marketing_total)

# Lets attempt to predict revenues if we spend $460,000 on marketing

library(dplyr)
select(adverts, marketing_total) %>% filter(marketing_total > 430)

# We know that there is not a value at 460 in the data, so we must predict this value.

newdata = data.frame(marketing_total = 460)
predict.lm(model1, newdata, interval = 'predict')

# With 95% confidence, we can say that the marketing total will fall somewhere between
# $49,757 and $62,030.

# Lets now predict with 99% confidence

predict.lm(model1, newdata, level = 0.99, interval='predict')

# Notice how the interval got wider...
# With 99% confidence, we can say that the marketing total will fall somewhere between 
# $47,796 and $63,991.

# What if we want to predict multiple input values at 95% confidence level

newdata = data.frame(marketing_total = c(450, 460, 470))
predict.lm(model1, newdata, interval = 'predict')

# Imagine we are dealing with big data and we are unable to look at the entire dataset
# We are now going to practice sampling and buildng linear models off of these samples

set.seed(4510)
market_sample = sample_frac(adverts, 0.30, replace = FALSE)

# set a randomization seed so we can get the same results. Then use the sample_frac command
# to choose 30% of the data set. use replace = FALSE so that way the samples do not get put back after 
# they have been used.

samp_model = lm(revenues~marketing_total, data = market_sample)
samp_model

# The returned values are close to the values found in model1, but are not exactly the same.
# Now use the confint() command to learn about the population estimates while accounting for 
# sampling uncertainty

confint(samp_model)

# This means that we are 95% confident that the slope of the marketing total in the population will 
# fall between 0.03644 and 0.05515. We know this to be true because the actual value is 0.05192.


# Create a simple dataset to learn how to refine data for SLR

x0 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y0 = c(1.00, 1.41, 1.73, 2.00, 2.24, 2.45, 2.65, 2.83, 3.00, 3.16)
fit0 = lm(y0~x0)

# Preforme the LINE tests on the model. Linearity, Normality, & Equal Variance.

par(mfrow = c(1, 3))
plot(x0, y0, pch = 19, main='Linearity?')
abline(fit0)
hist(fit0$residuals, main='Normality?', col='gray')
plot(fit0$fitted.values, fit0$residuals, main='Equal Variance?', pch = 19)
abline(h = 0)

# We can see that the data looks okay on the Linearity graph, but on the Normality and 
# Equal Variance graphs there are some major issues. This means that we need to transform the data

# This dataset is easy because the y0 set is the square root of the x0 set

y0_t = y0^2
fit0_t = lm(y0_t~x0)

plot(x0, y0_t, pch=19, main="Linear")
abline(fit0_t)
hist(fit0_t$residuals, main='Normal', col='gray')
plot(fit0_t$fitted.values, fit0_t$residuals, main='Equal Variance', pch = 19)
abline(h = 0)

# After transforming the dependent variable, the normal and equal variance graphs
# now seem to pass the test. We also can tell that the lienar graph is much more accurate.

# There is a library in R called MASS that can help transform data. Specifically the 
# boxcox() command in this library is helpful. This command only works on dependent variables.

# We will now use this command on the previous data set that did not pass the tests

library (MASS)
boxcox(fit0)

# Now we will create new data where the boxcox() command will not work in data transformation. 

x1 = c(1, 5, 15, 30, 60, 120, 240, 480, 720, 1440, 2880, 5760, 10080)
y1 = c(0.84, 0.71, 0.61, 0.56, 0.54, 0.47, 0.45, 0.38, 0.36, 0.26, 0.2, 0.16, 0.08)
fit1 = lm(y1~x1)

plot(x1, y1, pch = 19, main = 'Linearity?')
abline(fit1)
hist(fit1$residuals, main='Normality?', col = 'gray')
plot(fit1$fitted.values, fit1$residuals, main = 'Equal Variance?',
     pch = 19)
abline(h = 0)

# After looking at the first graph it is known that there are severe non-linearity issues
# with this data set. We cannot use the boxcox() command because it does NOT work with 
# independent variables. 

# After looking at the logarithnmic pattern in the scatter plot, a good place to begin data
# transformation will be using the log() command on the x value

x1_t = log(x1)
fit1_t = lm(y1~x1_t)

plot(x1_t, y1, pch = 19, main = 'Linear')
abline(fit1_t)
hist(fit1_t$residuals, main='Normal', col = 'gray')
plot(fit1_t$fitted.values, fit1_t$residuals, main = 'Equal Variance?',
     pch = 19)
abline(h = 0)

# After this logarithmic change we can see that the data passes all of the tests. 
# Data transformation is a lot of trial and error/ experience and research....

# Now we change to outlier recognition and removal
# Create a data set and determine if there is an outlier

x4 = c(1:20)
y4 = c(0.4, 2.2, 2.2, 5.6, 5.3, 5.2, 7.5, 8.7, 9.6, 9.7, 12.5, 12.4, 12.4, 12.8, 16.1, 16.0, 
         17.0, 11.5 19.8, 20.6)
fit4 = lm(y4~x4)

plot(x4, y4, pch = 19)
abline(fit4)

# After creating the scatterplot and plotting the linear model we can infer that at point
# (18, 11.5) there is an outlier. Because of this, we will remove both points for the data

x4_t = c(1:19)
y4_t = c(0.4, 2.2, 2.2, 5.6, 5.3, 5.2, 7.5, 8.7, 9.6, 9.7, 12.5, 12.4, 12.4, 12.8, 16.1, 16.0, 
       17.0, 19.8, 20.6)
fit4_t = lm(y4_t~x4_t)

plot(x4_t, y4_t, pch = 19)
abline(fit4_t)

# Upon a review of the new data we can see a slight change in the graph and we notice that
# there is a better linear relationship amongst the data

# to prove that there are changes and that the second data set is more accurate, we look
# at a summary of both linear models and compare the residuals, slopes, adjusted r-squared values, 
# and the F-stats

summary(fit4)
summary(fit4_t)

# We can use Cook's Distance to help determine if there is an outlier in the data.
# To access this use the plot() command with the model in the parenthesis and view the 
# fourth graph.

# We know that a number is suspicious when it has a Cook's Distance > 0.5.
# We know that a number is likely influential if it has a cook's Distance > 1.0.
# We know that a number is influential if its Cook's Distance stands out from all the others.

plot(fit4)

# Next we shift to multiple linear regression. 
# Lets take a look at revenues predicted by google_adwords, 
# facebok, and twitter all at once.

model2 = lm(revenues~google_adwords + facebook + twitter, data = adverts)

# We ahve just learned about the plot() function on the model so we can use that to 
# complete our LINE tests.

plot(model2)

# We now want to view the summary of the model we jsut created to better understand the data
# It looks like the twitter variable is not a good predictor of revenues when paired up with
# facebook and google_adwords. However, the other two variables seem to be extremely good 
# predictors. 

summary(model2)







