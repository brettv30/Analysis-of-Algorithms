# Chapter 6 Walkthrough

# Call the TSA library because we will run a time series analysis on this data first

library(TSA)

# Create the airpass data frame then preform some basic EDA on the data

data(airpass)

str(airpass)

summary(airpass)

# Extract the passenger volume and time as matrixes and store them in vectors

volume = as.matrix(airpass)

time = as.matrix(time(airpass))

# Create a data frame that combines both vectors into a data set and name each column

airpass_df = as.data.frame(cbind(volume, time))
colnames(airpass_df) = c("volume", "time")


# Create a linear model using the time data to predict the volume

lmfit = lm(volume~time, data = airpass_df)
summary(lmfit)

# preform the LINE test on the model. 
# Linearity, Independence, Normality, and Equal Variance

plot(lmfit)

# We can see that the normal distribution is left-skewed and that there is unequal variance 
# This means that the data does NOT pass the Independence, normality, or equal variance tests.

# We are going to plot the 95% confidence interval and prediction interval on the same graph
# to show that the linear model is a poor model to use on data like this.

plot(airpass, main = "95% Confidence and Prediction Intervals of airpass Data")

# plot the line for the linear model onto the graph as a solid blue line

abline(lmfit, col = 'blue')

# create new data that jumps from 1960 -> 1972 every two years

newdata = data.frame(time = seq(1960, 1972, 2))

# Create an object that holds the predicted values from the new data and lmfit model

pred = predict.lm(lmfit, newdata, interval = "prediction")

# This data above is used to create the blue dots on the graphs

points(seq(1960, 1972, 2), pred[ ,1], pch = 19, col = "blue")

# Create the prediction interval for the model in two solid red lines

abline(lsfit(seq(1960, 1972, 2), pred [ ,2]), col = "red")
abline(lsfit(seq(1960, 1972, 2), pred [ ,3]), col = "red")

# create an object that holds the confidence values based on the predictions

pred = predict.lm(lmfit, newdata, interval = "confidence")

# create the confidence interval for the model in two dashed red lines

abline(lsfit(seq(1960, 1972, 2), pred[,2]), lty=2, col = 'red')
abline(lsfit(seq(1960, 1972, 2), pred[,3]), lty=2, col = 'red')

# We now know that even though the linear regression model seemed to be great for the data
# that isn't always the case and we must always test our models. After extensive testing
# we learned that a time series analysis would be much better for forecasting the data. 

library(forecast)

# When forecasting data, using the decompose() command is great for viewing many relatioships
# that occur in the data. This is kind of like the pairs() command from earlier in the book.

plot(decompose(airpass))

# The observed graph is the view of the original data
# The trend graph shows the trend in the data. You see that over time, air passenger volume
# has increased.
# The seasonal graph is the view of the seasonal cycles the data goes through each year.
# The random graph is the view of the randomness in the data. It's like the error term in 
# linear regression. 

# In order to preform time series analysis, there is an assumption that
# data needs to be stationary. Meaning that when looking at graphs, 
# the data cannot display any trends or seasonal components or both of any kind.

# The technique used to transform non-stationary data into stationary data is called
# differencing. 
# Regular differencing subtracts each data point from the data point immediately in 
# front of it in the series
# Seasonal differencing is similar, but it subtracts each data point from its related 
# data point in the next cycle. 

# Build a small sample dataset to view transforming non-stationary data in action

seq_down = c(.625, .125, -0.125)
seq_up = c(0, 1.5, 0.25)
y = c(seq_down, seq_up, seq_down + .75, seq_up + .75, seq_down + 1.5, seq_up + 1.5)

# Create visualizations of each step in the non-stationary data transformation process

par(mfrow = c(1, 3))
plot(y, type = 'b', ylim = c(-.1, 3))
plot(diff(y), ylim = c(-.1, 3), xlim = c(0, 36))
plot(diff(diff(y), lag = 12), ylim = c(-.1, 3), xlim = c(0, 36))
par(mfrow = c(1,1))
detach(package:TSA, unload = TRUE)

# The graph on the far left shows that there is a steadily 
# increasing trend in the data and that there are three cycles. 
# This breaks the stationary data assumption.

# The graph in the center shows the result of differencing done on the first graph. 
# plotting the difference between each point and its next neighbor removes the trend. 

# The graph on the far right shows the results of seasonal differencing with a lag of 12
# The data is now stationary. Notice that we lose a cycle of data in this set. This 
# is due to the (n-lag) results.



# Now we are going to begin working with the dataset provided for this walkthrough

# read in the data

cycle = read.csv("Ch6_ridership_data_2011-2012.csv")

# view the first 6 lines in the data

head(cycle)

View(cycle)

# We want to aggregat the data on a monthly basis so that forecasts from the model 
# provide monthly periods.
# Call the dply and lubridate libraries to help with the data aggregation

library(dplyr)
library(lubridate)

monthly_ride = as.data.frame(cycle %>% group_by(year = year(datetime),
                                                month = month(datetime)) %>%
                               summarise(riders = sum(count)))

# Preform a quality check to show that the output is correct

table(monthly_ride$year, monthly_ride$month)


View(monthly_ride)

# We must now create a time series of the data
# Here we are going to subset to get a single column from the data frame
# Then use the ts() command to create a time series based on the 12 months in a year


riders = monthly_ride[ ,3]
monthly = ts(riders, frequency = 12, start = c(2011, 1))

# View the time series to make sure the data loaded properly

monthly

# Now we use the decompose() command to view the time series

plot(decompose(monthly))

# We can see that there is an increasing trend over time and there are seasonal
# cycles as well.

# Next we are going to run differencing and seasonal difference on the data

par(mfrow = c(1, 3))
plot(monthly, type = 'b')
plot(diff(monthly), type = 'b')
plot(diff(diff(monthly), lag = 12), type = 'b')

# We observe that differencing eliminates the trend and makes the data
# stationary. The seasonal differencing doesn't seem to make it any more stationary. 

# Next use the Auto correlation function (ACF) and the 
# partial autocorrelation function (PACF) on the data to get a sense of type and 
# levels of models to fit the monthly data.

par(mfrow = c(1, 2))
Acf(monthly)
Pacf(monthly)

# Based off of the ACF graph, we can see a slowly-diminishing ACF turning into a cycle.
# This is a clue that the data is AR. The PACF cuts off within the first lag. This
# points towards an AR(1) model with no seasonal component (based on the differencing results)

# We are now going to create five fitted models based on the data observed. 

# The first model will be an AR(1) model that has no seasonal component

fit1 = arima(monthly, c(1, 0, 0), seasonal = list(order = c(0, 0, 0)))
fit1
tsdiag(fit1)


# The second model will be an AR(1), I(1) model that has no seasonal component
# We give I a value of 1 because of the differencing results and that differencing was useful.

fit2 = arima(monthly, c(1, 1, 0), seasonal = list(order = c(0, 0, 0)))
fit2
tsdiag(fit2)

# The third model will be an AR(2), I(1) model that has no seasonal component
# We give AR a value of 2 because of the ACF graph having two lags over the blue line

fit3 = arima(monthly, c(2, 1, 0), seasonal = list(order = c(0, 0, 0)))
fit3
tsdiag(fit3)


# The fourth model will be an AR(1), I(1) model that has a seasonal differencing component

fit4 = arima(monthly, c(1, 1, 0), seasonal = list(order = c(0, 1, 0)))
fit4
tsdiag(fit4)

# The fifth and final model will be an I(1), MA(1) model that has no seasonal component
# We give MA a value of 1 because we want to represent a moving average model 

fit5 = arima(monthly, c(0, 1, 1), seasonal = list(order = c(0, 0, 0)))
fit5
tsdiag(fit5)


# We are going to use the fit2 model because it is simpler than AR(2) and has a slightly better
# AIC.

# Now we run the forecast on that model and set h = 12 for the next 12 data points (months)
# then we will display a graph to show the prediction

library(forecast)
yr_forecast = forecast(fit2, h = 12)
plot(yr_forecast)

# We find that this graph is not helpful because it predicts that there may be a good, bad, or even
# year. The dark shaded region is 80% confidence prediction. The light shaded region is 
# 90% confidence prediciton. 


# Now we are going to use advanced functionality for better modeling of the data
# Due to there being only two years in this data set the forecast wont be that strong. 
# In order to have proper forecasting there needs to be more than 5 years of data.

# We are going to use the tbats() command to create better models.
# Remeber that you cannot do this command all the time! Only do it if the data is 'small' 
# and if the previous forecast is lacking...

monthly_data = tbats(monthly)

year_forecast = forecast(monthly_data, h = 12)
plot(year_forecast)

# Create summaries based on the data so we can better accent the plot 

summary(year_forecast$mean)

summary(year_forecast$upper)

summary(year_forecast$lower)

# Now that we can see the data we need, lets extract it and save it for the visualization

mean_2011 = round(as.numeric(filter(monthly_ride, year == 2011) %>% 
                               summarise(mean = mean(riders))), 0)

mean_2012 = round(as.numeric(filter(monthly_ride, year == 2012) %>% 
                               summarise(mean = mean(riders))), 0)

mean_2013 = round(mean(year_forecast$mean), 0)

max_mean_2013 = round(max(year_forecast$mean), 0)

# Use the variables we just created and place lines on the plot representing the max mean 
# forecast and the mean for each year

abline(h = max(year_forecast$mean), lty = 2, col = 'blue')
segments(2011, mean_2011, x1 = 2012, y1 = mean_2011,
         col = 'darkgray', lty = 2, lwd = 2)
segments(2012, mean_2012, x1 = 2013, y1 = mean_2012,
         col = 'darkgray', lty = 2, lwd = 2)
segments(2013, mean_2013, x1 = 2014, y1 = mean_2013,
         col = 'blue', lty = 2, lwd = 2)

# Add text to the plot to label each of the lines. Add 10000 to the y coordinate to lift it
# off of the line.
# Example text() command: text(x-coordinate, y-coordinate, 'text being displayed')

text(2011.15, mean_2011 + 10000, mean_2011)
text(2012, mean_2012 + 10000, mean_2012)
text(2013, mean_2013 + 10000, mean_2013)
text(2013.85, max_mean_2013 + 10000, max_mean_2013)








