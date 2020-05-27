# Chapter 3 Walkthrough
# Here I am reading the Ch3_marketing to the marketing data frame
marketing = read.csv("Ch3_marketing.csv", stringsAsFactors = TRUE)
str(marketing)

# After viewing the marketing data frame I realized we need to reorder the 
# pop_density variable
# Refactor the pop_density variable to be in order of low, medium, high
marketing$pop_density = factor(marketing$pop_density, 
                               ordered = TRUE,
                               levels=c("Low", "Medium", "High"))

####################################
# We will now use a Tabular approach to analyze the data
# View a summary of the google_adwords variable
summary(marketing$google_adwords)

# View the same numbers as above except now the mean is missing
fivenum(marketing$google_adwords)

# I am now finding the standard deviation and variance of the variable
sd(marketing$google_adwords)
var(marketing$google_adwords)

# View a summary of the pop_density variable
summary(marketing$pop_density)
######################################
# Anscombe table practice

data("anscombe")
anscombe

sapply(anscombe, mean)
sapply(anscombe, sd)
sapply(anscombe, var)

par(mfrow=c(2,2))
plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)

######################################
# We will now use a Graphical approach to analyze the data
par(mfrow=c(1,1))

# Vizualize the values of 68, 52, and 52 for Low, Medium and High
plot(marketing$pop_density)

# Now we will vizualize the google_adwords variable through
# a boxplot and a histogram
boxplot(marketing$google_adwords, ylab="Expenditures")
hist(marketing$google_adwords, main = NULL)


#######################################

# According to the book, if we look at the difference between the median
# and the mean, this shows us that there is a skewness in this variable
summary(marketing$twitter)

# Because the mean is more than the median, the graph will be right(positively) skewed 
# If the mean was less than the meadian, then the graph would be left(negatively) skewed



# The two dots above the upper whisker indicate two outliers
# in the variable
boxplot(marketing$twitter, ylab="Expenditures", col="gray")

# The histogram shows a great example of a right skewness within this variable
hist(marketing$twitter, main=NULL, col="blue")

# look at a summary of the marketing data set
summary(marketing)

# Here I add a variable to the data set by using the cut() function.
# I take the marketing$employees variable, cut it into two pieces and then create
# a new column with that information. The cut is done directly in the middle 
# between the min(3) and the max(12)
marketing$emp_factor = cut(marketing$employees, 2)

# We use tabular exploration through the table() function 
# to see a relationship b/t the two variables
table(marketing$emp_factor, marketing$pop_density)

# We use graphical exploration to see this relationship differently

# Use the mosaicplot() function to graph two categorical factors within a 
# table() object. This shows the above table we created.
mosaicplot(table(marketing$pop_density, marketing$emp_factor),
           col=c("grey", "black"), main="Factor / Factor")

# Here we plot a categorical factor with a numeric variable.
# This graph shows why we needed to order factor the pop_density variable
# The titles would have been out of order had we not done that
boxplot(marketing$marketing_total ~ marketing$pop_density,
        main="Facotr / Numeric")

# x value comes first, then the y value comes second when creating a scatterplot
plot(marketing$google_adwords, marketing$revenues,
     main="Numeric / Numeric")

# to determine if there is a correlation between two variables,
# we can use the cor() function.There can be negatively correlated,
# relationships, however both examples below are positively correlated.

# this example shows a strong correlation because it approaches 1,
# 1 means there is definitely a correlation b/t the variables
cor(marketing$google_adwords, marketing$revenues)

# this example shows a weak correlation because it approaches 0,
# 0 means there is no correlation b/t the variables
cor(marketing$google_adwords, marketing$facebook)

# Here we preform a t-test to find out more information
# about the correlation between the two variables
cor.test(marketing$google_adwords, marketing$revenues)

# create two values based off of information on pg. 59
cheese = c(9.3, 9.7, 9.7, 9.7, 9.9, 10.2, 10.5, 11, 10.6, 10.6)
degrees = c(480, 501, 540, 552, 547, 622, 655, 701, 712, 708)

# see if there is any correlation b/t the two values
cor(cheese, degrees)

# run a t-test to see the actual math behind this correlation
cor.test(cheese, degrees)

# there is not a significant correlation between google_adwords and facebook
cor.test(marketing$google_adwords, marketing$facebook)

# The confidence interval on this data is very tight, indicating that 
# i need to graphically explore this relationship to better understand it
cor.test(marketing$marketing_total, marketing$revenues)

# The below plots reinforce the correlations that we tabularly explored above

# we can see that there is a strong correlation b/t these two variables
plot(marketing$google_adwords, marketing$revenues)

# we can see that there is a weak correlation b/t these two variables
plot(marketing$google_adwords, marketing$facebook)

# we can see that there is a very strong correlation b/t these two variables
plot(marketing$marketing_total, marketing$revenues)

# Now we remove the temporary variable from the data set
marketing$emp_factor = NULL

summary(marketing)

# using the pairs() function on the marketing data set, we can see
# every pair of data and a scatterplot describing the information 
pairs(marketing)

# using the cor() function and the first six variables in the data set, 
# we can see any correlations between the pairs of data
cor(marketing[ ,1:6])

# this prints the same thing as above, but with cleaner numbers
# and it also prints out the p-values as well
library(psych)
corr.test(marketing[ ,1:6])

# Here we create a correlogram, the order is false in order to keep things
# the way we see them in the data set. the lower panel we can see the 
# confidence intervals and correlation coefficients.
library(corrgram)
corrgram(marketing[ ,1:6], order=FALSE,
         main="Correlogram of Marketing Data, Unordered",
         lower.panel=panel.conf, upper.panel=panel.ellipse,
         diag.panel=panel.minmax, text.panel=panel.txt)

# If we change the order to TRUE, then we will see the stronger correlations
# in the top left of the graph and as we go diagonally down the 
# correlations begin to weaken. 









