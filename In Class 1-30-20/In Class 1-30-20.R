# In this data analysis we are examining the "bikes.csv" data set
# First we copy the data set to our new project folder
# Then we create a new script file 
# Then we read in the data set as follows:

MyData = read.csv("bikes.csv")

# examine the headers in the data set using the ls() command
ls(MyData)

# And we look at the first 6 rows in our data set using the head() command
head(MyData)

#look at a summary of the data
summary(MyData)

# remove any zer values existing in the data set
CleanData= factor(MyData)

# see if there are any rows that have missing values
# using the complete.cases() command
MyData[!complete.cases(MyData), ]

# If we happened to be missing data then we will use the 
# na.omit() command to delete any missing values 
CompleteData = na.omit(MyData)

# Additional descriptive statistics methods used to analyze specific columns
mean(MyData$Count)
max(MyData$Count)
min(MyData$Count)
median(MyData$Count)
mode(MyData$Count)
var(MyData$Count)
sd(MyData$Count)
IQR(MyData$Count)

# we can also get descriptive statistics using the psych library
# and the command describe()
install.packages("psych")
library(psych)

describe(MyData)

# we can calculate correlations using "normal" pearson correlations
# for interval data
install.packages("Hmisc")
library(Hmisc)

# Pearson correlations are for continuous data
rcorr(MyData$Count,MyData$Temperature, type="pearson")

# we can calculate Spearman correlations by the command rcorr()
# Spearman Correlations are for categorical data
rcorr(MyData$Count,MyData$Month, type="spearman")

# we can also calculate the correlations for a whole data frame
rcorr(as.matrix(MyData))

# A quick way to analyze correlations is to use the command corrgram().
# This command exists in the corrgram library, so you have to install 
# it first
install.packages("corrgram")
library(corrgram)

corrgram(MyData, order=TRUE, lower.panel=panel.shade,
        upper.panel=panel.pie, text.panel=panel.txt,
        main="Bicycle Data in PC2/PC1 Order")

# A nice way to explore relationships is to crossplot the data.
# This command exists in the corrgram library

corrgram(MyData, order=TRUE, lower.panel =panel.ellipse,
         upper.panel =panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Bicycle Data - Correlation and plotted relationships")

# We can also use a scatterplot to analyze data further
plot(MyData$Temperature, MyData$Count, main = "Temperature Vs. Number of Bikes",
     xlab="Temperature", ylab="Number of Bikes", pch=3)

# We can now overlay the scatterplot with a smoothing line.
# The lowess function performs a computation for the lwoess smoother
# between the items being plotted. (PS! lwd = line width)

lines(lowess(MyData$Temperature, MyData$Count), col="blue", lty=1, lwd=4)

# Now we can overlay our graph witha simple regression (prediction line)
# We will cover more on this in later class periods
abline(lm(MyData$Count~MyData$Temperature), col="red", lwd=3, lty=5)

# We can see the coefficients used for the line
lm(MyData$Count~MyData$Temperature)





