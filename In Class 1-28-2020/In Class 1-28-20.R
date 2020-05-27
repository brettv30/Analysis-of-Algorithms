# First we read in the file
bollywood =read.csv("bollywood_boxoffice.csv")

# Then we create a histogram with a size of the bar of 50
hist(bollywood$Gross, breaks=50)

# We can also force the number of bars by analysing the dataset
# and force the bars into equal sizes

hist(bollywood$Budget, seq(min(bollywood$Budget), max(bollywood$Budget), length.out = 11))

# Another type of graph in R is the barplot
barplot(bollywood$Gross)

# We can also create a whole new data set and sort it using the "order" command
# The negative symbol before the variable, means the data is sorted decending.
# If you don't add that, it will be asending by default.  You will also have to 
# tell the 'order' command what to do with the other two columns. Since we just want
#to copy them to the new dataset, we simply add a couple of commas at the end.
SortedGross = sort(bollywood$Gross)
barplot(SortedGross)

# Here we flip the graph that was determined from the above commands
Mine = bollywood[order(-bollywood$Gross),,]
barplot(Mine$Gross)


# Another graph in R is the scatterplot
plot(bollywood$Gross, bollywood$Budget)

# reading in a new file to data frame called airline
airline = read.csv("airline2008Nov.csv")

# We can also create meaningful barplots using tables.  
# In this case we want to graph how many flights each carrier 
# has in the data set.  We start by placing the data in a table 
# and then graph the table.

table(airline$UniqueCarrier)
barplot(table(airline$UniqueCarrier))

# create a graph of a specific type: l, S, s, b,c 
plot(airline$DepDelay, airline$ArrDelay, type = "l")

# We can analyze histograms by controlling both the breaks and the 
# range on the axis of what we want to display using the xlim/ylim commands. 

# In this example we want to look at the number of flights that 
# are delayed between 30 minutes and 3 hours (180 min) and we want to see
# the number of flights between 0 and 4,000 (ylim)

hist(airline$ArrDelay, breaks=50, xlim=c(30,180), ylim=c(0,4000))

# An example of using some commands for histograms
hist(airline$DayOfWeek, main="Delays by Weekday", xlab="Day", col=c ("red","blue","purple"), border="Black", las=1, breaks=8)

# another type of graphs are boxplots
boxplot(bollywood$Budget)

# We can also create box plot on two variables, add some labels and control colors

boxplot(airline$ArrDelay ~ airline$DayOfWeek, data = airline,  xlab = "Day of week", 
        ylim=c(0,400), ylab = "Minutes of Delay", main = "Arrival Delay by Day", 
        col=c("pink", "yellow"), border="blue", las=1)


# Another type of graph is piecharts. Since piecharts does not work wel with large 
# number of variables, we are simply reducing the dataset to only a few airports

smalldata = subset(airline, airline$Dest == "ATL"| airline$Dest=="JFK")

# NOTE: the order of the conditioning above (ATL/JFK) has to be in alphabetical order

# we can now look at the data
table(smalldata$Dest)

# remove all zeros from the data frame
table(factor(smalldata$Dest))

#notice that we have 261 airports with 0 data at the bottom of the new dataset

# We can now remove zero values from the other airports from the dataset using the factor command
factor(smalldata$Dest)

# We create a piechart of categorical data
pie(table(factor(smalldata$Dest)))

# We can also create 3D piecharts and seperate the wedges if we install some of 
# cool packages available

install.packages("plotrix")
library(plotrix)
pie3D(table(factor(smalldata$Dest)), col=c("yellow","Green"), explode=0.1,
      main="Number of Dealyed Flights by Airports", labels=c("ATL", "JFK"))

#Another way to make useful piecharts is to first aggregate the data
aggregate(airline$DepDelay, list(airline=airline$UniqueCarrier), FUN=mean)

# we now have average departure Delay by each unique airline carrier
# we can store this in a summarized data frame called "mydata"

mydata = aggregate(airline$DepDelay, list(airline=airline$UniqueCarrier), FUN=mean)

# CREATING LABELS FOR PIE CHARTS
# We can create a label as well. First we want the airline. Then we add the average
# minutes each airline is delayed (x). Since this number has a lot of decimals, we 
# simply round it to nearest minute. Then we add the text "min" to the label

lbls = mydata$airline
lbls = paste(lbls, round(mydata$x))
lbls = paste(lbls, "min")

# Now we can display the aggregated data in a pie chart 
# Notice that the command cex=1, controls the size of the text
# in the pie chart
pie(mydata$x, labels=lbls, main="Average Delay by Airlines", cex=1)

############ COMPLEX GRAPHS##############################
# We now want to crate a complex graph with plots and colored histograms
# We want to show the budget for the movies, as well as a cumulative
# spend by the movie industry in Bollywood. First we calculate the 
# cummulative sum using the cumsum command

IndustryBudgetSum= cumsum(bollywood$Budget)

# Now we create a histogram with a size of the bar of 50
hist(bollywood$Budget, breaks=15, col=c("Red", "green"), 
     main="Budget of Movies and Cummlative Costs Mill Rupees", cex=2, las=1)

# We now want to overlay the graph with a new plot
par(new=TRUE)

# Here we crate a new plot with no data labels in the axes
plot(IndustryBudgetSum, xlab="", ylab="", axes=FALSE, col="Blue", cex=1)

# Here we ask for the labels for the last graph to displayed to the right. 
# position 1 and 2 are normal positions, 3 places x-axis labels on the top
# of the graph and 4 places the y axis on the left
axis(4)

# We now will make a simple lineplot of arrival and departure delays
par(mfrow=c(1,1))
line(, type="l")
plot(airline$DepDelay, airline$ArrDelay,type = "l",col = "blue", xlab = "Departure Delay", ylab = "Arrival Delay", 
     main = "Delays in Minutes per Flight")

# We can add more controls in the linechart by these commands
Age = c(13,14,16,22,29,49)
Income = c(5000,9000,16000,24000,39000,52000)
Fortune = c(7000,15000,17000,27000,47000,67000)

par(mfrow=c(1,1))
par(pch=20, col="green")

plot(Age,Fortune, type = "l",col = "blue", xlab = "Age", 
     ylab = "Income", main = "Age Vs. Income and Fortune") 
lines(Age,Income, type="o", col = "red")

plot(Income,Fortune, type = "l",col = "blue", xlab = "Income", 
     ylab = "Fortune", main = "Income Vs. Fortune") 

# CREATING A GRAPH AND SAVING IT TO AN IMAGE

# Step-1: Create Image
png(file = "Income Vs. Age.jpg")

# STEP-2: Create Graph
plot(Age,Fortune, type = "l",col = "purple", xlab = "Age", 
     ylab = "Income", main = "Age Vs. Income and Fortune") 

# STEP-3: Saving the file and closing connection
dev.off()

######### ANALYZING DATA WITH SIMPLE COMMANDS AND GRAPHS #############
# In this example we are using the glassdoor data from Canvas
MyData = read.csv("glassdoordata.csv")

# STEP-1 we look at available variables
ls(MyData)

# STEP-2 Then we look at some sample data
head(MyData)

# STEP-3 we can view the data in a table (if the dataset is not too large)
View(MyData)

# STEP 4: Now we can look at summary descriptive statistics
summary(MyData)

# STEP-5: Analyzing distribution of data using a boxplot
boxplot(MyData$income ~ MyData$gender, data = MyData,  xlab = "Gender", 
        ylab="", main = "Income Distribution by Gender", 
        col=c("yellow", "brown"), border="purple", yaxt='n')
axis(2, at=seq(0,180000,10000), las=1, cex=1 )

# STEP-6 Basic Descriptive Statistics Commands
# Standard Deviation
sd(MyData$income)

# Variance
var(MyData$income)

# Quartile range
IQR(MyData$income)

# Range
range(MyData$income)

# STEP-7: Checking normality by the package called 'moments'
install.packages("moments") 
library(moments)
skewness(MyData$income)
kurtosis(MyData$income)

# STEP-8 Analyze Mean Income by Job Title
by(MyData$income, MyData$jobtitle, mean)

# STEP-9: Graphing Mean Income by Gender and Job Title

AvgInc= by(MyData$income, list(MyData$gender,MyData$jobtitle), FUN=mean)

barplot(AvgInc, main="Income by Job Title (red=female, blue=male, )", ylab="", yaxt='n', 
        cex.names=0.6, col=c("red","darkblue"),beside=TRUE)

axis(2, at=seq(0,150000,20000), las=1, cex=1 )

