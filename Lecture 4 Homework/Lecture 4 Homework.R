###### EDA Assignment - Andrew Reeves & Brett Voglesang ######

# Here I am reading the file into a dataset
CruiseData=read.csv("cruise.csv")

# Now I will preform Exploratory Data Analysis on the dataset

# Here I am viewing the entire dataset
# I found this to be the most informative when first
# trying to understand the dataset
View(CruiseData)

# Here I am viewing a summary of the dataset
summary(CruiseData)

# Here I view the first six rows
head(CruiseData)

# I am checking if there are any errors within the dataset
CruiseData[!complete.cases(CruiseData), ]

# Now we know the data is clean and there are no zeros 

# Here I am calculating and showing the average number 
# of passengers for each crew line 
by(CruiseData$passengers, CruiseData$cline, mean)


# Now I am implementing the psych library to learn some descriptive
# statistics about the dataset
library(psych)

# I use the describe() command to show all major descriptive
# statistics regarding the dataset
describe(CruiseData)

#this gives us some information about the age column 
max(cruise$age)       #Result: 48
min(cruise$age)       #Result: 4
range(cruise$age)     #Result: 4-48
mean(cruise$age)      #Result: 15.68987
median(cruise$age)    #Result: 14
var(cruise$age)       #Result: 57.99875
sd(cruise$age)        #Result: 7.615691
IQR(cruise$age)       #Result: 10

#this gives us some information about the tonnage column 
max(cruise$tonnage)       #Result: 220
min(cruise$tonnage)       #Result: 2.329
range(cruise$tonnage)     #Result: 2.329-220
mean(cruise$tonnage)      #Result: 71.28467
median(cruise$tonnage)    #Result: 71.899
var(cruise$tonnage)       #Result: 1386.039
sd(cruise$tonnage)        #Result: 37.22954
IQR(cruise$tonnage)       #Result: 44.7595

#this gives us some information about the passdens column 
max(cruise$passdens)       #Result: 71.43
min(cruise$passdens)       #Result: 17.7
range(cruise$passdens)     #Result: 17.7 - 71.43
mean(cruise$passdens)      #Result: 39.90095
median(cruise$passdens)    #Result: 39.085
var(cruise$passdens)       #Result: 74.63607
sd(cruise$passdens)        #Result: 8.639217
IQR(cruise$passdens)       #Result: 9.615


# I am now implementing the corrgram library to better understand
# the relationships between the dataset
library(corrgram)

# Here we see a corrgram graph using ellipse and scatter plot graphs
# to convey the correlations between the data
corrgram(CruiseData, order=TRUE, lower.panel=panel.pts,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Cruise data - Correlations")

# Here we see a corrgram graph using pie and shade graphs 
# to conver the correlations between the data
corrgram(CruiseData, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Cruise Data in PC2/PC1 Order")

# I am now implementing the Hmisc library to preform
# Pearson correlation calculations on the dataset 
library(Hmisc)

# By using this pearson correlation technique I conclude that
# the general age of the crew is early adult around 18-28 years old
rcorr(CruiseData$crew,CruiseData$age, type="pearson")

#this calculates spearman correlations
rcorr(CruiseData$tonnage, CruiseData$passdens, type="spearman")

# based on the graph and the line drawn below we can conclude that
# the relationship between the age of ship and number of crew members 
# shows that the older the ship, the less likely it is to have a crew, and
# the younger the ship, it is more likely to have a crew ranging from 5-15 people

# This graph also prove my above pearson correlation 
plot(CruiseData$crew,CruiseData$age, xlab="Number of Crew Members",
     ylab="Age of Ship",
     main="Relationship between amount of crew members and ship age", pch=11)
lines(lowess(CruiseData$crew,CruiseData$age), col="red", lty=1,
      lwd=5)

# create a graph showing the number of passengers on each cruise line within
# the data set 
plot(CruiseData$cline, CruiseData$passengers, xlab="Name of Crewship",
     ylab="Number of Passengers", las=3, cex.names=.04,
     main="Relationship between number of passengers on each crewship",
     pch=4)
lines(lowess(CruiseData$crew,CruiseData$passengers), col="blue", lty=1,
      lwd=4)

#plot the data in a scatterplot 
plot(CruiseData$tonnage, CruiseData$passengers, 
     main = "Tonnage Vs. Number of Passengers", 
     xlab="Tonnage", ylab="Passengers", pch=20)
lines(lowess(CruiseData$tonnage,CruiseData$passengers), col="blue", lty=1,
      lwd=4)

# Through this calculation below, we determine that 
# every ship's passenger density is determined by 
# the tonnage mulitplied by 1000, then divided by the number of passengers
# multiplied by 100
(CruiseData$tonnage * 1000) / (CruiseData$passengers * 100)


