#### Graphics Commands
mydata = read.csv("medicalmalpractice.csv") # read in medical malpractice data
names(mydata)
View(mydata)

pie(table(mydata$Gender),main='Distirbution of Gender')
pie(table(mydata$Severity),main='Distirbution of Severity')

barplot(table(mydata$Specialty),las=2) ###las=2 prints labels vertically

hist(mydata$Age,main="Histogram of Age",xlab="Age")

hist(mydata$Age,main="Histogram of Age",xlab="Age",breaks=20)
hist(mydata$Age,main="Histogram of Age",xlab="Age",breaks=20,col="gray")

plot(mydata$Age,mydata$Amount/100000,xlab="Age",
     ylab="Amount in $100000",main="Amount of Award by Age")


### Summarizing Data Commands
mydata = read.csv("haircut_survey.csv") # read in haircut survey data

## check the names
names(mydata)

## View the data
View(mydata)

# summarize overall and by subgroup
summary(mydata$haircut)

summary(mydata$haircut[mydata$male==0])
summary(mydata$haircut[mydata$male==1])

### or a more advaned command
by(mydata$haircut,mydata$male,summary)

boxplot(mydata$haircut)
boxplot(mydata$haircut~mydata$male)

## bascic summary statistics
median(mydata$haircut)
mean(mydata$haircut)

# Another way to look at data is to find out what the mainstream is spending on
#haircuts.  Here we arbitraily define "mainstream' as those spending between 
# 10% and 90% of the poplulation. We can do that using the quantile() command
quantile(mydata$haircut,c(0.1,0.9))

## again see the by command in action
by(mydata$haircut,mydata$male,mean)

var(mydata$haircut)
sd(mydata$haircut)
IQR(mydata$haircut)

### can summarize an entire data set at once
mydata = read.csv("medicalmalpractice.csv")
summary(mydata)

### Advanced Graphs #######

### density plot
plot(density(mydata$haircut[male==1],bw=11),"Haircut Cost by Gender",xlab="Cost in $")
lines(density(haircut[male==0]),lty=2)
legend(100,.02,c("Males","Females"),lty=1:2)


### scatter plot smoothing
cruise = read.csv("cruise.csv")
plot(cruise$passengers,cruise$crew)
lines(lowess(cruise$passengers,cruise$crew),col="blue")

# We can create 1000 random variable with a known distribution using the command
x = rnorm(1000,mean=140, sd=30)

# and graph it with a historgram and calculate sample statistics
hist(x, breaks=20)
mean(x)
sd(x)


#### First we read in the dataset  again
mydata = read.csv("haircut_survey.csv") 

# Then we can look at the distribution of Amounts spent using a histogram
hist(mydata$haircut,main="Histogram of Amount spent of haircuts",
     xlab="Amount",breaks=10,col="green")

# We can then summarize overall and by subgroups (male vs. female)
summary(mydata$haircut)
summary(mydata$haircut[mydata$male==0]) # Female
summary(mydata$haircut[mydata$male==1]) # Male

# A simpler way of doing the same is the "by" command we 
# covered earlier
by(mydata$haircut,mydata$male,summary)

# Then we can loook at outliers using the boxplot command
boxplot(mydata$haircut, main="Haircut Spending Amounts", 
        ylab="$", las=2)

# We can also look at the subgroups using boxplots
# Here we see amounts by gender (male=1, female =0")
boxplot(mydata$haircut~mydata$male, main="Haircut Spending ($)",
        col=c("red", "blue"), ylab="$", xlab="Female (0) Vs. Male (1)",
        las=2, border="brown")

#### COMPLEX density PLOTS ######
### We can also look at density plot for males
plot(density(mydata$haircut[mydata$male==1],bw=11),col="blue",
     main="Density by Amount spent ($)",xlab="Cost in $", 
     lwd=4, las=2)

# We can also overlay the density graph by adding a line for 
# the females using the lines() function and add a label 
# using the legend() function
lines(density(mydata$haircut[mydata$male==0]),col="red",
      lwd=4,lty=2)
legend(90,.02,c("Males","Females"),lty=1:2)

# As we did before, we can examine if the data looks 
# normally distributed using the commands kurtosis() and 
# skewness(). These functions comes from the library movements, 
# so we have to install it first
install.packages("moments")
library(moments)
by(mydata$haircut,mydata$male,kurtosis)
by(mydata$haircut,mydata$male,skewness)


### OUTLIER TREATMENT ####

# Remember from our histogram, we had a very unusual value for males 
# that looked like an outlier. We can find this value using the 
# which.max() command
mydata$haircut[mydata$male==1][which.max(mydata$haircut[mydata$male==1])]

# We can exclude the variable from further analysis by 
# creating new data sets for males and females fram without it
Haircut.males = mydata[(mydata$male==1) & (mydata$haircut<121),]

# Notice that the data for females were normal, so we just split the 
# data set, but keep all the values for females
Haircut.females = mydata[(mydata$male==0), ]


######## ESTIMATION OF PROPORTION #######
#----------------------------------------

plot(density(Haircut.females$haircut,bw=11),col="red",
     main="Density by Amount spent by Females ($)",xlab="Haircut Cost in $", 
     lwd=4, cex=0.5, las=2)


# pnorm function is extremely helpful when determining ranges. Using 
# logic and some commands is v helpful

# Since we have over 30 females in our sample and the data
# is normally distributed, we we can also estimate the 
# proportion of females who spent under $20 on a haircut 
# using the command pnorm()

# the generic command is pnorm(x, mean, sd) for us that is:
# pnorm shows the left tail from whatever value you're looking at, and downwards
pnorm(20, mean=mean(Haircut.females$haircut), sd=sd(Haircut.females$haircut))


# If I wanted to find out who spent MORE than $20, i would just add the statement
# lower.tail=FALSE and r would return the 'upper tail'
pnorm(20, mean=mean(Haircut.females$haircut), sd=sd(Haircut.females$haircut), lower.tail=FALSE)

# If I wanted to find estimate whjat proportion of females spend between $25 and $30 
# dollars on a haircut I could simply use the lower.tail=TRUE statement and simple subtraction
mean(Haircut.females$haircut)
sd(Haircut.females$haircut)
pnorm(30, mean=44.309, sd=31.36) - pnorm(20, mean=44.309, sd=31.36)

# We know from our sample, the haircut mean spending is $44.31 for females. 
# This is the ëbest-point-estimateí for all females in the USA.  However, it is 
# very unlikely to be the exact value. Now we want to be 95% sure what the 
# real average spending is, so I need a better way to estimate the real value. 
# we call the 95% benchmark a ëconfidence levelí

UCL = 44.309 - qnorm(0.975)* 31.36/sqrt(55)
print(UCL)

LCL = 44.309 + qnorm(0.975)* 31.36/sqrt(55)
print(LCL)

plot(density(Haircut.males$haircut,bw=11),col="blue",
     main="Density by Amount spent ($)",xlab="Cost in $", 
     lwd=4, las=2)

# We can also overlay the density graph by adding a line for 
# the females using the lines() function and add a label 
# using the legend() function
lines(density(Haircut.females$haircut,col="red", lwd=4,lty=2))
legend(90,.02,c("Males","Females"),lty=1:2)


# Now we only want to look at the data where male spending is 
outlierReplace(my_data, "num_students_total_gender.num_students_female", which(my_data$num_students_total_gender.num_students_female > 
                                                                                 500), 500)
# If there are 20 multiple choice questions in an Math exam. Each question 
# has five possible answers, and only one of them is correct. Find the 
# probability of having four or less correct answers if a student attempts 
# to guess each answer.
dbinom(4, size=20, prob=0.2)

# The statement above tells you the chance of having exactly 4 correct 
# answers, but we want to find the chance of 4 OR LESS. So we could do:
# the d stands for distribution
dbinom(0,size=20,prob=0.2) +dbinom(1,size=20,prob=0.2) +dbinom(2,size=20,
        prob=0.2) +dbinom(3,size=20,prob=0.2) +dbinom(4,size=20,prob=0.2)

# Calculates the probability between numbers 0, 1, 2, 3, and 4
# the p stands for probability
# We could also write this simpler in the pbinom() command:
pbinom(4, size=20, prob=0.2)

pbinom(8, size=20, prob=0.2)

# Example: What is the chance of winning exactly 4 times if I 
# played the roulette 50 times at a casino
dbinom(4,size=50, prob=1/38)

# Example: What is the chance of winning 3 or less times if I 
# played the roulette 50 times at a casino
pbinom(3,size=50, prob = 1/38)

# Example: What is the chance of winning 2 or more times if I 
# played the roulette 50 times at a casino

1- pbinom(2,size=50, prob = 1/38)



