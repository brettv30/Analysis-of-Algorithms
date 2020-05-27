# First we want to read in the data
glassdoor = read.csv("glassdoordata.csv")

View(glassdoor)

x = rnorm(1000, mean=140, sd=30)

hist(x, breaks=20)
mean(x)
sd(x)

nsim = 1000 # number of simulations
n = 50
xbars = rep(NA, nsim)

for (i in 1: nsim){
  sample.income = sample(glassdoor$income, n,
                         replace = F)
  xbars[i] = mean(sample.income)
}

hist(glassdoor$income, main="sample Means simulations vs. True Population mean.",
     ylim=c(0,200))
par(new=TRUE)
plot(50, mean(glassdoor$income), ylab=" ", xlab=" ", yaxt='n',xaxt='n', col='red')


MySamples = read.csv("subjects1.csv")
View(MySamples)

assignment = c(rep("Treatment group", 10), rep("control group", 10))
View(assignment)

MySamples$assignments = assignment.random
View(MySamples)

random.assignment = sample(c("Special Offer Group", "No offer group"), 5000, repl=TRUE)
View(random.assignment)





# First we read in the dataset
MyData = read.csv("icecream.csv")

# The we take a look at the data
View(MyData)

# Now we want to look at all relationships using the 
# pairs() command to see if we have some linear relationships
pairs(MyData, pch=20)

# Now we want to explore the data so we first plot it
plot(MyData$temp, MyData$cons, xlab="Temperature", ylab="Icecream Sales")

# Then we calculate the regresion line of the temperature and ice cream 
# consumption and graph it on top of our scatter plot
lm(MyData$cons~MyData$temp)
par(new=TRUE)
abline(lm(MyData$cons~MyData$temp), col="blue", lwd=3, lty=5) 

# We can get more regression information by using the 
# summary() command:
summary(lm(MyData$cons~MyData$temp))

#### BULDING A NEW MODEL FOR INCOME AND ICE CREAM SALES ###
plot(MyData$income, MyData$cons, xlab="Income", 
     ylab="Icecream Sales")
par(new=TRUE)
abline(lm(MyData$cons~MyData$income), col="red", lwd=4, lty=6) 
summary(lm(MyData$cons~MyData$income))




