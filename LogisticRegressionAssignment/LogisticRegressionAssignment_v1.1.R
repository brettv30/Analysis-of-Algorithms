###### Logistic Regression Model Assignment - Andrew Reeves & Brett Voglesang ######
## 0 = NO, 1 = YES ##

#read in the data and store it in the tips variable
tips = read.csv("RestaurantTips1.csv")

#get the headers of the data
ls(tips)

#view a summary of the data 
summary(tips)

#change the values in credit from y & n to 1 & 0
tips$Credit = factor(tips$Credit, levels = c('n','y'), labels = c(0, 1))

# View the sata to make sure the appropriate changes were made
summary(tips)

# we now use two different methods to look at number of tips by 
# type of payment (0=cash, 1=card)
aggregate(tips$Tip, list(Payment=tips$Credit), FUN=sum)

by(tips$Tip, tips$Credit, sum)

# Here we see a really good graph that displays the bill amount by the 
# type of payment that is used. We can see that on average, Card users 
# pay more per bill than cash users. 
boxplot(tips$Bill~tips$Credit, data=tips,
        xlab="Payment type", ylab="Bill Amount",
        main="Tip Percentage by Payment",
        col=c("blue", "yellow"), border="brown", las=1)

# Here we create a full linear model of all the data 
tipslm.full=lm(as.numeric(tips$Credit)~.,data=tips)

# Now we look at the linear model to find out which variable are good predictors 
# of tip
summary(tipslm.full)

# Here we Split the data in half and use one for building the model
# and the other for testing the model
Build = tips[1:78,]
Test = tips[79:157,]

# we create a model using the glm command to test which variables are good 
# predictors of payment type
model=glm(Credit~., family=binomial(link="logit"), data=Build)

# verify which variables seem to be good predictors and which ones do not 
# seem to be good predictors
summary(model)


# Now we use the complete model and do a backwards stepwise 
# regression and remove variable that might be poor predictors
Step.model=step(model, direction="backward")


# now we run the anova command to see which model is the best predictor 
# of payment type. 
# We can see that the best predictor of payment type is Tip and then second 
# is Tip Percentage.
anova(Step.model, test="Chisq")

# Use the pscl library & the pR2 command to see the strength of our
# relationships between the data.
#
# We can see that based on our McFadden value, our relationship is not
# that strong of a predictor for payment type. What could it be then?
library(pscl)
pR2(Step.model)


# view a summary of the step model 
summary(Step.model)


# Use the dplyr package to help create a new subset of data
library(dplyr)

# Create a new subset of data excluding monday from the list 
# because it was a very bad predictor after comparing the model summary and lm summary
new.obs=subset(Test, Test$Day != "m",
               select=c(Bill, Credit))

# Now we predict using the step model and the new observation on the 79
# transactions that were not used to build the model
predict=predict(Step.model, new.obs, type="response")

#make a table of the "confusion matrix" to see how well the model predicted
table_mat = table(new.obs$Credit, predict>0.5)

#view the table
table_mat


