# First we load in the housing data we have been working on in the past
MyData = read.csv("glassdoordata.csv")

# and take a quick look at the data
summary(MyData)

# Then we build a complete model of all data in the dataset
lm.full = lm(income ~ ., data=MyData)

# Now we can take a look at the result, when all predictor variables are incuded
summary(lm.full)

# Now we want to find the best model by starting with the 'lm.full'
# model with all predictors and step-by-step have the algorithm
# remove the worst predictors and automatically examine the AIC to
# pick the best possible model for predicting income
lm.step = step(lm.full, direction = "backward")

# Since we stored the final model in the dataframe we called 'lm.step"
# we can now examing the final model and see how well it performs
summary(lm.step)

################ LOGISTIC REGRESSION #######################
# First lets load in the loan data from the csv file
LoanData = read.csv("loans.csv")

# and take a look at the headers
ls(LoanData)

# Now we want to see summary statistics
summary(LoanData)

# Let us look at number loan delinquencies by type of loans
aggregate(LoanData$DelinquentStatus, list(Status=LoanData$CreditPurpose), FUN=sum)

# and by total amount outstanding that is delinquent of loans
by(LoanData$CurrAcctBal, LoanData$DelinquentStatus, sum)

# We can also examinge the average FICO score for those
# who are deliquent on theloan repayment using a box plot
boxplot(LoanData$CustFICO ~ LoanData$DelinquentStatus, data = LoanData,  
        xlab = "Delinquent Status", ylab = "FICO score", 
        main = "FICO Credit Score by delinquency", 
        col=c("Red", "Green"), border="blue", las=1)


# Let us build a full regression with all variables to see what 
# drives loan delinquencies
lm.full = lm(LoanData$DelinquentStatus ~ ., data=LoanData)

# Now we can take a look at the result, with all predictor variables
summary(lm.full)

# If we built a LOGISTIC (glm) simple model to predict default probabilities 
# based on FICO credit scores as our only predictor. We would get:
FICOModel = glm(DelinquentStatus~CustFICO, family=binomial, data=LoanData)
summary(FICOModel)


# Now lets predict default for a person with a credit score of 550
p = predict(FICOModel, newdata = data.frame(CustFICO=550), type="response", se.fit=T)

# and see the value of p (probability of default)
p

# Now lets get a prediction interval of default rates, using 
# a 95% confidence level. Remember that is equal to 
# prediction +/- 2 Standard Errors (Se)
out=c(p$fit, p$fit-2*p$se.fit, p$fit+2*p$se.fit )
names(out) = c("Fit", "Lower", "Upper")
out

# Now let us look at the estimated default chances for applicants
# with credit scores between 500 and 900 and look at it graphically

plot(LoanData$CustFICO, LoanData$DelinquentStatus, ylim=c(0,0.2),
     xlab="FICO score", main="Predicted Default Rates", ylab="Rates (%)")

p = predict(FICOModel, newdata = data.frame(CustFICO=500:900), type="response")
lines(500:900, p, col="blue", lwd=3)


### Larger Logistics model #####

# First, let us split our data in two sets.  One for building the model
# and one dataset for testing the model:

Build <- LoanData[1:4789,]
Test <- LoanData[4790:9578,] 

# Lets build our first logistic model based on the first 'Build" dataset
model = glm(DelinquentStatus ~.,family=binomial(link='logit'), data=Build)
summary(model)

# Now we will use the complete model and let R do a backwards stepwise 
# regression and remove the variables that might be poor predictors
Step.model = step(model, direction = "backward")

# We can also test the model and see the distance using the anova command
anova(Step.model, test="Chisq")

# While no exact equivalent to the R2 of linear regression exists, 
# the McFadden R2 index can be used to assess the model fit.

install.packages("pscl")
library(pscl)
pR2(Step.model)

# What type of loans are most risky for the bank?
summary(Step.model)

################## TESTING OUR MODEL'S ACCURACY ##############
# We will now use the Test data we split out earlier to see how
# well our model actually worked.  Remember: we split the LoanData
# into two groups: "Build" to build our model, and 'Test' to test
# the model after we completed this task.

# first we install the dplyr package:
install.packages("dplyr")
library(dplyr)

# Now we create a new dataset with the variables that was used in 
# our mode. Remember that loans for Business and CreditCards were not
# good predictors. So we remove these loans from our new data set, 
# and only inlude variables that were used in the final model we built

new.obs = subset(Test, Test$CreditPurpose !="Business" & Test$CreditPurpose !="CreditCard",
                 select= c(DelinquentStatus, CustFICO, CustIncome, IntRate,InqLst6Mos,
                           NoLatePmtLst2Yrs, PublicRecords, CreditPurpose, 
                           CurrAcctBal,AcctOpenNoMonths, CreditOpenNoMonths))

# We can now execute the predictions on the 3,797 loans that were not used
# to build the model and see the model performed
predict = predict(Step.model, new.obs, type = 'response')

# The 'confusion matrix' shows us how well we predicted loan defaults 
table_mat = table(new.obs$DelinquentStatus, predict>0.5)
table_mat