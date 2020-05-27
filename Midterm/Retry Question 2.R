# First we are going to read in the file
MyData=read.csv("Mid_term_Exam.csv")

# Next we are going to view a summary of the file
summary(MyData)

# now we will use the pairs command to view graphical relationships between the variables
pairs(MyData)

######## Build a predictive model of NetWorthAM. 
######## Find the best predictor for NetWorthAM.

# Here we refactor the Gender column to better understand our data
MyData$Gender = factor(MyData$Gender, levels = c(0, 1), 
                       labels = c("Male", "Female"))

# Now we build a linear model of all the columns besdies the ID column
NetWorth.lm=lm(NetWorthAM~Income + HouseSqft + Age + Gender + CarMakeNM, 
               data=MyData)

# Here we view a summary of the linear model to better understand what
# is a good predictor of networth
summary(NetWorth.lm)

# Now we create another linear model after removing the bad predictors
NetWorth.lm2=lm(NetWorthAM~Income + CarMakeNM, data=MyData)

# View a summary of the second linear model
summary(NetWorth.lm2)

# Now we create two different models to find out which variables are good
# predictors of net worth
ModelA=lm(NetWorthAM~Income, data=MyData)

ModelB=lm(NetWorthAM~Income + CarMakeNM, data=MyData)

# run the anova command to compare the F-stats of the two models.
anova(ModelA, ModelB)

# create a third model just for funsies to do the AIC command
ModelC = lm(NetWorthAM~Gender + Age, data=MyData)

# We found the same thing in both the AIC and anova tests. That 
# ModelB is the best model so therefore Income & the kind of car you drive
# is a good predictor of your NetWorth. 
AIC(ModelA, ModelB, ModelC)






