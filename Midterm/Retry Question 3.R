# First we are going to read in the file
MyData=read.csv("Mid_term_Exam.csv")

# Next we are going to view a summary of the file
summary(MyData)

# now we will use the pairs command to view graphical relationships between the variables
pairs(MyData)

########## Complete a Logistics Regression Model to predict
########## those who drive a Ferrari and those who don't. We
########## don't care about other cars

# First we start by subsetting the data to only deal with people
# driving ferrari's
FerrariSubset=MyData[1:50,]

# View the subset to make sure it is correct
View(FerrariSubset)

# Now we are going to change the 'Ferrari' name to a 0
FerrariSubset$CarMakeNM = factor(FerrariSubset$CarMakeNM,
                                 levels = c('Ferrari'), labels = c(0))

# Now we are going to create a full linear model of the data
FSfull.lm=lm(as.numeric(CarMakeNM)~NetWorthAM + Income + HouseSqft +
               Age + Gender, data=FerrariSubset)

# view a summary of our full linear model
summary(FSfull.lm)

# Next we are creating build data and test data for the logistics
# model
Build=FerrariSubset[1:25,]
Test=FerrariSubset[26:50,]

# Now We create a model using the global linear model command to 
# see which variables are good predictors of owning a Ferrari
model=glm(as.numeric(CarMakeNM)~NetWorthAM + Income + HouseSqft +
            Age + Gender, family=binomial(link="logit"), data=Build)

# View a summary of the model
summary(model)

# Now we use the complete model and do a backwards stepwise 
# regression and remove variable that might be poor predictors
Step.model=step(model, direction="backward")

# now we run the anova command to see which model is the best predictor 
# of owning a Ferrari
anova(Step.model, test="Chisq")

# view a summary of the step model 
summary(Step.model)

# create a new observation that will serve our test data
# Keep all variables because there were no changes from full.lm to 
# model
new.obs=subset(Test, select=c(NetWorthAM, Income, HouseSqft,
                              Age, Gender, CarMakeNM))

# Here we are going to predict using the step model and the 25 
# observations that were not used to build the model
predict=predict(Step.model, new.obs, type="response")

# Here we are creating a confusion matrix to see how well the 
# model predicted who owns a Ferrari
table_mat=table(new.obs$CarMakeNM, predict>0.5)

# Would you look at that, all 25 people own a ferrari!!
table_mat

# Here I am attempting to randomize the dataset
# After this is completed I will create a new model to compare 
# between who owns a Ferrari and who does not.
MyData=MyData[sample(1:nrow(MyData)),]

# Here we are creating a full linear model that will show us what
# is a good linear predictor of owning a Ferrari
MyData.lm=lm(as.numeric(CarMakeNM)~NetWorthAM + Income + HouseSqft +
               Age + Gender, data=MyData)

# Lets view a summary of the linear model we jsut build
summary(MyData.lm)

# Now we are going to create two subset of the data 
# One for the build and one to test the model
Build2=MyData[1:250,]
Test2=MyData[251:500,]

# Now We create a model using the global linear model command to 
# see which variables are good predictors of owning a Ferrari
model=glm(CarMakeNM=='Ferrari'~NetWorthAM + Income + HouseSqft +
            Age + Gender, family=binomial(link="logit"), data=Build2)

# here we view a summary of the model we just created
# we can see that Income & NetWorthAM are good predictors of owning a Ferrari
summary(model)

# Now we use the complete model and do a backwards stepwise 
# regression and remove variables that might be poor predictors
Step.model=step(model, direction="backward")

# now we run the anova command to see which model is the best predictor 
# of owning a Ferrari
anova(Step.model, test="Chisq")

# Here we are viewing a summary of the backwards Stepwise regression
# model. This shows us the final variables that were decent 
# predictors of Ferrari's in the data set
summary(Step.model)

# Here we are creating a new subset based off of our test data
# We are throwing this in the new logistics regression model
# by taking only Income, NetWorthAM & Age into account.
new.obs=subset(Test2, select=c(CarMakeNM, Income, NetWorthAM, Age))

# Here we are going to predict using the step model and the 250
# observations that were not used to build the model
predict=predict(Step.model, new.obs, type="response")

# Here we are creating a confusion matrix to see how well the 
# model predicted who owns a Ferrari
confMatrix=table(new.obs$CarMakeNM, predict>0.5)

# we are going to view the confusion matirx
confMatrix
