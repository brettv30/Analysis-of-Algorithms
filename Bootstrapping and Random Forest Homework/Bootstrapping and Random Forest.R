# First we read in the Income.csv file and take a look at it
MyData = read.csv("Income2.csv")

# preform EDA on the MyData set
View(MyData)
summary(MyData)


# And do a density plot
plot(density(MyData$Income),"Income",xlab="$", col="blue", lty=2, lwd=4)

# Make the data into number really quick to create a scatter plot of the data
MyData$Education = factor(MyData$Education, levels = c('HS', 'BS', 'MS', 'PHD'), 
                         labels = c(1, 2, 3, 4), ordered = TRUE)

# And do a scatter plot to see education level versus yearly income
plot(MyData$Income, MyData$Education, main = "Education Level vs Yearly Income (HS-1.0) (BS-2.0) (MS-3.0) (PHD-4.0)", 
     ylab="Education Level", xlab="Yearly Income", col="blue", lty=2, lwd=4)

# Turn the data back into the labels it originally had 
MyData$Education = factor(MyData$Education, levels = c(1, 2, 3, 4), 
                          labels = c('HS', 'BS', 'MS', 'PHD'), ordered = TRUE)


library(boot)

# We can now set the parameters for our bootstrap sampling
n = 11  # Number of boostrap samples
B = 10   # Number of observations in each bootstap sample

MySamples = MyData[sample(NROW(MyData), B * n, replace = TRUE),]
View(MySamples)

# We will now set a random seed so that we can re-create the results in the future
# This set seed command is how you constantly get the same random numbers for the Build & Validate data sets
set.seed(190)

# set the sample size to be greater than 35 so that way we can accurately predict the entire population
Samples = sample(1:110, size = 55)

# Now we assign the Samples to Build, and the 'leftovers' to Validate datasets
Build = MySamples[Samples,]
Validate = MySamples[-Samples,]

# We can now look at the two data sets
summary(Build)
summary(Validate)


library(randomForest)

# We can now train our Model based on the Build dataset
# we ask for 500 decision trees, set the importance to TRUE.
# This allows the algo to calculate the importance of the variables
# The mtry=2 is the number of randomly selected features used in each 
# of the 500 trees. This mtry setting is normally set to the square root of the 
# number of features in the model. Since we have 4, we will set mtry=2

MyModel = randomForest(Education ~ ., data=Build, ntree=500, mtry=2, importance=TRUE)

# We can now view the model and see how well it did against the data on which it was built:
MyModel

# Since we set importance = TRUE, we can see what 
# importance our model has assigned to each feature 
# using the varImpPlot() command
varImpPlot(MyModel)

# Now we will use our holdout data (Validation data set with 55 observations) 
# to test the data model, using data on which the model has never seen before. 
# This allows a better evaluation of real performance.

MyPredictions = predict(MyModel, Validate[,-5])
table(observed=Validate[,5],predicted=MyPredictions)

library(ROCR)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(MyModel,Validate[,-5],type="prob")

# Adding line colours and specifying EducationClass based on unique variety
Colors  = c("Green","Blue","Red","Yellow")
EducationClass = levels(Validate$Education)

# For each type of degree
for (i in 1:4)
{
  # Define which observations belong to that education class
  true_values = ifelse(Validate[,5]==EducationClass[i],1,0)
  
  # Assess the performance of model for that education class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for Each type of Education Level 
         (Green=BS) (Blue=HS) (Red = MS) (Yellow = PHD)") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}
