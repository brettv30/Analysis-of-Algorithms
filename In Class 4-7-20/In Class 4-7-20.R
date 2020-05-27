#### BOOTSTRAPPING#######

# First we read in the Income.csv file and take a look at it
MyData = read.csv("Income.csv")

View(MyData)
summary(MyData)

# And do a density plot
plot(density(MyData$Income),"Income",xlab="$", col="blue", lty=2, lwd=4)

# Now we import the boot pagage (only needed once)
# and call the boot library
install.packages("boot",dep=TRUE)
library(boot)

# We can now set the parameters for our bootstrap sampling
n = 5  # Number of boostrap samples
B = 10   # Number of observations in each bootstap sample

MySamples = matrix(sample(MyData$Income, size = B * n, replace = TRUE), n, B)
View(MySamples)

# Now we can ask for statistical functions from our 10 samples 
BootstapIncomeMean = apply(MySamples, 2, mean)
BootstapIncomeMean
BootstapIncomeStDev = apply(MySamples, 2, sd)
BootstapIncomeStDev
BootstapIncomeRange = apply(MySamples, 2, range)
BootstapIncomeRange

# We can also use the standard commands we already know, 
# like summary() and describe()
summary(MySamples)
library(psych)
describe(MySamples)

####### RANDOM FOREST ###############

# first we read in the iris dataset
MyFlowers= read.csv("iris2.csv")
View(MyFlowers)

# We will now set a random seed so that we can re-create the results in the future
# This is how you constantly get the same random numbers for the Build & Validate data sets
set.seed(267)

# Now we crate a random sample of 75 from our 150 flowers
Samples = sample(1:150, size = 75)

# Now we assign the Samples to Build, and the 'leftovers' to Validate datasets
Build = MyFlowers[Samples,]
Validate = MyFlowers[-Samples,]

# We can now look at the two data sets
summary(Build)
summary(Validate)

###### MUST DO THIS FIRST TO SEE IF THE RANDOM FOREST IS REASONABLE ####
# Decide on the MAX number of features by doing e^Nf < No
# e = 2.71828. Nf = the number of features in the data. No = the number of observations. 

#import the package and call randomForest library
install.packages("randomForest")
library(randomForest)

# We can now train our Model based on the Build dataset
# we ask for 500 decision trees, set the importance to TRUE.
# This allows the algo to calculate the importance of the variables
# The mtry=2 is the number of randomly selected features used in each 
# of the 500 trees. This mtry setting is normally set to the square root of the 
# number of features in the model. Since we have 4, we will set mtry=2

MyModel = randomForest(variety ~ ., data=Build, ntree=500, mtry=2, importance=TRUE)

# We can now view the model and see how well it did against the data on which it was built:
MyModel

# Since we set importance = TRUE, we can see what 
# importance our model has assigned to each feature 
# using the varImpPlot() command
varImpPlot(MyModel)

# Now we will use our holdout data (Validation data set with 75 observations) 
# to test the data model, using data on which the model has never seen before. 
# This allows a better evaluation of real performance.

MyPredictions = predict(MyModel, Validate[,-5])
table(observed=Validate[,5],predicted=MyPredictions)

# Now we can look at the ROC curve and calculate AUC
# This might take 3-5 minutes
install.packages("ROCR")
library(ROCR)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(MyModel,Validate[,-5],type="prob")

# Adding line colours and specifying FlowerClasses based on unique variety
Colors  = c("Green","Blue","Red")
FlowerClass = levels(Validate$variety)

# For each type of flower)
for (i in 1:3)
{
  # Define which observations belong to that flower class
  true_values = ifelse(Validate[,5]==FlowerClass[i],1,0)
  
  # Assess the performance of model for that flower class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for Each type of Flower 
         (Green=Setosa) (Blue=Versiolor) (Red = Virginica)") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  # Find the Area Under the Curve (AUC) and show it for each FlowerClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}