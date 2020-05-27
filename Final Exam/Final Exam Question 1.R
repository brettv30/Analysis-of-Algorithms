# Read in the data 

mydata = read.csv('smallsurvey.csv')

# Preform EDA to better understand the data

View(mydata)

summary(mydata)

pairs(mydata)

mydata = na.omit(mydata)

plot(density(mydata$income),"Income",xlab="$", col="blue", lty=2, lwd=4)

hist(mydata$age, main = "Histogram of people's ages", xlab = "Age", col='darkgray')

# We must remove the id column from the data because with it comes a multicolinearity issue

mydata = subset(mydata, select = -id)

# We are attempting to classify each political party accurately

# First we are going to build a random forest model to attempt to classify the data
# Recognize that because we only have 30 observations, we need to use the bootstrapping technique
# on the data to create viable random forest results

library(boot)

# We must take the equation 'e^Nf < No' into account when determining the number of
# samples and observations. The number of observations in the sampled data needs to be greater
# than 8103 because that is e^9, where 9 is the number of features we are looking at.

n = 150 # Number of boostrap samples
b = 150 # Number of observations in each bootstap sample

# Here we are going to create the bootstrapped samples

mysamples = mydata[sample(NROW(mydata), b * n, replace = TRUE),]

# Quality check the data frame to make sure the samples look accurate

View(mysamples)

# set the seed so that way the model chooses the same random values every time

set.seed(345)

Samples = sample(1:22500, size = 11250)

# Now we assign the Samples to Build, and the 'leftovers' to Validate datasets

Build = mysamples[Samples,]
Validate = mysamples[-Samples,]

# We can now look at the two data sets for quality checks

summary(Build)

summary(Validate)

# Now we will begin creating our random forest model on the build data

library(randomForest)

MyModel = randomForest(politicalparty ~ ., data=Build, ntree=500, mtry=3, importance=TRUE)

# View the model we just created

MyModel

varImpPlot(MyModel)

# Our model seems to have extreme accuracy at predicting peoples' political party 
# We can see that our model looks at age, radiohours, and numbchildren the most to create an accurate
# model. Its actually surprising that gender has no effect on peoples' political party.

# Now we are going to use our model on the validate data to see if we can properly classify the 
# political party

MyPredictions = predict(MyModel, Validate[,-10])
table(observed=Validate[, 9], predicted=MyPredictions)

library(ROCR)

# Calculate the probability of new observations belonging to each political party
ROC_Predictions= predict(MyModel, Validate[,-10], type="prob")

# Adding line colours and specifying EducationClass based on unique variety
Colors  = c("Green","Blue","Red","Yellow")
PoliticalClass = levels(Validate$politicalparty)

# For each type of party
for (i in 1:4)
{
  # Define which observations belong to that political class
  true_values = ifelse(Validate[,-10]==PoliticalClass[i], 1, 0)
  
  # Assess the performance of model for that political class
  # Use the 3rd column in the true_values matrix as labels for the predictions
  pred = prediction(ROC_Predictions[,i], true_values[,3])
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for Each type of Political Party 
         (Green=Democrat) (Blue=Independent) (Red=Other) (Yellow=Republican)") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}

# The model we created predicted each political party with 100% accuracy. And we know this by 
# viewing the AUC values printed out to the screen as well as the graphic that shows each line 
# giving a perfect 90 degree angle.

# Now lets attempt to use logistics regression on the data to see if we can better classify the data





