# First we are going to read in the file
MyData=read.csv("LoanLarge copy.csv")

# Here we view the names of all of the variables 
ls(MyData)

# Here we see some descriptive statistics pertaining to each 
# variable
summary(MyData)

# Lets clean up the data before we begin to use it
# Remove the LoanID column because there is multicolinearity with this variable
MyData$LoanID = NULL

# remove all NA values in the data set
MyData = na.omit(MyData)

# there were some values in the CustEmplLength column that held no data so
# we need to remove those as well
MyData = MyData[!MyData$CustEmplLength == "", ]


# randomize the rows to have a better distribution
MyData=MyData[sample(1:nrow(MyData)),]

# We have finally cleaned all of the data so now lets split the data
# Now we are going to split the data in order to create a test set 
# and a build set
Build = MyData[1:95732,]
Test = MyData[95733:191464,]

# call the party and partykit libraries because
# we will need some commands from them
library(party)
library(partykit) 

# Now we are going to build our decision tree model with our build data. 
# here we look at the High FICO credit score and the Loan Grade and how those 
# affect the LoanDefault Status. 
MyBuildTreeModel=ctree(LoanDefaultStatusCD~ CredFICOHighNO + LoanGradeCD, data=Build)

# Here we are creating a visual graph of the Decision Tree we created above. 
png(file = "decision_tree_build.png", res=80, height=800, width=5000)
class(MyBuildTreeModel) 
plot(MyBuildTreeModel, type="simple",         
     inner_panel=node_inner(MyBuildTreeModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()

# Here w created a pruned decision tree with a maximum depth of 3. This is done to 
# alleviate any groups that were too drilled down. 
PrunedBuildTreeModel=ctree(LoanDefaultStatusCD~ CredFICOHighNO + LoanGradeCD, data=Build,
                        maxdepth=3)

# Below we create a graph of the pruned decision tree.  Here we see our errors have 
# skyrocketted! We really liked the frist model becuase it specified a few quality
# groups. Maybe we should keep this decision tree at 6 layers....
png(file="pruned_decision_tree_build.png", res=80, height=800, width=5000)
class(PrunedBuildTreeModel)
plot(PrunedBuildTreeModel, type="simple",
      inner_panel=node_inner(PrunedBuildTreeModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()


# Now we are going to run our test data through the model 
MyTestTreeModel=ctree(LoanDefaultStatusCD~ CredFICOHighNO + LoanGradeCD, data=Test)

# Here we are creating a visual graph of the Decision Tree we created above. 
png(file = "decision_tree_test.png", res=80, height=800, width=5000)
class(MyTestTreeModel) 
plot(MyTestTreeModel, type="simple",         
     inner_panel=node_inner(MyTestTreeModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()

# Below we are using our test data for the pruned tree as well. 
# Here w created a pruned decision tree with a maximum depth of 5. This is done to 
# alleviate any groups that were too drilled down, however if we were to go any higher,
# then the errors would be way too large. 
PrunedTestTreeModel=ctree(LoanDefaultStatusCD~ CredFICOHighNO + LoanGradeCD, data=Test,
                           maxdepth=5)

# Below we create a graph of the pruned decision tree.
png(file="pruned_decision_tree_test.png", res=80, height=800, width=5000)
class(PrunedTestTreeModel)
plot(PrunedTestTreeModel, type="simple",
     inner_panel=node_inner(PrunedTestTreeModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()

