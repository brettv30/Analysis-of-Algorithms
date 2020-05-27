# First We in the data from the "LoanSmall.csv" file
MyData = read.csv("LoanSmall.csv")

# Now we can look at the headers. Don't use the View() command
# this data set has over 413,000 observations
ls(MyData)

# Now we explore the data
summary(MyData)

#Installing the required packages and libraries
library(party)
library(partykit) 

# We now build a regression tree to predict the loan Default Status
MyModel = ctree(LoanDefaultStatusCD~  CustIncAM + CredFICOHighNO, data = MyData)

# Creating a graph of the Complete Decision Tree 

png(file = "decision_tree.png", res=80, height=800, width=5000)
class(MyModel) 
plot(MyModel, type="simple",         
     inner_panel=node_inner(MyModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()

# Now we create a more compex graph to compare groups
png(file = "decision_tree.png", res=80, height=1400, width=5000)
class(MyModel) 
plot(MyModel,          
     inner_panel=node_inner(MyModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()

# We can also create graphs with much more information about p-values
png(file = "decision_tree.png", res=80, height=1400, width=5000)
class(MyModel) 
plot(MyModel)          
dev.off()

# Since the graph was messy, we want to limit number of levels/depths
PrunedMyModel = ctree(LoanDefaultStatusCD~  CustIncAM + CredFICOHighNO, data = MyData, 
                      maxdepth=3)

# smaller Graphics of Pruned model
png(file = "Decision_tree_pruned.png", res=80, height=500, width=1500)
class(PrunedMyModel) 
plot(PrunedMyModel, type="simple",         
     inner_panel=node_inner(PrunedMyModel, abbreviate = TRUE, pval = FALSE, id= TRUE)) 
dev.off()

# We can also see the model in a text format using the print() command
print(PrunedMyModel)


# A new Complete model with four variables that is pruned to 
# 4 levels with a new graph
LargeModelPruned = ctree(LoanDefaultStatusCD~  LoanAM + CustIncAM + IncVerifiedCD + 
                           CredFICOHighNO, data = MyData, maxdepth=4)

png(file = "Decision_tree_pruned.png", res=80, height=550, width=3000)
class(LargeModelPruned) 
plot(LargeModelPruned, type="simple")
dev.off()




