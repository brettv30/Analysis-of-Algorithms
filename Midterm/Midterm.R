################### Question 1 #########################

MyData=read.csv("Mid_term_Exam.csv")

View(MyData)

head(MyData)

tail(MyData)

summary(MyData)

pairs(MyData)

plot(MyData$Income, MyData$HouseSqft, xlab="Income", ylab="Square Footage",
     main="Income vs Square Footage", col="green")

hist(MyData$Income, main="Income within the dataset", xlab="Income",
     col="blue", border="red")



################### Question 2 ########################
NetvAll=lm(NetWorthAM~., data=MyData)

summary(NetvAll)

Netv2=lm(NetWorthAM~Income + CarMakeNM, data=MyData)

summary(Netv2)

# find the percentage that I think these two variables are correlated
cor(MyData$Income, MyData$NetWorthAM)

# find the conf interval of my second linear model
confint(Netv2)

# Create models to determine which one is a better predictor. Can do this bc the 
# models are nested
ModelA=lm(NetWorthAM~Income, data=MyData)

ModelB=lm(NetWorthAM~Income + CarMakeNM, data=MyData)

# Preformed anova test to see which model was better. ModelB is better
anova(ModelA,ModelB)




##################### Question 3 #####################
MyDatalm.full=lm(MyData$CarMakeNM=='Ferrari'~ NetWorthAM + Income + HouseSqft
                 + Age + Gender,data=MyData)

summary(MyDatalm.full)

########### Don't Pay attention to id #######

# split data
Build = MyData[1:250,]
Test = MyData[251:500,]

model=glm(CarMakeNM=='Ferrari'~NetWorthAM + Income + HouseSqft
         + Age + Gender, family=binomial(link="logit"), data=Build)

summary(model)

Step.model=step(model, direction="backward")

anova(Step.model, test="Chisq")

summary(Step.model)

library(pscl)
pR2(Step.model)


library(dplyr)
new.obs=subset(Test, select=c(NetWorthAM, Income, HouseSqft, Age))

predict=predict(Step.model, new.obs, type="response")





