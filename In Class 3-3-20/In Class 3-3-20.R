### Arrival Rates##
# First we try finding the probability of customers arriving. For example,
# if we had 3 customer arriving on average per hour, what if the probability
# that exactly 4 customers would arrive in an hour?

AvgCustHr = 3
CustArriving =4
Probability = ((2.718281828459^-AvgCustHr) * (AvgCustHr ^ CustArriving)) / factorial(CustArriving)
Probability


# What if we wanted to know the probability of 4 OR LESS customers arriving 
# at our store, if we normally had 3 customer arriving on average per hour?

AvgCustHr = 3
CustArriving =4

Prob4 = ((2.718281828459^-AvgCustHr) * (AvgCustHr ^ CustArriving)) / factorial(CustArriving)
Prob3 = ((2.718281828459^-AvgCustHr) * (AvgCustHr ^ (CustArriving-1))) / factorial(CustArriving-1)
Prob2 = ((2.718281828459^-AvgCustHr) * (AvgCustHr ^ (CustArriving-2))) / factorial(CustArriving-2)
Prob1 = ((2.718281828459^-AvgCustHr) * (AvgCustHr ^ (CustArriving-3))) / factorial(CustArriving-3)
Prob0 = ((2.718281828459^-AvgCustHr) * (AvgCustHr ^ (CustArriving-4))) / factorial(CustArriving-4)
Total = Prob0+Prob1+Prob2+Prob3+Prob4
Total

# Now we can plot the probabilities
MyData = c(Prob0,Prob1,Prob2,Prob3,Prob4)
barplot(MyData, col="blue", main="Prob of 0,1,2,3,4 customers arriving")

# A better way to calculate the continious arrival rates is the
# use of the ppois() command. For example if we want to know the
# probability of 4 or less customers in an hour, when historical average 
# number of customers arriving is 3 per hour.

AvgCustHr = 3
CustArriving =4

Probability = ppois(CustArriving, lambda = AvgCustHr) 
Probability

# Another example: If normally 105 passengers arrived, 
# on average, at a TSA check-point per hour, what is the 
# probability that a line of 10 or more passengers would form?

AvgCustHr = 105
CustArriving = 115
Probability = 1- ppois(CustArriving, lambda = AvgCustHr) 
Probability

# EXAMPLE-1: At a large McDonalds, if the service time of each customer is 
# independent of the others and they can handle 2 people per minute, 
# what is the propability that a customer can be served in less 
# than 15 seconds (0.25 minutes).

ServiceRate = 2
AvgTime = 1/ ServiceRate
m = ServiceRate

TargetServiceTime = 0.25
t = TargetServiceTime

Probability = 1- (2.718281828459^-(m*t))
Probability

# EXAMPLE-2: in a factory, if the manufacturing time of each 
# product is independent of the others. and they manufacture 
# 400 per day (8 hrs), what is the propability that a 70 products can be 
# made in 45 min (0.75 hours).

ServiceRate = (400/8)
AvgTime = 1/ ServiceRate
m = ServiceRate

TargetServiceTime = (0.75/70)
t = TargetServiceTime

Probability = 1- (2.718281828459^-(m*t))
Probability

### ANALYZING QUEUES WITH LITTLE'S LAW
# EXAMPLE-1; If customer complaints arrive at an average of 24 per hr and the 
# call center can handle 30 customers per hour, analyze the queue:

ArrivalRate = 24
ServiceRate = 30

AverageUtilization =  ArrivalRate/ServiceRate
AverageQueueLength 	= (ArrivalRate^2) / (ServiceRate*(ServiceRate-ArrivalRate)) 
AverageWaitinginQueueMin = (AverageQueueLength/ArrivalRate)*60
AveragenumberofpeopleinSystem = AverageQueueLength + (ArrivalRate/ServiceRate)
AverageTotalTimeInSystemMin = AverageWaitinginQueueMin + (1 / ServiceRate)


# Example-2: If customers arrive at an average of 16 per hr and 
# DMW can handle 18 customers per hour on average, analyze the queue:

ArrivalRate = 16
ServiceRate = 18

AverageUtilization =  ArrivalRate/ServiceRate
AverageQueueLength 	= (ArrivalRate^2) / (ServiceRate* (ServiceRate-ArrivalRate)) 
AverageWaitinginQueueMin = (AverageQueueLength/ArrivalRate)*60
AveragenumberofpeopleinSystem = AverageQueueLength + (ArrivalRate/ServiceRate)
AverageTotalTimeInSystemMin = AverageWaitinginQueueMin + (1 / ServiceRate)

# Lets examine the DMW further and explore effects of different service levels.
# First we change Servicerates to a set of values. Then we calculate all new 
# numbers for each service level, and graph the total time in system

ArrivalRate = 16
ServiceRate = 16:40

AverageUtilization =  ArrivalRate/ServiceRate
AverageQueueLength 	= (ArrivalRate^2) / (ServiceRate* abs(ServiceRate-ArrivalRate)) 
AverageWaitinginQueueMin = (AverageQueueLength/ArrivalRate)*60
AveragenumberofpeopleinSystem = AverageQueueLength + (ArrivalRate/ServiceRate)
AverageTotalTimeInSystemMin = AverageWaitinginQueueMin + (1 / ServiceRate)

plot(ServiceRate, AverageTotalTimeInSystemMin, ylim=c(0,60), xlim=c(16,40), col="blue",
     main="Total Time for Service and Wait", xlab="ServiceRate", ylab="minutes")
text(ServiceRate, AverageTotalTimeInSystemMin, round(AverageTotalTimeInSystemMin,2), 
     pos=3, cex=0.5)


# We can also analyze the average LENGTH of the queue based on 
# changing our service levels so that we can handle between 
# 16 - 40 people per hour. 

ArrivalRate = 16
ServiceRate = 16:40

AverageUtilization =  ArrivalRate/ServiceRate
AverageQueueLength 	= (ArrivalRate^2) / (ServiceRate* (ServiceRate-ArrivalRate)) 
AverageWaitinginQueueMin = (AverageQueueLength/ArrivalRate)*60
AveragenumberofpeopleinSystem = AverageQueueLength + (ArrivalRate/ServiceRate)
AverageTotalTimeInSystemMin = AverageWaitinginQueueMin + (1 / ServiceRate)

plot(ServiceRate, AverageQueueLength, ylim=c(0,16), xlim=c(16,40), col="red",
     main="Average Number of People in DMW Queue", xlab="ServiceRate", ylab="")
text(ServiceRate, AverageQueueLength, round(AverageQueueLength,2), 
     pos=3, cex=0.5)
