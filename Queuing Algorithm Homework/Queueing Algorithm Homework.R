################ Brett Vogelsang & Andrew Reeves #################
# Question 1

# First we know that the average customer per hour is 37 so we place that into a 
# variable
AvgCustHr = 37

# Next we know that the number of customers arriving is 40 or more so we place the 
# number 40 into a variable as well
CustArriving = 40

# Now we use the ppois command because the values we are dealing with are continuous
# and we use 1 - the ppois command because this will give us the probably of 40 
# customers and above
Probability = 1- ppois(CustArriving, lambda = AvgCustHr) 

# here we print out the probablity to the screen
Probability
# Result:
# 0.2763471
# We can see that the probability of 40 or more people arriving in an hour is 27.63%.


# Question 2

# Now we are determining the service rate by taking 48 people for every 60 minutes
ServiceRate = (48/60)

# We determine the average time of each customer by dividing the service rate by 1
AvgTime = 1/ ServiceRate

# here we make it easier to look like the formula by placing the service rate 
# in the m variable
m = ServiceRate

# We want to see the probability of a customer being served in a minute or less 
# so the target service time is 1 
TargetServiceTime = 1

# rename the target service time to t in order to make it look like the formula
t = TargetServiceTime

# Here we calculate the probability of a customer being served in a minute or less by 
# using the formula 1-e^-(m*t)
Probability = 1- (2.718281828459^-(m*t))

# here we print out the probability to the screen
Probability
# Result:
# 0.550671
# We can see that the probability of a customer being served in less than a minute is 55.06%.


# Question 3
# Here we are going to be analyzing a queue with Little's Law

# Our arrival rate is 95 defects per hour
ArrivalRate = 95

# Our service rate is 103 repairs per hour
ServiceRate = 103

# Here we are measuring how much the system is being used compared to its service rate
AverageUtilization =  ArrivalRate/ServiceRate
# Result:
# 0.9223301
# We see that the system is being used 92.23% of the time. 

# Here we are finding the average length of each queue
AverageQueueLength 	= (ArrivalRate^2) / (ServiceRate*(ServiceRate-ArrivalRate))
# Result:
# 10.95267
# We can see that the average length of the queue is just under 11 repairs. Meaning that
# there is a long queue developing even though we can service more defects.

# Here we are measuring the average time spent waiting in the queue.
AverageWaitinginQueueMin = (AverageQueueLength/ArrivalRate)*60
# Result:
# 6.917476
# We see here that the average wait time is about 7 minutes for each repair in the queue. 
# To get through all 11 repairs in the queue it would take an hour and 17 minutes which 
# is a fairly inefficient system. This could be done better. 

# Here we are measuring the average number of repairs in the system at any given time.
AveragenumberofpeopleinSystem = AverageQueueLength + (ArrivalRate/ServiceRate)
# Result:
# 11.875
# We see that the average number of repairs in the system is just under 12. We know this
# is true because the average queue length is just under 11. 

# Here we are measuring the Average total time spent within the system.
AverageTotalTimeInSystemMin = AverageWaitinginQueueMin + (1 / ServiceRate)
# Result:
# 6.927184
# We see here that the average time spent in the entire system is about 7 minutes. 
# It's pretty cool that this happens because the average wait timne in the queue is 
# about seven minutes as well. This makes sense though because of course you would have to wait 
# around the same amount of time it takes for a repair to go through the system. 

ServiceRate = 100:150

AverageUtilization =  ArrivalRate/ServiceRate
AverageQueueLength 	= (ArrivalRate^2) / (ServiceRate* abs(ServiceRate-ArrivalRate)) 
AverageWaitinginQueueMin = (AverageQueueLength/ArrivalRate)*60
AveragenumberofpeopleinSystem = AverageQueueLength + (ArrivalRate/ServiceRate)
AverageTotalTimeInSystemMin = AverageWaitinginQueueMin + (1 / ServiceRate)

# This lets us see a plot of the different total system time in minutes based on 
# the different service rates between 100 and 150
plot(ServiceRate, AverageTotalTimeInSystemMin, ylim=c(0,30), xlim=c(100,125), col="red",
     main="Total Time for Service and Wait", xlab="ServiceRate", ylab="minutes")
text(ServiceRate, AverageTotalTimeInSystemMin, round(AverageTotalTimeInSystemMin,2), 
     pos=3, cex=0.5)

