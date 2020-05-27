# create the MyData vector
MyData = read.csv("medicalmalpractice.csv")

# create a sorted vector from the data in the MyData vector
SortedMyData = sort(MyData)

# Show all rows of data for columns 3,4,5
MyData[,3:5]

# Show the last set of 6 rows in the dataset
tail(MyData)

# Show the age of all males in the dataset
MyData$Age[MyData$Gender == "Male"]

# Calculate the total age of all males in the dataset
sum(MyData$Age[MyData$Gender == "Male"])

# Calculate the average age of all males in the dataset
mean(MyData$Age[MyData$Gender == "Male"])

#Ages of all patients who had a severity malpractice incident less than 5
MyData$Age[MyData$Severity<5]

# List of the amounts awarded to all patients who had a severity malpractice incident equal or greater to 8
MyData$Amount[MyData$Severity >= 8]

#The total amount awarded to patients who had a severity malpractice incident equal or greater to 8
sum(MyData$Amount[MyData$Severity >= 8])

#The average amount awarded to patients who had a severity malpractice incident equal or greater to 9
mean(MyData$Amount[MyData$Severity >= 9])

# Looking at a list of unique medical specialties in the dataset
unique(MyData$Specialty)

# Creating a subset of data with only 2 columns where awarded amount is greater than 0
SpecialityData = subset(MyData, MyData$Amount>0, select= c(Amount, Specialty))

# Creating a subset of data with all columns for People who are uninsured
Uninsured = subset(MyData, MyData$Insurance=="No Insurance")

# Calculating the average malpractice amount for all specialties (column 1)
mean(SpecialityData$Amount)

# Calculating the total malpractice amount awarded for each medical specialties
aggregate(SpecialityData$Amount, list(Speciality=SpecialityData$Specialty), FUN=sum)

# Calculating the average malpractice amount awarded for each medical specialties
aggregate(SpecialityData$Amount, list(Speciality=SpecialityData$Specialty), FUN=mean)

# Another way of calculating averages by groups using the "by" command
by(SpecialityData$Amount,list(SpecialityData$Specialty),FUN=mean)

# Another way of calculating total malpractice awards by specialty and Insurance type
# (cross-tab) using the "by" command
by(MyData$Amount,list( MyData$Insurance,MyData$Specialty),FUN=sum)

# Creating a Table using that summarized number of observations
# for each Insurance type
table(MyData$Insurance,MyData$Specialty)

# You can also create cross-tab tables. In this example we are summarizing
# the number of people who had different type of Insurance, with the services they received
table(MyData$Specialty,MyData$Insurance)

#Sorting the highest awards given by medical specialties
sort(by(SpecialityData$Amount,list(SpecialityData$Specialty),FUN=mean))

----------------------------
  
  # First I created a new Project and downloaded the file named
  # called "bollywood.boxoffice.csv" from Canvas to the new project directory
  
  # Second we read the data file into a new data frame we call "Bollywood
  Bollywood <- read.csv(c("bollywood_boxoffice.csv"))

# Third, we create our data frame 
Bollywood = read.csv("bollywood_boxoffice.csv")

# Since the box office gross sales amounts and budget amounts in this data frame is
# in millions of Rupees, we want to add new columns with millions of Dollar amounts as 
# well. We know a rupee is worth 0.0139147 dollars and will use that in our formula
Bollywood.GrossUSD = Bollywood$Gross * 0.0139147
Bollywood.BudgetUSD = Bollywood$Budget * 0.0139147
Bollywood.ProfitUSD = Bollywood.GrossUSD - Bollywood.BudgetUSD

# Now we can see the total profit (million USD) and the average profit 
# for these 190 movies
sum(Bollywood.ProfitUSD)
mean(Bollywood.ProfitUSD)

# We can also see the summary statistics for the move sales in millions of USD
summary(Bollywood.ProfitUSD)

# We can see the NUMBER of movies that were profitable
sum(Bollywood.ProfitUSD > 0)

# We can see the total number of movies made
length(Bollywood.ProfitUSD)

# We can see the Percentage of movies that were profitable
sum(Bollywood.ProfitUSD > 0) / length(Bollywood.ProfitUSD)

# Or we can use this alternative formula to see the Percentage of 
# movies that were profitable (gives same result as above)
mean(Bollywood.ProfitUSD > 0)

# We can see what movie made the most gross sales in Rupees (millions)
Bollywood$Movie[which.max(Bollywood$Gross)]

# We can see what movie made the least gross sales in Rupees (millions)
Bollywood$Movie[which.min(Bollywood$Gross)]

# We can see what movie made the most profit in Rupees (millions)
Bollywood$Movie[which.max(Bollywood$Gross-Bollywood$Budget)]