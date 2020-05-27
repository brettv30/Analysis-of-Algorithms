# read the data from the raw_bikeshare_data file to the bike data frame
bike = read.csv("Ch2_raw_bikeshare_data.csv")

# summarize the bike data frame
str(bike)

# learn the dimensions of the data frame
dim(bike)

# show the first six lines of the data frame
head(bike)

# show the bottom six lines of the data frame
tail(bike)

# use the is.na() function to determine the number of missing values within the data set
table(is.na(bike))

# install the necessary package 
#install.packages("stringr")
# call the stringr library and use the str_detect method to 
# find where the first "NA" value occurs. In this case it occurs within column 13.
library(stringr)
str_detect(bike, "NA")

# create a table to better understand where the NA values occur. 
# they all occur within the sources variable
table(is.na(bike$sources))

# take all instances of char values within humidity and place them in a bad_data object
bad_data = str_subset(bike$humidity, "[a-z, A-Z]")

# we see that bad data is saved with the value "x61" and that is not correct
# assign the row position of x61 to location by passing the elements of bad_data into the
# str_detect() function
location = str_detect(bike$humidity, bad_data)

# find the flaw in the data by subsetting it and showing it on the screen
# because of this we know that the flaw is in row 14177
bike[location, ]

# use the str_replace_all() function to replace all instances based on my own parameters
bike$humidity = str_replace_all(bike$humidity, bad_data, "61")

# now you can see under the humidity row, the 'x' has been removed from the 61
# the data has been fixed
bike[location, ]

# convert the humidity column to numeric values 
bike$humidity = as.numeric(bike$humidity)

# change the numeric values of 0 and 1 within the holiday and workingday columns
# to easily understandable words like no and yes
bike$holiday = factor(bike$holiday, levels = c(0,1),
                      labels = c("no", "yes"))
bike$workingday = factor(bike$workingday, levels = c(0, 1),
                         labels = c("no", "yes"))

# use the same idea as above but now changes the season and weather to understandable words
bike$season = factor(bike$season, levels = c(1,2,3,4),
                     labels = c("Winter", "Spring",
                                "Summer", "Autumn"),
                     ordered = TRUE)
bike$weather = factor(bike$weather, levels = c(1,2,3,4),
                      labels = c("clr_part_cloud",
                                 "misty_cloudy",
                                 "lt_rain_snow",
                                 "hvy_rain_snow"),
                      ordered = TRUE)

# install.packages("lubridate")
# use the lubridate library to format the datetime column as {m/dd/yyyy hh:mm}
library(lubridate)
bike$datetime = mdy_hm(bike$datetime)
str(bike$datetime)

# view the different unique kinds of advertising sources within the data frame
unique(bike$sources)

# incorporate the stringr library
library(stringr)

#convert all strings in the sources column to lower case
bike$sources = tolower(bike$sources)

#convert all strings in the sources column to have no whitespace
bike$sources = str_trim(bike$sources)

#subset the NA values in the sources column to the na_loc object
na_loc = is.na(bike$sources)

#change all NA values within the sources column to "unknown"
bike$sources[na_loc] = "unknown"

# re-view all different unique sources within the data frame
unique(bike$sources)

# Always think of Miller's Law when determining a number of quantities
# The human mind works best with things and categories when they appear in quantities of seven, plus or minus two
# install.packages("DataCombine")
library(DataCombine)

# assign the web_sites variable with all options of strings starting with www.
web_sites = "(www.[a-z*].[a-z]*)"

# find all instances where the above occurs and assign them to the current object
current = unique(str_subset(bike$sources, web_sites))

# fill the replace object with the length of the current object and then replace each current  
# object with the string "web" 
replace = rep("web", length(current))

# create a table that shows the current and replace vectors in a data frame
replacements = data.frame(from = current, to = replace)

# use the FindReplace() function to replace all websites with the "web" string
# the data set is bike, the variable is sources, the replacements table is used to replace data in sources
# use the "from" and "to" elements as input parameters
bike = FindReplace(data = bike, Var = "sources", replacements,
                   from = "from", to = "to", exact = FALSE)

# print the new table of unique observations in the sources column
unique(bike$sources)

# I am now fully adapting the string variable by converting it to a factor
bike$sources = as.factor(bike$sources)

# show a summary of the bike dataset
str(bike)

# write the cleaned data to a csv file
write.csv(bike, "Ch2_clean_bike_sharing_data.csv",
          row.names = FALSE)


