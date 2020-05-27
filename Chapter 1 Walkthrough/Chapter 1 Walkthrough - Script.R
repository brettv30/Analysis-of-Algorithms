# Import the bike sharing data file to the bike variable
bike <- read.csv("Ch1_bike_sharing_data.csv")

# use the dplyr library
library(dplyr)
# create a temporary data frame with the registered bikers during
# the spring or the summer season
extracted_rows <- filter(bike, registered == 0, 
                         season == 1 | season == 2)

# show the number of observations and variables for the criteria.
dim(extracted_rows)

# create an identical data frame as extracted rows
using_membership = filter(bike, registered == 0, season %in% c(1,2))

# check to ensure both data frames are identical
identical(extracted_rows, using_membership)

# The only columns of interest are the season and casual renter columns 
# so we apply those columns to the new data frame.
extracted_columns = select(extracted_rows, season, casual)

# Adding the revenue column to the add_revenue data frame  which is a mutation of the 
# extracted_columns data fram the output will be casual * 5
add_revenue = mutate(extracted_columns, revenue = casual * 5)

# the dplyr package includes the group_by funtion to help aggregate data
# group the data in the add_revenue data frame on the season variable 
grouped = group_by(add_revenue, season)

# create a report of the summarized group data frame by summing up the casual and the revenue 
# columns with respect to the spring and the summer seasons 
report = summarise(grouped, sum(casual), sum(revenue))

# write the report information to a new csv file titled revenue_report
write.csv(report, "revenue_report.csv", row.names = FALSE)

