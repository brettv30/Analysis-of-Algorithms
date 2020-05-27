# Begin by reading in the data

plot_dat = read.csv('Ch7_marketing.csv')

plot_dat$emp_size = cut(plot_dat$employees, breaks = 3,
                        labels = c('Employees: 3-6', '7-9', '10+'))

# Call the ggplot2 and the scales libraries

library(ggplot2)
library(scales)

# Create a plot object that holds the information for the marketing_total vs revenues

plot = ggplot(data = plot_dat, aes(x = marketing_total, y = revenues))

# Now we are going to add layers to the plot so it appears how we would like

# We are separating the data based on the emp_size variable which is what the 
# facet_grid() command is doing.

# Then we are creating the points and adding a color, 
# shape, and size to them. 

plot = plot + facet_grid(. ~ emp_size) +
  geom_point(aes(color = pop_density), shape = 18, size = 4)

# We still need to add the proper axis and labeling to the plot

plot + scale_y_continuous(labels = dollar,
                          breaks = pretty_breaks(n = 5)) + 
  scale_x_continuous(labels = dollar,
                     breaks = pretty_breaks(n = 5)) +
  scale_color_discrete(guide = guide_legend(title = "Population\nDensity")) +
  xlab('Marketing Expenditures ($K)') + ylab("Revenues ($K)")

# Now we have created a much more stylish graphic than in previous chapters. This 
# is easier to interpret and it is much more user friendly. 

# It is recommended to use the scales library hand in hand with the ggplot2 library 
# to style charts in a more professional and presentable manner.

# Now we are going to transition to geo-mapping using Leaflet!

# First read in the data

stations = read.csv('Ch7_bike_station_locations.csv')

# Call the leaflet and magrittr libraries
library(magrittr)
library(leaflet)

# Now view the power of the leaflet package

leaflet() %>%
  addTiles() %>%
  addMarkers(data = stations, ~longitude, ~latitude, popup = stations$popup)

# What a cool mapping of each bike station!

# Pay attention to how longitude and latitude appear in the parenthesis. 
# Longitude goes first, then latitude goes second. This does affect the view.

# Now we are going to add a popup to each marker indication the station location number
# to the user after they click.

stations$popup = paste0('Station Location #',
                        seq(1, nrow(stations)))

# Next we are going to attempt to figure out the optimal route for a service truck 
# to take so that way they are maximizing their efficiency 

# To do this we are going to use a different data set that has already found an 
# optimal solution from the Traveling Salesman Problem (TSP) package in R.

short_path = read.csv('Ch7_optimal_maint_route.csv')

# view the first six lined in the data

head(short_path)

# Display the number of rows in the data

nrow(short_path)

# We are going to map the shortest path using leaflet to geo-map the solution

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = short_path, lat = ~latitude,
                   lng = ~longitude, radius = 3,
                   popup = ~popup) %>%
  addPolylines(data = short_path, lat = ~latitude, 
               lng = ~longitude, color = '#A93E36',
               opacity = .7)

# Notice how adding the addPolylines() command allowed us to have straight lines between
# the different data points. This is an effective way to show the optimal route for 
# the service bus to take. 

# There was one more section in this chapter that focused on utilizing rCharts however,
# due to my personal updated version of R I am unable to download and use that library.

# This is the end of the content for Chapter 7. 


















