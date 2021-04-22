# Andrew Mc Daid - L00135038
# Data Science - Class Test 2

# -------------------------------------------------------------
# Q1 - Read Dataset into a Dataframe called london_crime. Then show
# The structure of the dataset. Use The paste() function to amalgamate month
# and year variables into a new variable called Date
# -------------------------------------------------------------

# Reading dataset into a DataFrame
london_crime <- read.csv("london-crime-data.csv", na = "")


# Amalgamating month and year into a variable called Date. As there is no day
# variable, day will be added onto the Date Variable as the first of each month.
# This is to get it to work with the as.Date() function
london_crime$Date <- paste(london_crime$month, london_crime$year, "01", sep='/')

# Check if the Date variable has been added correctly and 
# amalgamated month and year
head(london_crime)

# -------------------------------------------------------------
# Q2 - The dataframe should only retain The variables: borough
# Major_category, Minor_category, value and Date
# These should then be converted to more appropriate names
# Discard the unneeded variables
# -------------------------------------------------------------

# Check current colnames
names(london_crime)

# Remove ï..lsoa_code, year and month Columns as they are not needed
london_crime$ï..lsoa_code <- NULL
london_crime$year <- NULL
london_crime$month <- NULL



#Change The Names Of The remaining Columns to more appropriate ones
colnames(london_crime)[1] = "Borough"
colnames(london_crime)[2] = "MajorCategory"
colnames(london_crime)[3] = "SubCategory"
colnames(london_crime)[4] = "Value"
colnames(london_crime)[5] = "CrimeDate"

# Check Column names again to verify the correct Columns have been removed and
# The names have been changed in the other 5 Columns
names(london_crime)


# -------------------------------------------------------------
# Q3 - Convert CrimeDate so that it is a variable of type Date
# Make sure to confirm it has changed type by showing the Structure and 
# content of the date variable
# -------------------------------------------------------------

# Use str command to see that CrimeDate is currently of type - chr
str(london_crime)

# Use the as.Date() function to convert it to a Date with the correct format
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format ="%m/%Y/%d")

# Check if the Date look to be in the right format - YYYY-MM-DD 
# using the str command
str(london_crime)

#Use the head command to see if the format is correct for the first few rows
head(london_crime)


# -------------------------------------------------------------
# Q4 - Plot a chart to show the summary of Borough information
# This is to view where most of the crimes occur.
# Use the summary() function to display the data in a chart with
# a suitable chart title and axes labels
# Add a comment showing which Borough has the highest level of crime
# Add another comment to show which area has the lowest level of crime
# -------------------------------------------------------------

# Change Borough to a factor in order to plot it correctly
london_crime$Borough <- as.factor(london_crime$Borough)

# Plot Borough, this orders the Bars in the plot by Alphabetical Boroughs
plot(london_crime$Borough)
summary(london_crime$Borough)

# Add titles and labels to the plot to define it better
attach(london_crime)
display_settings <- par(no.readonly = TRUE)

plot(Borough, main="Number Of Crimes Committed By Borough"
     , ylab="# Of Crimes", xlab="London Boroughs")

# From the Plot and summary information we can see -

# ***
# Borough with Highest Crime - Croydon with 5226 Crimes
# Borough with Lowest Crime - City Of London with 86 Crimes
# ***

# -------------------------------------------------------------
# Q5 - Display MajorCategory in a Pie Chart. Using data output from
# Summary function, determine highest and lowest categories of Crime
# In London, then display in a chart using pie() function.
# Add a comment to indicate which Category had highest level of crimes
# Add another to indicate which Category had lowest level of Crime
# -------------------------------------------------------------

# Change MajorCategory to a factor in order to plot it correctly
london_crime$MajorCategory <- as.factor(london_crime$MajorCategory)

# Plot The Pie Chart
pie(table(london_crime$MajorCategory))

# View The Summary Data to Check If The Chart Is Correct
summary(london_crime$MajorCategory)

# Add Titles To the Pie Chart to define it better
attach(london_crime)
display_settings <- par(no.readonly = TRUE)

pie(table(MajorCategory), main="London Crime By Category")

# From the Pie Chart and Summary Info we can see -

# ***
# Category with Highest Number Of Crimes - Theft and Handling with 33,759 Crimes
# Category with Lowest Number Of Crimes - Sexual Offenses with 917 Crimes
# ***


# -------------------------------------------------------------
# Q6 - Categorise each Borough into the general Area it lies 
# Within London. Use The Table In Assessment Test 2 
# Create a new variable called Region and store the correct
# region for each Borough
#
# Check each borough has been successfully added to a region
# This can be done by checking if any Borough has an NA value
# If you find any region with NA, replace them with a 
# Suitable region. Indicate in Code how this was checked and
# any decisions that needed to be made
# -------------------------------------------------------------

# Create a new "Region" variable 
# and assign each Borough to a region using the table in the Assessment sheet
# The pipe command separates each Borough out
attach(london_crime)
london_crime$Region[Borough == "Barking and Dagenham" | Borough == "Bexley" |
                    Borough == "Greenwich" | Borough == "Havering" |
                    Borough == "Kingston upon Thames" | Borough == "Newham" |
                    Borough == "Redbridge" | Borough == "Wandsworth"] <- "East"

london_crime$Region[Borough == "Brent" | Borough == "Ealing" |
                      Borough == "Hammersmith and Fulham" | Borough == "Harrow" |
                      Borough == "Hillingdon" | Borough == "Hounslow" |
                      Borough == "Richmond upon Thames"] <- "West"

london_crime$Region[Borough == "Barnet" | Borough == "Camden" |
                      Borough == "Enfield" | Borough == "Hackney" |
                      Borough == "Haringey"] <- "North"

london_crime$Region[Borough == "Bromley" | Borough == "Croydon" |
                      Borough == "Merton" | Borough == "Sutton"] <- "South"

london_crime$Region[Borough == "Islington" | Borough == "Kensington and Chelsea" |
                      Borough == "Lambeth" | Borough == "Lewisham" |
                      Borough == "Southwark" | Borough == "Tower Hamlets" |
                      Borough == "Waltham Forest" | Borough == "Westminster"] <- "Central"

# Check If There are any NA values left over or that I forgot to assign to a Region
# This line of code checks for any incomplete lines such as NA's in the dataframe
london_crime[!complete.cases(london_crime),]

# City Of London was missing from the table and therefore Region is set to NA
# for those rows. Following a google search, City of London is the very middle
# of the city. For this reason I will assign it to 'Central'

london_crime$Region[Borough == "City of London"] <- "Central"

# Check to see if the NA values are Resolved
london_crime[!complete.cases(london_crime),]

# ** There are now 0 lines of NA's. All is Resolved **



# -------------------------------------------------------------
# Q7 - Display which region has the highest recorded crime rate
# Use plot() to do this
# Label the chart and axes
#
# Add a comment to indicate which Region had highest level of crimes
# and how many were committed
# Add another to indicate which Region had lowest level of Crime
# and how many were committed
# -------------------------------------------------------------

# Change Region to a factor in order to plot it correctly
london_crime$Region <- as.factor(london_crime$Region)

# Plot Region, this orders the Bars in the plot by Alphabetical Regions
plot(london_crime$Region)
summary(london_crime$Region)

# Add titles and labels to the plot to define it better
attach(london_crime)
display_settings <- par(no.readonly = TRUE)

plot(Region, main="Number Of Crimes Committed By Region"
     , ylab="# Of Crimes", xlab="London Regions")

# From the Plot and summary information we can see -

# ***
# Region with Highest Crime - Central with 28,591 Crimes
# Region with Lowest Crime - South with 15,487 Crimes
# ***


# -------------------------------------------------------------
# Q8 - Extract a subset of data with the highest level of crime
# and also extract a subset of the lowest level of crime
# Discuss the major crime categories of both regions
# -------------------------------------------------------------
attach(london_crime)
highest_region <- subset(london_crime, Region == 'Central')
lowest_region <- subset(london_crime, Region == 'South')

summary(highest_region$MajorCategory)

# The highest crime region of Central London seems to have alot of Thefts
# aserll as quite alot of Violence against the person related crimes. I imagine
# This is due to the amount of people and vehicles passing through the center of 
# The city. It is a busy place and this is no doubt why these crimes are high

summary(lowest_region$MajorCategory)

# Similarily, the region of South london also has the same two top categories likely for
# Similar reason although city-wide they are also by far the two highest crime types
# South london is the lowest crime region


# -------------------------------------------------------------
# Q9 - Using info from Summary(), plot the contents of highest_region
# and lowest_region side by side. Make sure the Y axis on both charts
# are shown in the same scale and that both charts have suitably labeled
# Titles, x and Y axeses and text on the x- axis should be Vertical
# -------------------------------------------------------------

# To plot multiple charts side by side we use the mfrow paramater from
# the par() function 

opar <- par(no.readonly = TRUE)
par = opar

par(mfrow=c(1,2))
plot(highest_region$MajorCategory, main="Number Of Crimes Committed By Region")
plot(lowest_region$MajorCategory, main="Number Of Crimes Committed By Region")

#Currently the Y Axis are different on both charts so I will set a range
graph_range <- range(0, 9000)
graph_range

# Both graphs will now have a max of 9000 on the Y axis
# I also set X axis text to be vertical using las=3. However doing it this way
# , the text extended beyond the plot
par(mfrow=c(1,2))
plot(highest_region$MajorCategory, main="Category Of Crimes Committed In Central London", 
     ylab="# Of Crimes", xlab="Category Of Crime", las=3, ylim=graph_range)
plot(lowest_region$MajorCategory, main="Category Of Crimes Committed In South London", 
     ylab="# Of Crimes", xlab="Category Of Crime", las=3, ylim=graph_range)


# -------------------------------------------------------------
# Q10 - Using write.csv() save london_crime DF as london-crime-modified.csv
# Sync the script file as well as london-crime-modified.csv
# -------------------------------------------------------------

# Saving london_crime as 'london-crime-modified.csv'
write.csv(london_crime, file="london-crime-modified.csv")

# Just in case I have also saved and uploaded the first Plots file to github
# This is beacuse on my computer the plot couldnt display all the column names
# in some plots. I had to export and set a custom number of pixels to 
# see them more clearly

