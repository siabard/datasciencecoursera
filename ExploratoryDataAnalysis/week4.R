#### WEEK 4

#  We stored the 1999 data in the array pm0 for you. Run the R command dim now to see its dimensions.
dim(pm0)
head(pm0)
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- make.names( cnames[[1]][wcol])

# The measurements of particulate matter
# (pm25) are in the column named Sample.Value. 
# Assign this component of pm0 to the variable x0

x0 <- pm0$Sample.Value
str(x0)

# Exactly what percentage of values are missing in this vector? Use the R function mean with
# | is.na(x0) as an argument to see what percentage of values are missing (NA) in x
mean( is.na(x0))

# First assign the output of make.names(cnames[[1]][wcol]) to names(pm1).
names(pm1) <- make.names(cnames[[1]][wcol])

# Find the dimensions of pm1 with the command dim.
dim(pm1)

# Create the variable x1 by assigning to it the Sample.Value component of pm1
x1 <- pm1$Sample.Value

# Now let's see what percentage of values are missing in x1
mean(is.na(x1))

# Now let's look at summaries (using the summary command) for both datasets
summary(x0)
summary(x1)

#  Call the boxplot function with 2 arguments, x0 and x1.
boxplot(x0, x1)

# It might be
# more informative to call boxplot on the logs (base 10) of x0 and x1. Do this now using log10(x0)
# and log10(x1) as the 2 arguments.

boxplot(log10(x0), log10(x1))

# Let's count how many negative values there are.
# form the vector negative by assigning to it the boolean x1<0
negative <- x1 < 0

# Now run the R command sum with 2 arguments
sum(negative, na.rm = TRUE)


# Run the R command mean with
# same 2 arguments you just used with the call to sum
mean(negative, na.rm = TRUE)

# We see that just 2% of the x1 values are negative. Perhaps that's a small enough percentage that
# we can ignore them. Before we ignore them, though, let's see if they occur during certain times of
# the year.
dates <- pm1$Date

# We see dates is a very long vector of integers. However, the format of the entries is hard to
# read. Reassign to dates the output of a call to as.Date 
dates <- as.Date(as.character(dates), "%Y%m%d")
head(dates)


# Let's plot a histogram of the months when the particulate matter measurements are negative
hist(dates[negative], "month")

# we'll try to find one monitor that was taking measurements in both 1999 and 2012
# We'll narrow our search and look just at monitors in New York State.

str(site0)


# Use the intersect command with site0 and site1 as arguments and put the result in the variable both.
both <- intersect(site0, site1)

# The subsets will filter for 2 characteristics. The first is State.Code equal to 36 (the code for
# New York), and the second is that the county.site (the component we added) is in the vector both.
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

# We want to examine a monitor with a reasonable number of measurements so let's look at the monitor
# with ID 63.2008. Create a variable pm0sub which is the subset of cnt0 (this contains just New York
# data) which has County.Code equal to 63 and Site.ID 2008.
pm0sub <- subset(cnt0, County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(cnt1, County.Code == 63 & Site.ID == 2008)

# Now we'd like to compare the pm25 measurements of this particular monitor (63.2008) for the 2 years.
x0sub <- pm0sub$Sample.Value
x1sub <- pm1sub$Sample.Value

# We'd like to make our comparison visually so we'll have to create a time series of these pm25
# measurements.
dates0 <- as.Date( as.character(pm0sub$Date), "%Y%m%d")
dates1 <- as.Date( as.character(pm1sub$Date), "%Y%m%d")

# Now we'll plot these 2 time series in the same panel using the base plotting system
par(mfrow = c(1,2), mar=c(4,4,2,1))
plot( dates0, x0sub, pch = 20 )
abline( h = median(x0sub, na.rm = TRUE), lwd=2)
plot( dates1, x1sub, pch = 20)
abline( h = median(x1sub, na.rm = TRUE), lwd=2)

#  We should really plot the points of both datasets on the same range of values on the y axis
rng <- range(x0sub, x1sub, na.rm = TRUE)

# The last avenue of this data we'll explore (and we'll do it quickly) concerns a comparison of all
# the states' mean pollution levels. This is important because the states are responsible for
# implementing the regulations set at the federal level by the EPA.


# Let's first gather the mean (average measurement) for each state in 1999
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

#  Now we'll create 2 new dataframes containing just the state names and their mean measurements for each year.
d0 <- data.frame( state = names(mn0), mean =  mn0)
d1 <- data.frame( state = names(mn1), mean =  mn1)
mrg <- merge(d0, d1, "state")

# Now we'll plot the data to see how the state means changed between the 2 years
with(mrg, plot(rep(1, 52), mrg[,2], xlim = c(.5, 2.5)))
with(mrg, points(rep(2, 52), mrg[,3]))
segments(rep(1,52), mrg[,2], rep(2, 52), mrg[,3])
mrg[mrg$mean.x < mrg$mean.y,]