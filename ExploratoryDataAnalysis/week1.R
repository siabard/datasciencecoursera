# Base Plot

library(datasets)
data(cars)
with(cars, plot(speed, dist))

# Lattice System
# xyplot, bwplot

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout=c(4,1))

## gglopt2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data=mpg)

## Assignment
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", dest="./data/hpc.zip")

#hpc_data = read.csv("./data/hpc/household_power_consumption.txt", sep=";",na.strings="?")
hpc_data = read.csv(unz("./data/hpc.zip", "household_power_consumption.txt"), sep=";",na.strings="?")

hpc_data <- mutate(hpc_data, longdate = paste(Date, Time, sep = " "))

small_hpc_data <- subset(hpc_data
                         , as.Date(strptime(longdate, "%d/%m/%Y %H:%M:%S")) >= as.Date("2007-02-01") & 
                           as.Date(strptime(longdate, "%d/%m/%Y %H:%M:%S")) <= as.Date("2007-02-02"))

small_hpc_data$datetime = strptime(small_hpc_data$longdate, "%d/%m/%Y %H:%M:%S")
rm(hpc_data)

## PLOT 1
png(filename = "plot1.png", width=480, height=480, units = "px")
hist(small_hpc_data$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()

## PLOT 2
png(filename = "plot2.png", width=480, height=480, units = "px")
with(small_hpc_data , plot(datetime
     , small_hpc_data$Global_active_power, type="l"
     , xlab=""
     , ylab="Global Active Power(killowatts)")  
)
dev.off()

## PLOT 3
png(filename = "plot3.png", width=480, height=480, units = "px")
plot(small_hpc_data$datetime, small_hpc_data$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")  
points(small_hpc_data$datetime, small_hpc_data$Sub_metering_1, type="l")  
points(small_hpc_data$datetime, small_hpc_data$Sub_metering_2, type="l", col="red")  
points(small_hpc_data$datetime, small_hpc_data$Sub_metering_3, type="l", col="blue")  
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "Red", "blue"), lwd=1)
dev.off()

## PLOT4
png(filename = "plot4.png", width=480, height=480, units = "px")
par(mfcol = c(2,2))

plot(small_hpc_data$datetime
     , small_hpc_data$Global_active_power, type="l"
     , xlab=""
     , ylab="Global Active Power(killowatts)")  


plot(small_hpc_data$datetime, small_hpc_data$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")  
points(small_hpc_data$datetime, small_hpc_data$Sub_metering_1, type="l")  
points(small_hpc_data$datetime, small_hpc_data$Sub_metering_2, type="l", col="red")  
points(small_hpc_data$datetime, small_hpc_data$Sub_metering_3, type="l", col="blue")  
legend("topright", bty="n", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "Red", "blue"), lwd=1)


with(small_hpc_data
     , plot(datetime, Voltage, type="l"))


with(small_hpc_data
     , plot(datetime, Global_reactive_power, type="l"))
dev.off()