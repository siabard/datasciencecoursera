## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

## Making smaller NEI dataset
set.seed(12345)
NEI.reduced <- NEI[sample(nrow(NEI), 1000), ]

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# Total Emission
png(filename="plot1.png")
total_emission <- with(NEI, aggregate( Emissions, by = list(year), sum))
plot(total_emission, type = "o", xlab="Year", ylab = "Total Emissions")
dev.off()

## total emission is decreased.

# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting 
# system to make a plot answering this question.
png(filename="plot2.png")
NEI.fips.24510 <- subset(NEI, fips == 24510)
total_emission.fips.24510 <-  with(NEI.fips.24510, aggregate( Emissions, by = list(year), sum))
names(total_emission.fips.24510) <- c("Year", "Total Emissions")
plot(total_emission.fips.24510, type = "o", ylab="Total Emission", xlab= "YEAR")
dev.off()

# Yes, Overall is decreased, but between 2002 and 2006 is increased.


# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system 
# to make a plot answer this question.

library(ggplot2)
library(dplyr)
library(plyr)

NEI.fips.24510.type <- ddply(NEI.fips.24510, .(type, year), summarize, Emissions = sum(Emissions))
NEI.fips.24510.type$Pollutant_Type <- NEI.fips.24510.type$type

png(filename="plot3.png")
ggplot(data = NEI.fips.24510.type, aes(x = year, y = Emissions, group = Pollutant_Type, color=Pollutant_Type)) + geom_line() + geom_point()
dev.off()
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

SCC.coal <- grep("coal", SCC$Short.Name, ignore.case = TRUE)
SCC.coal <- SCC[SCC.coal, ]
SCC.id <- as.character(SCC.coal$SCC)

NEI$SCC <- as.character(NEI$SCC)
NEI.coal <- subset(NEI, SCC %in% SCC.id)

total_emission.coal <- with(NEI.coal, aggregate(Emissions, by = list(year), sum))
names(total_emission.coal) <- c("Year", "Total Emissions")
png(filename="plot4.png")
plot(total_emission.coal, type = "o", ylab="Total Emission", xlab= "YEAR")
dev.off()
## It decrease 

## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor, ]
SCC.id <- as.character(SCC.motor$SCC)


NEI$SCC <- as.character(NEI$SCC)
NEI.motor <- subset(NEI, SCC %in% SCC.id)
NEI.motor.fips.24510 <- subset(NEI.motor, fips == 24510)

total_emission.motor <- with(NEI.motor.fips.24510, aggregate(Emissions, by = list(year), sum))
names(total_emission.motor) <- c("Year", "Total Emissions")

png(filename="plot5.png")
plot(total_emission.motor, type = "o", ylab="Total Emission", xlab= "YEAR")
dev.off()

# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?




SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor, ]
SCC.id <- as.character(SCC.motor$SCC)


NEI$SCC <- as.character(NEI$SCC)
NEI.motor <- subset(NEI, SCC %in% SCC.id)
NEI.motor.fips.24510 <- subset(NEI.motor, fips == "24510")
NEI.motor.fips.06037 <- subset(NEI.motor, fips == "06037")


total_emission.motor.24510 <- with(NEI.motor.fips.24510, aggregate(Emissions, by = list(year), sum))
total_emission.motor.06037 <- with(NEI.motor.fips.06037, aggregate(Emissions, by = list(year), sum))

total_emission.motor.24510$group <- rep("Baltimore", nrow(total_emission.motor.24510))
total_emission.motor.06037$group <- rep("California", nrow(total_emission.motor.06037))
total_emission.motor <- rbind(total_emission.motor.24510, total_emission.motor.06037)
names(total_emission.motor) = c("Year", "Emissions", "Group")

png(filename="plot6.png")
ggplot(data = total_emission.motor, aes(x = Year, y = Emissions, group = Group, color=Group)) + geom_line() + geom_point()
dev.off()