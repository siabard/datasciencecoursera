## Week 4

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile = "./data/hid.csv" )
hid <- read.csv("./data/hid.csv")
split_names <- strsplit(names(hid), "wgtp")


download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", destfile = "./data/FGDP.csv")
fgdp <- read.csv("./data/FGDP.csv", skip=4, nrows = 215)

fgdp_value <- sapply(fgdp$X.4, function(x) {as.numeric(gsub(",", "", x))})
mean(sapply(fgdp$X.4, function(x) {as.numeric(gsub(",", "", x))}) , na.rm = TRUE)
grep("^United", fgdp$X.3)


download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", destfile = "./data/FEDSTATS.csv")
fed <- read.csv("./data/FEDSTATS.csv")

whoData <- merge(fgdp, fed, by.y = "CountryCode", by.x = "X")
#Special.Notes
whoData(grep("[Ff]iscal year end", whoData$Special.Notes), )
length(grep("June", whoData[grep("[Ff]iscal year end", whoData$Special.Notes), "Special.Notes"]))

install.packages('quantmod')
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

length(grep("^2012", sampleTimes))
sum(wday(sampleTimes[grep("^2012", sampleTimes)]) == 2)