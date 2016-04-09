#Quiz 1

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", dest="./data/hid.csv")

hid <- read.csv("./data/hid.csv")

## ACR == 3, AGS == 6

agricultureLogical <- hid$ACR == 3 & hid$AGS == 6
which(agricultureLogical)

#Quiz 2
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", dest="./data/jeff.jpg", method="curl")

jpgData <- readJPEG("./data/jeff.jpg", native=TRUE)
quantile(jpgData, c(.30,.80))

#Quiz 3
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", dest="./data/GDP.csv", method="curl")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", dest="./data/FEDSTATS_Country.csv", method="curl")
gdpData <- read.csv("./data/GDP.csv", skip = 4, nrow=215)
fedStates <- read.csv("./data/FEDSTATS_Country.csv")

#whoData <- merge(fedStates, gdpData, by.x = "CountryCode", by.y = "X")
whoData <- merge(gdpData, fedStates, by.y = "CountryCode", by.x = "X")

order_who <- whoData[!is.na(whoData$X.1),]
order_who <- order_who[order(order_who$X.1, decreasing = TRUE),]

#Quiz 4

mean(order_who[order_who$Income.Group == "High income: OECD", "X.1"])
mean(order_who[order_who$Income.Group == "High income: nonOECD", "X.1"])

#Quiz 5