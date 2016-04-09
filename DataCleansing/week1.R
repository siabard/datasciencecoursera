if(!file.exists("./data")) {
  dir.create("./data")
}

download.file(url="https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD", destfile = "./data/rows.csv")
download.file(url="https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD", destfile="./data/rows.xlsx")
cameraData <- read.table("./data/rows.csv", sep=",", header = TRUE)
# cameraData <- read.csv("./data/rows.csv)


fileUrl <- "http://espn.go.com/nfl/team/_name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal=TRUE)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)
                     

library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)

download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile="./data/acs.csv")

csvData <- read.csv("./data/acs.csv")
nrow(csvData[csvData$VAL == 24 & !is.na(csvData$VAL),])
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", destfile = "./data/gov_ngap.xlsx" , mode="wb")

dat <- read.xlsx("./data/gov_ngap.xlsx", rowIndex = 18:23, colIndex=7:15, sheetIndex = 1)
sum(dat$Zip*dat$Ext,na.rm=T)

fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xmlData <- xmlTreeParse(fileUrl, useInternal = TRUE)
nodes <- xmlRoot(xmlData)

zipcodeNode <- xpathSApply(nodes, "//zipcode", xmlValue)
sum(zipcodeNode == "21231")


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", destfile = "./data/pid.csv")

DT <- fread("./data/pid.csv")

system.time({mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})

system.time({mean(DT$pwgtp15,by=DT$SEX)})

system.time({tapply(DT$pwgtp15,DT$SEX,mean)})

system.time({DT[,mean(pwgtp15),by=SEX]})

system.time({rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]})

system.time({sapply(split(DT$pwgtp15,DT$SEX),mean)})