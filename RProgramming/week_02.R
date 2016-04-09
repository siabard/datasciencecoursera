### Your First R Function

## Function add 2 variable and return its result

add2 <- function(x, y) {
  x + y
}

## Function filter vector elements larger than 10

above10 <- function(x) {
  use <- x > 10
  x[use]
}

## return vector elements only larger than n
above <- function(x, n) {
  use <- x > n
  x[use]
}

## mean of each column in a matrix

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  means
}

### Lexical Scoping

make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}



## pollutantmean
# calculate the mean of a pollutant (sulfate or nitrate)
# directory, pollutant, id


read_all_data <- function(directory, id=1:332) {
  for (i in id) {
    filename <- paste("000", i, ".csv", sep="")
    
    if(!exists("dataset")) {
      dataset <- read.csv( file.path(directory, substr(filename, nchar(filename)-6, nchar(filename))))  
    } else if(exists("dataset")) {
      tmp_dataset <- read.csv( file.path(directory,substr(filename, nchar(filename)-6, nchar(filename))))
      dataset <- rbind(dataset, tmp_dataset)
      rm(tmp_dataset)
    }
  
    
  }
  dataset
  
}
pollutantmean <- function(directory, pollutant, id=1:332) {
  
  
  pollut_data <- read_all_data(directory, id)
  mean(pollut_data[[pollutant]], na.rm=TRUE)
  
}

complete <- function(directory, id=1:332) {
  
  alldata <- read_all_data(directory, id)
  for(i in id) {
    if(!exists("dataset")) {
      dataset <- cbind(id=i, nobs = nrow(alldata[!is.na(alldata$sulfate) & !is.na(alldata$nitrate) & alldata$ID==i,]))
    } else {
      tmp_dataset <- cbind(id=i, nobs = nrow(alldata[!is.na(alldata$sulfate) & !is.na(alldata$nitrate) & alldata$ID==i,]))
      dataset <- rbind(dataset, tmp_dataset)
      rm(tmp_dataset)
    }
  }
  dataset
}

      
corr <- function(directory, threshold = 0) {
  # find threshold data
  # use comdata as global
  
  # comdata <- complete(directory)
  thres <- comdata[comdata[,2] > threshold,]
  
  
  corresult <- c()
  for(i in thres[,1]) {
    alldata <- read_all_data(directory, i)
    
    corresult <- c(corresult, cor( alldata$sulfate, alldata$nitrate , use="complete.obs"))
    
    rm(alldata)
  }
  # find threshold data
  corresult
}
