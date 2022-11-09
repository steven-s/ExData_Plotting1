library(dplyr)

downloadAndExtractRawDataset <- function(destdir) {
  if (!dir.exists(destdir)) {
    dir.create(destdir)
  }
  
  zipFileDest <- file.path(destdir, "exdata-data-household_power_consumption.zip")
  
  if (!file.exists(zipFileDest)) {
    print(paste("Downloading UCI Household Power Consumption dataset to", zipFileDest))
    download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = zipFileDest)
  } else {
    print("UCI Household Power Consumption dataset zip already exists")
  }
  
  extractedFile <- file.path(destdir, "household_power_consumption.txt")
  
  if (!file.exists(extractedFile)) {
    print(paste("Extracting UCI Household Power Consumption dataset to", extractedFile))
    unzip(zipFileDest, exdir = destdir)
  } else {
    print("UCI Household Power Consumption dataset file already exists")
  }
  
  extractedFile
}

loadDataset <- function(destdir=getwd()) {
  print("Loading UCI household power consumption dataset")
  dataFile <- downloadAndExtractRawDataset(destdir)
  
  powerData <- read.table(dataFile, header = TRUE, sep=";", na.strings = "?") %>% 
    mutate(Date = as.Date(Date, tryFormats=c("%d/%m/%Y"))) %>% 
    filter(Date == as.Date("2007/02/02") | Date == as.Date("2007/02/01"))
  powerData <- powerData %>% mutate(Time = strptime(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
  
  powerData
}

powerData <- loadDataset()

png(filename = "plot1.png", width = 500, height = 500)

with(powerData, 
     hist(Global_active_power, 
          main = "Global Active Power", 
          xlab = "Global Active Power (kilowatts)", 
          ylab="Frequency", 
          col="red"))

dev.off()
