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

png(filename = "plot3.png", width = 500, height = 500)

with(powerData, 
     plot(Time,
          Sub_metering_1,
          type = "n",
          xlab = NA,
          ylab = "Energy sub metering")
     )
with(powerData,
     lines(Time,
          Sub_metering_1,
          type = "l"))
with(powerData,
     lines(Time,
          Sub_metering_2,
          type = "l",
          col = "red"))
with(powerData,
     lines(Time,
          Sub_metering_3,
          type = "l",
          col = "blue"))

legend("topright", lty = 1, lwd = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()
