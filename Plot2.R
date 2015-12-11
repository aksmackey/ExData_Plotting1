# This function downloads electric power consumption data (if needed) 
# then creates a plot with the data. 

# Please note, the data downloaded in this function is large. Approximately
# 0.3 GB of memory is necessary to complete this analysis. 

plot2 <- function(workdir = './') {
  # Set the working directory
  setwd(workdir)
  
  # Check that the file hasn't already been downloaded
  if (file.exists('household_power_consumption.txt') == FALSE) {
    url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    download.file(url, destfile = 'exdata-data-household_power_consumption.zip', 
                  method = "curl")
    unzip('exdata-data-household_power_consumption.zip')
  }
  
  # Import the data
  preTbl <- read.table("household_power_consumption.txt", sep = ";", 
                       header = TRUE, nrows = 20)
  classes <- as.vector(sapply(preTbl, class))
  rawTbl <- read.table("household_power_consumption.txt", sep = ";", 
                       header = TRUE, colClasses = classes, na.strings = "?")
  
  # Subset the data from the days of interest
  pwrData <- subset(rawTbl, Date == "1/2/2007" | Date == "2/2/2007")
  
  # Convert date and time to a useable format
  pwrData$datetime <- strptime(paste(pwrData$Date, pwrData$Time), 
                               "%d/%m/%Y %H:%M:%S")

  # Set device
  png(file = "Plot2.png")
  
  # Build Plot A
  with(pwrData, plot(datetime, Global_active_power, type = "s", 
                     xlab = "", 
                     ylab = "Global Active Power (kilowatts)"))
  # Save Plot
  dev.off()
  
}