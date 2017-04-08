#Plot 2.
#The actual plot script, is in the end part of this file.
#First, we need to download, import and format the data.
#There is also a check to see if the file is already downloaded.

#Set the filenames needed in this script.
	filename <- "Assignment_file.zip"
	filname_unzipped <- "household_power_consumption.txt"
#This is the from date in the assignment, change as needed.
	daterange_from <- "2007-02-01"
#This is the to date in the assignment, change as needed.
	daterange_to <- "2007-02-02"

#This will check if file is present, if not, it will be downloaded and unzipped.
if (file.exists(filename)){
  unzip(zipfile = filename)
} else {
  path <- getwd()
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  download.file(url, file.path(path, filename))
  unzip(zipfile = filename)
}
#Import the dataset
if (exists("dataset")) {
} else {
	dataset <- read.csv(filname_unzipped, header = TRUE, sep = ";",
	na.strings = "?", colClasses = c('character','character',
	'numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
}
#Convert the date to a format that R like.
	dataset$Date <- as.Date(dataset$Date, format = "%d/%m/%Y")
#Subset the data, to the daterange declared earlier
	dataset2 <- subset(dataset, Date >= daterange_from & Date <= daterange_to)
#Use only complete cases
	dataset2 <- dataset2[complete.cases(dataset2),]
#Create new date and time field 
	dateTime <- paste(dataset2$Date, dataset2$Time)
#Set the name of the new field
	dateTime <- setNames(dateTime, "DateTime")
#Merge the new date and time field with the subset already created
	dataset2 <- cbind(dateTime, dataset2)
#Remove the original date and time field, not longer needed
	dataset2 <- dataset2[ ,!(names(dataset2) %in% c("Date","Time"))]
#Convert the date time field
	dataset2$dateTime <- as.POSIXct(dateTime)

#Create plot 2.
#Save the plot to a file named plot2.png, with the dimensions
#height = 480 and width = 480

#Comment out this line, and the line with dev.off() at the bottom,
#to discard the save function
png("plot2.png", width=480, height=480, units="px", bg="white")

	with(dataset2, {
    plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
	})

dev.off()