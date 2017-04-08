#Plot 4.
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

#This will check if directory exists, if not, it will be created.
#	if (file.exists(subDir)){
#		setwd(file.path(mainDir, subDir))
#	} else {
#		dir.create(file.path(mainDir, subDir))
#		setwd(file.path(mainDir, subDir))
#}

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
#This will check if the dataset is already present.
#If one of the other scripts already have been run,
#it is not neccesary to import again.
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

#Create plot 4.
#Using par to set the canvas to contain 4 plots
#Using "with", to create the different plots for
#final png file.

#Save the original par values
opar <- par()

#Create four plots in the same image
#Save the plot to a file named plot2.png, with the dimensions
#height = 480 and width = 480

#Comment out this line, and the line with dev.off() at the bottom,
#to discard the save function
png("plot4.png", width=480, height=480, units="px", bg="white")

#Set the canvas 2 by 2
	par(mfrow=c(2,2))
#Set the margins
	mar=c(4,4,2,1)
#Set the outer margins
	oma=c(0,0,2,0)

with(dataset2, {
	
#Plot 1: (Global Active Power - Days)
	plot(Global_active_power~dateTime, type="l", 
		ylab="Global Active Power", xlab="")

#Plot 2: (Voltage - Days)  
	plot(Voltage~dateTime, type="l", 
		ylab="Voltage", xlab="Datetime")

#Plot 3: (Energy Sub Metering - Days)
	plot(Sub_metering_1~dateTime, type="l", 
		ylab="Energy Sub Metering", xlab="")
		lines(Sub_metering_2~dateTime,col='Red')
		lines(Sub_metering_3~dateTime,col='Blue')
		legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
		legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#Plot 4: (Global Reactive Power - Days)
	plot(Global_reactive_power~dateTime, type="l", 
		ylab="Global Rective Power",xlab="Datetime")
})

dev.off()

#Restore original par values
#Might give som warning messages,
#if certain values in par cant be set.
par(opar)
