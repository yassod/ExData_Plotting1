plot1 <- function() {
        
        #this function will load the data, downloading it if needed
        load_data_file <- function() {
                #load dplyr so that we can mutate the data frame
                library (dplyr)
                
                #set working directory to my synced fork
                #setwd("/Users/yasso/Documents/Git/Exploratory/ExData_Plotting1")
                
                files <- dir()
                if (!("household_power_consumption.txt" %in% files)) {
                        #we need to download the ZIP file and extract our dataset
                        #we will leave the text file in our working directory
                        temp <- tempfile()
                        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
                        plotme <- read.table(unz(temp, "household_power_consumption.txt"),  sep=";", header=TRUE, na.strings="?")
                        # unz(temp, "household_power_consumption.txt")
                        unlink(temp)
                } else {
                        
                        #read the whole data file initially
                        plotme <- read.table("household_power_consumption.txt", sep=";", header=TRUE, na.strings="?")
                }
                
                #plotme2 <- mutate(plotme, Date = as.Date(Date), Time = strptime(Time, format="%H:%M:%S"))
                #dplyr throws an error when I try to convert the Time to a POSIXlt format
                # Error: `mutate` does not support `POSIXlt` results 
                plotme2 <- mutate(plotme, Date2 = as.Date(Date, format = "%d/%m/%Y"))
                
                #read just the rows that have our selected dates
                #We will only be using data from the dates 2007-02-01 and 2007-02-02.
                #Start row is XXXXX and end row is XXXXX
                #You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
                #Note that in this dataset missing values are coded as ?.
                plotme3 <- plotme2[plotme2$Date2 %in% as.Date(c('2007-02-01', '2007-02-02')),]
                rm(list = c("plotme", "plotme2"))
                return(plotme3)
        }
        
        #we want to plot a single graph, so we set the grid to 1X1
        par(mfrow = c(1,1))
        
        plotdata <- load_data_file()
        
        #open the PNG graphics device
        png("plot1.png", width = 480, height = 480)

        #plot the histogram
        hist(plotdata$Global_active_power, col="red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power" )
        
        #close the PNG device
        dev.off()
}


