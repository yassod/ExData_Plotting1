plot3 <- function() {
        #this creates the third plot
        
        #this function will load the data, downloading it if needed
        load_data_file <- function() {
          #load dplyr so that we can mutate the data frame
          library (dplyr)
          
          #set working directory to my synced fork for testing
          #setwd("/Users/yasso/Documents/Git/Exploratory/ExData_Plotting1")
          
          files <- dir()
          if (!("household_power_consumption.zip" %in% files)) {
            #we need to download the ZIP file and extract our dataset
            #we will leave the text file in our working directory
            download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip")
            unz("household_power_consumption.zip", "household_power_consumption.txt")
          }        
          
          #Note that in this dataset missing values are coded as ?.
          #read the whole data file
          plotme <- read.table(unz("household_power_consumption.zip", "household_power_consumption.txt"),  sep=";", header=TRUE, na.strings="?")
          
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
        

        
        #Make sure that just one graph will be placed
        par(mfrow = c(1,1))
        
        #read the file into a data frame
        plotdata <- load_data_file()
        
        #open the graphic device and set dimensions
        png("plot3.png", width = 480, height = 480)
        
        #find maximum Y axis value
        maxwatthours <- max(plotdata$Sub_metering_1, plotdata$Sub_metering_2, plotdata$Sub_metering_3)
        
        #set up the layout for the graph but don't plot any points
        with(plotdata, plot(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), plotdata$Sub_metering_3, ylim = c(0, maxwatthours), xlab = "", ylab = "Energy sub metering", type="n"))
        
        #plot the lines connecting the points
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Sub_metering_1))
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Sub_metering_2, col = "red"))
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Sub_metering_3, col = "blue"))
        
        #create legend
        legend("topright", pch = 46, lwd = 2, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        #close the device
        dev.off()



      }