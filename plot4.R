plot4 <- function() {
        #this function plots the fourth set of graphs
 
         load_data_file <- function() {
                #this function downloads the data set if it is not in the current directory
                 
                #load dplyr since we mutate later
                library(dplyr)
                
                #set working directory to my synced fork
                #setwd("/Users/yasso/Documents/Git/ExData_Plotting1")
                
                files <- dir()
                
                #Note that in this dataset missing values are coded as ?.
                #Check whether the file is in the current working directory
                #if not, download the ZIP and extract the text to a data frame
                #if the text file is present, read it into a data frame directly
                if (!("household_power_consumption.txt" %in% files)) {
                        #we need to download the ZIP file and extract our dataset
                        temp <- tempfile()
                        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
                        plotme <- read.table(unz(temp, "household_power_consumption.txt"),  sep=";", header=TRUE, na.strings="?")
                        unlink(temp)
                } else {
                        #read the whole text data file initially
                        plotme <- read.table("household_power_consumption.txt", sep=";", header=TRUE, na.strings="?")
                }

                #add a date column to the data table                
                plotme2 <- mutate(plotme, Date2 = as.Date(Date, format = "%d/%m/%Y"))
                
                #We will only be using data from the dates 2007-02-01 and 2007-02-02.
                plotme3 <- plotme2[plotme2$Date2 %in% as.Date(c('2007-02-01', '2007-02-02')),]
                
                #remove the two earlier instances of the data set
                rm(list = c("plotme", "plotme2"))
                return(plotme3)
        }




        #read the file into a data frame
        plotdata <- load_data_file()
        
        #######################
        #Start Plotting Graphs
        #######################
        
        #open the graphic device and set dimensions
        png("plot4.png", width = 480, height = 480)
        
        #Set up a 2x2 layout; we will fill by rows
        par(mfrow = c(2,2))

        #################
        #First Graph    #
        #################
        
        #set up the layout for the graph but don't plot any points
        with(plotdata, plot(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Global_active_power, xlab = "", ylab = "Global Active Power", type="n"))
        #plot the lines connecting the points
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Global_active_power))

        #################
        #Second Graph   #
        #################
        #set up the layout for the graph but don't plot any points
        with(plotdata, plot(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Voltage, xlab = "datetime", ylab = "Voltage", type="n"))
        #plot the lines connecting the points
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Voltage))
        
        
        #################
        #Third Graph    #
        #################
        
        #find maximum Y axis value
        maxwatthours <- max(plotdata$Sub_metering_1, plotdata$Sub_metering_2, plotdata$Sub_metering_3)
        
        #set up the layout for the graph but don't plot any points
        with(plotdata, plot(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), plotdata$Sub_metering_3, ylim = c(0, maxwatthours), xlab = "", ylab = "Energy sub metering", type="n"))
        
        #plot the lines connecting the points
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Sub_metering_1))
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Sub_metering_2, col = "red"))
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Sub_metering_3, col = "blue"))
        
        #create legend
        legend("topright", pch = 46, lwd = 2, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")
        
        
        #################
        #Fourth Graph   #
        #################
        #set up the layout for the graph but don't plot any points
        with(plotdata, plot(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type="n"))
        #plot the lines connecting the points
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Global_reactive_power))
        
        
        
        #close the device
        dev.off()
        
        

 }