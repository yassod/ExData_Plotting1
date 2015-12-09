plot2 <- function() {
	
	#this function downloads the data set if it is not in the current directory
        load_data_file <- function() {
                #load dplyr since we mutate later
                library(dplyr)
                
                #set working directory to my synced fork
                #setwd("/Users/yasso/Documents/Git/Exploratory/ExData_Plotting1")
                
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

        
        #Make sure that just one graph will be placed
        par(mfrow = c(1,1))
        
        #read the file into a data frame
        plotdata <- load_data_file()
        
        #open the graphic device and set dimensions
        png("plot2.png", width = 480, height = 480)
        
        #set up the layout for the graph but don't plot any points
        with(plotdata, plot(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type="n"))
        #plot the lines connecting the points
        with(plotdata, lines(strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S"), Global_active_power))
        
        #close the device
        dev.off()

      }