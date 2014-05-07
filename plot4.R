# Course Project 1 - Plot 4
# for Coursera's Exploratory Data Analysis

# The function get_data_and_plot reads the file household_power_consumption.txt
# creates a png file plot4.png 
# containing four graphs of summarizing a household's power consumption
# between 2007-02-01 and 2007-02-02.

# The data was obtained from
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# and unzipped into a file named household_power_consumption.txt
# in the same directory as this file plot4.R is located in.

# to create the plot do
# source("plot4.R")
# get_data_and_plot()
get_data_and_plot <- function(){
    # get the data for the given date range
    power_consumption <- get_data()
    # create the plot and store as plot4.png
    make_and_store_plot4(power_consumption)
}

make_and_store_plot4 <- function(power_consumption){
    # open png file device with transparent background
    png(filename = "plot4.png", bg = "transparent")
    # create four graphs in one plot using the Base plotting system
    par(mfrow = c(2,2)) # 4 graphs in a 2x2 layout
    # plot 1, top left
    with(power_consumption,plot(Date_time,Global_active_power, # x, y data
                                type="l", # draw a line
                                xlab = "", # no x-axis label
                                ylab = "Global Active Power")) # y-axis label
    # plot 2, top right
    with(power_consumption,plot(Date_time,Voltage,
                                type="l",
                                xlab = "datetime",
                                ylab = "Voltage"))
    
    # plot 3, bottom left
    # create axis and labels without points
    with(power_consumption,plot(Date_time,Sub_metering_1,
                                type = "n",
                                xlab = "",
                                ylab = "Energy sub metering"))
    # add energy sub metering points
    with(power_consumption,points(Date_time,Sub_metering_1,
                                  type = "l", col = "black"))
    with(power_consumption,points(Date_time,Sub_metering_2,
                                  type = "l", col = "red"))
    with(power_consumption,points(Date_time,Sub_metering_3,
                                  type = "l", col = "blue"))
    # add the legend
    legend("topright",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col = c("black", "red", "blue"), lty = "solid", 
           bty = "n") # do not draw a box around the legend
    
    # plot 4, bottom right
    with(power_consumption,plot(Date_time,Global_reactive_power,
                                type="l",
                                xlab = "datetime"))
    # close the file device
    dev.off()
}

get_data <- function(){
    # path to file
    file_path <- file.path("household_power_consumption.txt")
    # set the column types
    column_types <- c(rep("character",2),rep("numeric",7))
    # read the whole file
    power_consumption <- read.table( file_path, header = TRUE, sep = ";", 
                                     na.strings = "?",
                                     comment.char = "", 
                                     colClasses = column_types)
    # create a Date_time column and delete Date and Time columns
    # to be able to easily select date ranges
    power_consumption <- combine_date_and_time(power_consumption)
    # select the data of 2007-02-01 and 2007-02-02
    power_consumption <- extract_date_range(power_consumption)
    # return the data frame
    power_consumption
}

extract_date_range <- function(df){
    # select date range
    # this is inclusive of start_date and exclusive of end_date
    start_date <- as.POSIXct("2007-02-01")
    end_date <- as.POSIXct("2007-02-03")
    # return data frame rows within date range
    df[df[,"Date_time"] >= start_date & df[,"Date_time"] < end_date, ]
}

combine_date_and_time <- function(df){
    # column names
    time_column_name <- "Time"
    date_column_name <- "Date"
    date_time_column_name <- "Date_time"
    # formats
    date_format <- "%d/%m/%Y"
    time_format <- "%H:%M:%S"
    date_time_format <- paste(date_format,time_format)
    #add date_time column in POSIXct format
    df[,date_time_column_name] <- as.POSIXct(strptime(paste(df[,date_column_name],
                                                            df[,time_column_name]),
                                                      date_time_format))
    #return with date and time columns removed
    df[,!(names(df) %in% c(time_column_name, date_column_name))]
}