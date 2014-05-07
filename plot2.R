# Course Project 1 - Plot 2
# for Coursera's Exploratory Data Analysis

# The function get_data_and_plot reads the file household_power_consumption.txt
# creates a png file plot1.png 
# containing a plot of a household's global active power
# between 2007-02-01 and 2007-02-02.

# The data was obtained from
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# and unzipped into a file named household_power_consumption.txt
# in the same directory as this file plot2.R is located in.

# top-level function
get_data_and_plot <- function(){
    # get the data for the given date range
    power_consumption <- get_data()
    # create the plot and store as plot1.png
    make_and_store_plot2(power_consumption)
}

make_and_store_plot2 <- function(power_consumption){
    # open png file device with transparent background
    png(filename = "plot2.png", bg = "transparent")
    # create a plot using the Base plotting system
    with(power_consumption,plot(Date_time,Global_active_power,
                                type="l",
                                xlab = "",
                                ylab = "Global Active Power (kilowatts)"))
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
    # select the data between 2007-02-01 and 2007-02-02
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