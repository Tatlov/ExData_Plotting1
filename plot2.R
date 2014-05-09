# Course Project 1 - Plot 2
# for Coursera's Exploratory Data Analysis

# The function get_data_and_plot
# gets the data and
# creates a png file plot2.png 
# containing a plot of a household's global active power vs time
# between 2007-02-01 and 2007-02-02.

# If the household_power_consumption.txt file does not already exist, 
# the data is obtained from
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# and unzipped into a file named household_power_consumption.txt.

# to create the plot run
# source("plot2.R")
# get_data_and_plot()

get_data_and_plot <- function(){
    # download the data and unzip if zip file is not already in the directory
    download_file_and_unzip()
    # get the data for the given date range
    power_consumption <- get_data()
    # create the plot and store as plot2.png
    make_and_store_plot2(power_consumption)
}

################################################################################
# plotting
################################################################################

# Note:I decided to make the background of the four plots transparent 
# in order to reproduce the reference plots exactly using the "bg" parameter
# taught in the slide 10 of "The Base Plotting System in R" presentation 
# of the course (e.g., for the Plot 1, see the Instructor's repository image 
# https://github.com/rdpeng/ExData_Plotting1/blob/master/figure/unnamed-chunk-2.png).
# Please consider that the plots in the Instructor's README file 
# (https://github.com/rdpeng/ExData_Plotting1) are shown with a white background, 
# even when all the plots have in reality a transparent one.  
# When a transparent image is overlay on a white background, 
# it seems to have this background color.

make_and_store_plot2 <- function(power_consumption){
    # open png file device with transparent background
    png(filename = "plot2.png", bg = "transparent")
    # create a plot using the Base plotting system
    with(power_consumption, plot(Date_time,Global_active_power, #x, y data
                                 type="l", # plot a line
                                 xlab = "", # no x-axis label
                                 ylab = "Global Active Power (kilowatts)")) # y-axis label
    # close the file device
    dev.off()
}

###############################################################################
# getting the data
#
# tests for getting the data are in plot1_test.R
###############################################################################

get_data <- function(){
    # path to file
    file_path <- file.path("household_power_consumption.txt")
    # set the column types
    column_types <- c(rep("character",2), rep("numeric",7))
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
    # extract the rows of data frame df 
    # corresponding to 2007-02-01 and 2007-02-02
    
    # select date range
    # this is inclusive of start_date and exclusive of end_date
    start_date <- as.POSIXct("2007-02-01")
    end_date <- as.POSIXct("2007-02-03")
    # return data frame rows within date range
    df[df[,"Date_time"] >= start_date & df[,"Date_time"] < end_date, ]
}

combine_date_and_time <- function(df){
    # combine the Date and Time columns into a single date_time column
    
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


################################################################################
# Downloading the data
################################################################################

download_file_and_unzip <- function(){
    # download the raw data from file url and unzip
    # if the txt file does not already exist
    
    # set path to zip archive
    file_name <- "exdata_data_household_power_consumption.zip"
    file_path <- file.path(".",file_name)
    # check if the txt file exists
    if (!file.exists(file.path(".","household_power_consumption.txt"))){
        # check if the zip file already exists
        if (!file.exists(file_path)){
            #if the file does not exist, download it
            file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
            download.file(file_url, destfile = file_path, method = "curl")
            # store the download date, url and filename in a csv file
            date_file_name <- "source_info_exdata_data_household_power_consumption.csv"
            date_file_path <- file.path(".",date_file_name)
            write.csv(data.frame(date_downloaded = date(),
                                 file_url = file_url,
                                 file_name = file_name),
                      file = date_file_path)
        }
        
        # unzip the file
        unzip(file_path)
    }
}
