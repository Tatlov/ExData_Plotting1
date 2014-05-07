# tests for plot1.R
source( "plot1.R" )

# to run the tests, use RUnit
# library( RUnit )
# printTextProtocol( runTestFile("plot1_test.R") )

test_get_data <- function(){
    # row out of file
    #2/2/2007;00:11:00;1.484;0.222;242.740;6.200;0.000;0.000;18.000
    power_consumption <- get_data()
    date_time <- as.POSIXlt("2007-02-02 00:11:00")
    column_name <- "Global_active_power"
    global_active_power_target <- 1.484 #kW
    checkEquals(global_active_power_target, 
                power_consumption[date_time == power_consumption[,"Date_time"], 
                                    column_name])
}

test_extract_date_range <- function(){
    input_data_frame <- data.frame(Date = c("31/1/2007", "2/2/2007", "16/11/2011"), 
                                   Time = c("23:59:00","00:11:00", "17:36:01"),
                                   x = c(1,2,3) )
    target <- data.frame(x = 2, Date_time = as.POSIXct("2007-2-2 00:11:00"),
                         row.names = 2L)
    checkIdentical(target, extract_date_range(combine_date_and_time(input_data_frame)))
}

test_combine_date_and_time <- function(){
    input_data_frame <- data.frame(Date = c("2/2/2007","16/11/2011"), 
                                   Time = c("00:11:00","17:36:01"),
                                   x = c(1,2) )
    target <- data.frame(x = c(1,2),
                         Date_time = as.POSIXct(c("2007-2-2 00:11:00",
                                                  "2011-11-16 17:36:01")))

    checkIdentical(target, combine_date_and_time(input_data_frame))
}


