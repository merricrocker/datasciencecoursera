library("data.table")

powerHH<-read.table("/Users/meredithcrocker/datasciencecoursera/household_power_consumption.txt")

#Reads data, subsets data for specified dates
powerHH <- data.table::fread(input = "household_power_consumption.txt"
                             , na.strings="?"
)

# Stops scientific notation
powerHH[, Global_active_power := lapply(.SD, as.numeric), .SDcols = c("Global_active_power")]

#Makes POSIXct date > filter and graph by time of day
powerHH[, dateTime := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]

# Filter Dates for 2007-02-01 and 2007-02-02
powerHH <- powerHH[(dateTime >= "2007-02-01") & (dateTime < "2007-02-03")]

png("plot2.png", width=480, height=480)

## Plot 2
plot(x = powerHH[, dateTime]
     , y = powerHH[, Global_active_power]
     , type="l", xlab="", ylab="Global Active Power (kilowatts)")

dev.off()