library("data.table")

powerHH<-read.table("/Users/meredithcrocker/datasciencecoursera/household_power_consumption.txt")

#Reads data, subsets data for specified dates
powerHH <- data.table::fread(input = "household_power_consumption.txt"
                             , na.strings="?"
)

# Stops scientific notation
powerHH[, Global_active_power := lapply(.SD, as.numeric), .SDcols = c("Global_active_power")]

# Change Date Column to Date Type
powerHH[, Date := lapply(.SD, as.Date, "%d/%m/%Y"), .SDcols = c("Date")]

# Filter Dates for 2007-02-01 and 2007-02-02
powerHH <- powerHH[(Date >= "2007-02-01") & (Date <= "2007-02-02")]

png("plot1.png", width=480, height=480)

## Plot 1
hist(powerDT[, Global_active_power], main="Global Active Power", 
     xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")

dev.off()