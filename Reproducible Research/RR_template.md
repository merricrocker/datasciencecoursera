---
title: "Reproducible Research Project 1"
author: "Meredith Crocker"
date: "10/3/2017"
output:
  html_document: default
  pdf_document: default
  md_document: varient:markdown_github
  
---


github repo: [Data Science Coursera](https://github.com/merricrocker/datasciencecoursera)

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br>
date: The date on which the measurement was taken in YYYY-MM-DD format </br>
interval: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a csv file and there are a total of 17,568 observations in this dataset. 

## Loading and preprocessing the data
```{r}
library("data.table")
library(ggplot2)
activity<-read.csv("~/datasciencecoursera/activity.csv",colClasses = c("integer", "Date", "factor"))
activity$month <- as.numeric(format(activity$date, "%m"))
noNA <- na.omit(activity)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
dim(noNA)
```
#Mean Steps Taken per day

##1. Histiogram of total daily steps
```{r}
ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```
##2. Calculate mean and median steps per day
```{r}
totalSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
```
###Mean Steps
```{r}
mean(totalSteps)
```
###Median Steps
```{r}
median(totalSteps)
```

#What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of 5-minute Intervals", x = "5-minute intervals", y = "Average Number of Steps")
```
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```
#Imputing missing values
##The total rows with NAs:
```{r}
sum(is.na(activity))
```
##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###Strategy:use the mean for that 5-minute interval to fill each NA value in the steps column.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
newData <- activity 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(newData)
sum(is.na(newData))
```
##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r}
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "steelblue",
                                             fill = "steelblue",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```
 ##Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
 ###Mean total number of steps taken per day:
 ```{r}
              newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
```
###Median total number of steps taken per day:
```{r}
newMedian <- median(newTotalSteps)
newMedian
```
###

Compare them with the two before imputing missing data:
```{r}
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
newMedian - oldMedian
```
###The new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.
#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
head(newData)
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
table(newData$weekdays)
```
##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```
