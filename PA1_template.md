---
title: "Reproducible Research: Peer Assessment 1"
author: "Daniel Cevallos"
date: "12/05/2023"
output: 
  html_document:
    keep_md: true
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 



## Loading and preprocessing the data
Set Data files names

```r
library(ggplot2)
if(!file.exists("./data")){dir.create("./Data")}
zipFile <- "./Data/repdata_data_activity.zip"

if (!file.exists("./Data/activity.csv")) {
  unzip(zipFile, overwrite = T, exdir = "./Data")
}
```
Read csv file

```r
activity<- read.csv("./Data/activity.csv")
```

## What is mean total number of steps taken per day?

```r
steps_by_day <- aggregate(steps ~ date, activity, sum)
hist(steps_by_day$steps, main = paste("Total Steps Per Day"), col="darkgreen",xlab="Steps",breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Mean steps per day

```r
mean(steps_by_day$steps)
```

```
## [1] 10766.19
```
## What is the average daily activity pattern?
Calculate average steps for each interval for all days

```r
steps_interval <- aggregate(steps ~ interval, activity, mean)
```

Plot the Average Number Steps per Day by Interval

```r
plot(steps_interval$interval,steps_interval$steps, type="l", xlab="Interval", ylab="Steps",main="Average Number of Steps per Day", col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Find interval with most average steps

```r
steps_interval[which.max(steps_interval$steps),1]
```

```
## [1] 835
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


Using Mean for the day compute missing values and create new dataset

```r
stepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(stepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
new_activity <- activity
new_activity$steps <- fillNA
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
stepsIntegrated <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(stepsIntegrated$steps, main = paste("Total Steps per Day"), col="darkred", xlab="Steps")
hist(steps_by_day$steps, main = paste("Total Steps per Day"), col="darkblue", xlab="Steps", add=T)
legend("topright", c("IN", "Non-In"), col=c("darkblue", "darkred"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean

```r
mean(stepsIntegrated$steps)
```

```
## [1] 10766.19
```

Median

```r
median(stepsIntegrated$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment?

```r
mean(stepsIntegrated$steps) - mean(steps_by_day$steps)
```

```
## [1] 0
```


```r
median(stepsIntegrated$steps) - median(steps_by_day$steps)
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?
Created a plot to compare and contrast number of steps between the week and weekend

```r
myweekdays <- c("lunes", "martes", "miércoles", "jueves", 
              "viernes")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date,  "%Y-%m-%d")),myweekdays), "Weekday", "Weekend"))
stepsIntegrated <- aggregate(steps ~ interval + dow, new_activity, mean)

library(lattice)
xyplot(stepsIntegrated$steps ~ stepsIntegrated$interval|stepsIntegrated$dow, main="Average Steps per Day",xlab="Interval", ylab="Steps",layout=c(1,2), type="l", col="orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
