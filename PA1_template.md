Reproducible Research, Peer Assessment 1
========================================================

Introduction
------------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This analysis makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data
-------------------
The data for this analysis can be downloaded from this web site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Analysis:
-------------------

### Loading and preprocessing the data

* Load the data

```r
activity_data <- read.csv("activity.csv",colClasses=c("date"="Date"))
```
* Process/transform the data (if necessary) into a format suitable for this analysis

### What is mean total number of steps taken per day?

For this part of the analysis, I will ignore the missing values in the dataset.

* Here is a histogram of the total number of steps taken each day

```r
steps_per_day <- tapply(activity_data$steps, activity_data$date, sum, na.rm=TRUE)
hist(steps_per_day, breaks=20, xlab="Steps per Day", ylab="Frequency",
     main="Histogram of Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
* Here is a report of the mean and median total number of steps taken per day

```r
mean(steps_per_day)
```

```
## [1] 9354
```

```r
median(steps_per_day)
```

```
## [1] 10395
```

### What is the average daily activity pattern?

* Here is a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_per_interval <- tapply(activity_data$steps, activity_data$interval, mean, na.rm=TRUE)
plot(names(steps_per_interval), steps_per_interval, type="l", 
     main="Average Steps per 5-minute Interval",
     xlab="Interval", ylab="Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(which.max(steps_per_interval))
```

```
## [1] "835"
```

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Here is the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(which(is.na(activity_data$steps)==TRUE))
```

```
## [1] 2304
```
* I will create a new dataset that is equal to the original dataset but with the missing data filled in, using the average of values for that interval over all days.

```r
imputed_data <- activity_data

for (i in 1:nrow(imputed_data)) {
    if (is.na(imputed_data[i,1])) {
        imputed_data[i,1] <- as.integer(round(steps_per_interval[imputed_data[i,3]/5+1]))
    }
    if (weekdays(imputed_data[i,2]) == "Saturday" || 
            weekdays(imputed_data[i,2]) == "Sunday") {
        imputed_data[i,4] <- "weekend"
    } else imputed_data[i,4] <- "weekday"
}
```
* Here is a histogram of the total number of steps taken each day and a report of the mean and median total number of steps taken per day. These values differ from the estimates from the first part of the analysis. Because missing data was imputed based on the averages, the estimates of the total daily number of steps increased.

```r
isteps_per_day <- tapply(imputed_data$steps, imputed_data$date, sum, na.rm=TRUE)
hist(isteps_per_day, breaks=20, xlab="Imputed Steps per Day", ylab="Frequency",
     main="Histogram of Imputed Steps per Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(isteps_per_day)
```

```
## [1] 10282
```

```r
median(isteps_per_day)
```

```
## [1] 10395
```

### Are there differences in activity patterns between weekdays and weekends?

* Adding a new factor variable in the imputed dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
imputed_data[,4] <- as.factor(imputed_data[,4])
names(imputed_data)[4] <- "daytype"
```
* Here is a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
steps_per_weekend <- tapply(imputed_data$steps[imputed_data$daytype == "weekend"], 
                            imputed_data$interval[imputed_data$daytype == "weekend"], 
                            mean, na.rm=TRUE)
steps_per_weekday <- tapply(imputed_data$steps[imputed_data$daytype == "weekday"], 
                            imputed_data$interval[imputed_data$daytype == "weekday"], 
                            mean, na.rm=TRUE)
par(mfrow=c(2,1))
par(mar=c(4,4,3,2))
par(oma=c(3,3,3,3))
plot(names(steps_per_weekday), steps_per_weekday, type="l", main="Weekdays", 
     xlab="5-Minute Interval", ylab="Avg # of Steps")
plot(names(steps_per_weekend), steps_per_weekend, type="l", main="Weekends", 
     xlab="5-Minute Interval", ylab="Avg # of Steps")
mtext("Average Steps on Weekdays vs. Weekends", side=3, line=1, cex=2, outer=TRUE)  
box("outer", col="blue")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
