# Name: Jun Hui Ho
## Date: 13 July 2015

This data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

```r
# load data into workspace
activity <- read.csv("activity.csv")

# process data
activity$date <- as.Date(activity$date)
activity$date <- julian(activity$date, origin = as.Date("2012-09-30"))

# calculate the total number of steps taken per day, ignoring NAs
total_steps <- vector("numeric", length = max(activity$date))
for (i in 1:length(total_steps)) {
  total_steps[i] <- sum(activity$steps[activity$date == i], na.rm = TRUE)
}

# Total number of steps taken per day
total_steps
```

```
##  [1]     0   126 11352 12116 13294 15420 11015     0 12811  9900 10304
## [12] 17382 12426 15098 10139 15084 13452 10056 11829 10395  8821 13460
## [23]  8918  8355  2492  6778 10119 11458  5018  9819 15414     0 10600
## [34] 10571     0 10439  8334 12883  3219     0     0 12608 10765  7336
## [45]     0    41  5441 14339 15110  8841  4472 12787 20427 21194 14478
## [56] 11834 11162 13646 10183  7047     0
```

```r
# generate a histogram for the total number of steps taken each day
hist(total_steps, main = "Histogram of Total Number of Steps Each Day, Ignoring NAs")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
# calculate the mean and median of the total number of steps taken per day
mean_stepsperday <- vector("numeric", length = max(activity$date))
median_stepsperday <- vector("numeric", length = max(activity$date))
for (i in 1:max(activity$date)) {
  mean_stepsperday[i] <- mean(activity$steps[activity$date == i], na.rm = TRUE)
  median_stepsperday[i] <- median(activity$steps[activity$date == i], na.rm = TRUE)
}

# Mean of the total number of steps taken per day
mean_stepsperday
```

```
##  [1]        NaN  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667
##  [7] 38.2465278        NaN 44.4826389 34.3750000 35.7777778 60.3541667
## [13] 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667
## [19] 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167
## [25]  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500
## [31] 53.5208333        NaN 36.8055556 36.7048611        NaN 36.2465278
## [37] 28.9375000 44.7326389 11.1770833        NaN        NaN 43.7777778
## [43] 37.3784722 25.4722222        NaN  0.1423611 18.8923611 49.7881944
## [49] 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778
## [55] 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
## [61]        NaN
```

```r
# Median of the total number of steps taken per day
median_stepsperday
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [24]  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0
## [47]  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA
```

```r
# generate a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
mean_stepsperinterval <- vector("numeric", length = 24*12)
j <- 1
for (i in unique(activity$interval)) {
  mean_stepsperinterval[j] <- mean(activity$steps[activity$interval == i], na.rm = TRUE)
  j = j + 1
}
plot(mean_stepsperinterval, type="l", main = "Time Series Plot - Average Number of Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) 

```r
# 5-minute interval which contains the maximum average number of steps across all the days
unique(activity$interval[mean_stepsperinterval==max(mean_stepsperinterval)])
```

```
## [1] 835
```

```r
# Total number of NAs in the dataset
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# create new identical dataset
activity2 <- activity

# replace NAs with the mean for that 5-minute interval
for (i in 1:nrow(activity2)) {
  if (is.na(activity2$steps[i])) {
    if(i%%288 == 0) {
      activity2$steps[i] <- mean_stepsperinterval[288]
    }  
    else {
      activity2$steps[i] <- mean_stepsperinterval[i%%288]
    }
  }
}

# calculate the total number of steps taken per day, after replacing NAs
total_steps2 <- vector("numeric", length = max(activity2$date))
for (i in 1:max(activity2$date)) {
  total_steps2[i] <- sum(activity2$steps[activity2$date == i], na.rm = TRUE)
}

# Total number of steps taken per day, after replacing NAs
total_steps2
```

```
##  [1] 10766.19   126.00 11352.00 12116.00 13294.00 15420.00 11015.00
##  [8] 10766.19 12811.00  9900.00 10304.00 17382.00 12426.00 15098.00
## [15] 10139.00 15084.00 13452.00 10056.00 11829.00 10395.00  8821.00
## [22] 13460.00  8918.00  8355.00  2492.00  6778.00 10119.00 11458.00
## [29]  5018.00  9819.00 15414.00 10766.19 10600.00 10571.00 10766.19
## [36] 10439.00  8334.00 12883.00  3219.00 10766.19 10766.19 12608.00
## [43] 10765.00  7336.00 10766.19    41.00  5441.00 14339.00 15110.00
## [50]  8841.00  4472.00 12787.00 20427.00 21194.00 14478.00 11834.00
## [57] 11162.00 13646.00 10183.00  7047.00 10766.19
```

```r
# generate a histogram for the total number of steps taken each day, after replacing NAs
hist(total_steps2, main = "Histogram of Total Number of Steps Each Day, after Replacing NAs")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png) 

```r
# calculate the mean and median of the total number of steps taken per day, after replacing NAs
mean_stepsperday2 <- vector("numeric", length = max(activity2$date))
median_stepsperday2 <- vector("numeric", length = max(activity2$date))
for (i in 1:max(activity2$date)) {
  mean_stepsperday2[i] <- mean(activity2$steps[activity2$date == i], na.rm = TRUE)
  median_stepsperday2[i] <- median(activity2$steps[activity2$date == i], na.rm = TRUE)
}

# Mean of the total number of steps taken per day, after replacing NAs
mean_stepsperday2
```

```
##  [1] 37.3825996  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667
##  [7] 38.2465278 37.3825996 44.4826389 34.3750000 35.7777778 60.3541667
## [13] 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667
## [19] 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167
## [25]  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500
## [31] 53.5208333 37.3825996 36.8055556 36.7048611 37.3825996 36.2465278
## [37] 28.9375000 44.7326389 11.1770833 37.3825996 37.3825996 43.7777778
## [43] 37.3784722 25.4722222 37.3825996  0.1423611 18.8923611 49.7881944
## [49] 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778
## [55] 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
## [61] 37.3825996
```

```r
# Median of the total number of steps taken per day, after replacing NAs
median_stepsperday2
```

```
##  [1] 34.11321  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
##  [8] 34.11321  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [15]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [22]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [29]  0.00000  0.00000  0.00000 34.11321  0.00000  0.00000 34.11321
## [36]  0.00000  0.00000  0.00000  0.00000 34.11321 34.11321  0.00000
## [43]  0.00000  0.00000 34.11321  0.00000  0.00000  0.00000  0.00000
## [50]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [57]  0.00000  0.00000  0.00000  0.00000 34.11321
```

```r
# create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
activity2$date <- as.Date(activity2$date, origin = as.Date("2012-09-30"))
activity2$day <- factor(weekdays(activity2$date) == "Saturday" | weekdays(activity2$date) == "Sunday")
levels(activity2$day)[levels(activity2$day) == TRUE] <- "weekend"
levels(activity2$day)[levels(activity2$day) == FALSE] <- "weekday"

# generate a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays and weekend days separately (y-axis)
mean_stepsperintervalweekend <- vector("numeric", length = 24*12)
mean_stepsperintervalweekday <- vector("numeric", length = 24*12)
j <- 1
for (i in unique(activity2$interval)) {
  mean_stepsperintervalweekend[j] <- mean(activity2$steps[activity2$interval == i & activity2$day == "weekend"])
  mean_stepsperintervalweekday[j] <- mean(activity2$steps[activity2$interval == i & activity2$day == "weekday"])
  j = j + 1
}
plot(mean_stepsperintervalweekend, type="l", main = "Time Series Plot - Average Number of Steps (Weekend)")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png) 

```r
plot(mean_stepsperintervalweekday, type="l", main = "Time Series Plot - Average Number of Steps (Weekday)")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png) 
