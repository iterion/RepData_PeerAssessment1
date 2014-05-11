# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv("~/Development/RepData_PeerAssessment1/activity.csv")
activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?


```r
stepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm = T)
hist(stepsPerDay)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

mean(stepsPerDay)
```

```
## [1] 9354
```

```r

median(stepsPerDay)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
stepsPerInterval <- aggregate(activity$steps, list(activity$interval), mean, 
    na.rm = T)
plot(stepsPerInterval[, 1], stepsPerInterval[, 2], type = "l", col = "blue", 
    xlab = "Interval", ylab = "Number of steps (mean)")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

sortedSteps <- stepsPerInterval[order(stepsPerInterval[, 2], decreasing = T), 
    ]
```

Interval with most steps:

```r
sortedSteps[1, 1]
```

```
## [1] 835
```


## Imputing missing values


```r
# missing rows
missingRows <- is.na(activity$steps)
sum(missingRows)
```

```
## [1] 2304
```

```r

activity2 <- activity
intervalForMissingRows <- activity2$interval[which(missingRows)]
activity2$steps[which(missingRows)] <- as.vector(stepsPerInterval[as.character(intervalForMissingRows)])
```

```
## Error: undefined columns selected
```

```r

stepsPerDay2 <- tapply(activity2$steps, activity2$date, sum, na.rm = T)
hist(stepsPerDay2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r

mean(stepsPerDay2)
```

```
## [1] 9354
```

```r

median(stepsPerDay2)
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?


```r
weekdays <- weekdays(activity2$date)
activity2$is.weekend <- weekdays == "Saturday" | weekdays == "Sunday"

# subset data
weekendActivities <- activity2[activity2$is.weekend, ]
weekdayActivities <- activity2[!activity2$is.weekend, ]

# mean across intervals
stepsPerIntervalWeekEnd <- tapply(weekendActivities$steps, weekendActivities$interval, 
    mean, na.rm = T)
stepsPerIntervalWeekDay <- tapply(weekdayActivities$steps, weekdayActivities$interval, 
    mean, na.rm = T)

par(mfrow = c(2, 1))
plot(names(stepsPerIntervalWeekEnd), stepsPerIntervalWeekEnd, type = "l", col = "blue", 
    xlab = "Interval", ylab = "Number of steps (mean)", main = "Weekends")
plot(names(stepsPerIntervalWeekDay), stepsPerIntervalWeekDay, type = "l", col = "blue", 
    xlab = "Interval", ylab = "Number of steps (mean)", main = "Weekdays")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

