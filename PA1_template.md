---
title: "Analyzing activity monitor data"
author: "Zografoula Vagena"
date: "December 17, 2015"
output: html_document
---

In the current report we describe the steps we took to load and analyze activity monitor data. The data comes from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

* _steps_ : Number of steps taking in a 5-minute interval (missing values are coded as NA)
* _date_ : The date on which the measurement was taken in YYYY-MM-DD format
* _interval_: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

The first step we took was to load the dataset from the downloaded file into R:


```r
dt1 <- read.csv("C:/Users/Foula/Desktop/Coursera/ReproducibleResearch/activity.csv")
dt2 <- subset(dt1, steps = as.numeric(dt1$steps))
dt <- subset(dt2, date = as.Date(dt2$date))
summary(dt)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

Subsequently, ignoring the missing values in the dataset, we calculate the total number of steps per days and make a histogram of that, as follows:


```r
agg <- aggregate(dt$steps, by=list(Day=dt$date), FUN=sum, na.rm=TRUE)
hist(agg$x, xlab = "Number of Steps", main = "Histogram of the total number of steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

We then calculate and report the mean and median of the total number of steps per day.


```r
mean_agg <- mean(agg$x)
print(mean_agg)
```

```
## [1] 9354.23
```

```r
median_agg <- median(agg$x)
print(median_agg)
```

```
## [1] 10395
```

Again ignoring the mising values in the dataset, we calculate the number of steps per 5-minute interval, averaged across all days and we create a timeseries plot of the values


```r
agg_int <- aggregate(dt$steps, by = list(interval=dt$interval), FUN=mean, na.rm=TRUE)
plot(agg_int, type = "l", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Using the information above we caclulate the 5-minute interval that has the max number of steps:


```r
index <- which(agg_int$x == max(agg_int$x))
print(agg_int$interval[index])
```

```
## [1] 835
```

### Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The total number of missing values in the dataset is computed as follows:


```r
sum(is.na(dt$steps))
```

```
## [1] 2304
```

We decided to fill in the missing values by using the mean (rounded to the closest integer that is larger or equal to that mean) for each 5-minute interval. With this strategy we create a new dataset  that is equal to the original dataset but with the missing data filled in as follows:


```r
agg_int$xRounded <- ceiling(agg_int$x)
new_dt <- merge(dt, agg_int, by = "interval")
new_dt_index <- which(is.na(new_dt$steps))
new_dt$steps[new_dt_index] <- new_dt$xRounded[new_dt_index]
imputed_dt <- data.frame(steps = new_dt$steps, date = new_dt$date, interval = new_dt$interval)
```

Using this new dataset we make a histogram of the total number of steps taken each day:


```r
imputed_agg <- aggregate(imputed_dt$steps, by = list(Day = imputed_dt$date), FUN=sum, na.rm=TRUE)
hist(imputed_agg$x, xlab = "Number of Steps", main = "Histogram of the total number of steps per day over the imputed dataset")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

and then we calculate and report the mean and median total number of steps taken per day:


```r
mean_imputed_agg <- mean(imputed_agg$x)
print(mean_imputed_agg)
```

```
## [1] 10784.92
```

```r
median_imputed_agg <- median(imputed_agg$x)
print(median_imputed_agg)
```

```
## [1] 10909
```

It seems that imputing the data with our strategy results in a larger mean and median values in the dataset.

Finally, to check whether there are differences in activity patterns between weekdays and weekends, we create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:


```r
imputed_dt$day_of_week <- ifelse(as.POSIXlt(imputed_dt$date)$wday > 2,"weekday", "weekend")
```

and using this new dataset we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) as follows:


```r
d1 <- subset(imputed_dt, imputed_dt$day_of_week == "weekend")
agg_d1 <- aggregate(d1$steps, by = list(interval = d1$interval), FUN = mean, na.rm=TRUE)

d2 <- subset(imputed_dt, imputed_dt$day_of_week == "weekday")
agg_d2 <- aggregate(d2$steps, by = list(interval = d2$interval), FUN = mean, na.rm=TRUE)

agg_d <- rbind(d1, d2)

library (lattice)
xyplot (steps ~ interval | day_of_week, agg_d, type="l", xlab = "Interval", 
        ylab = "Number of steps", layout=c(1,3))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
