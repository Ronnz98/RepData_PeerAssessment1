---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---

```r
library(ggplot2)
```

## Loading and preprocessing the data

load data

```r
myActivity <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(myActivity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
convert date variable from char to date type

```r
myActivity$date <- as.Date(myActivity$date)
str(myActivity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Check the data

```r
summary(myActivity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
## What is mean total number of steps taken per day?
1.What is mean total number of steps taken per day?

```r
myActivity_steps_per_day <- aggregate(steps ~ date, data = myActivity, FUN = sum, simplify = TRUE, na.rm = TRUE)
```
show histogram

```r
hist(myActivity_steps_per_day$steps, main = "Total number of steps taken per Day", xlab = "Total # Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
show mean

```r
mean
```

```
## function (x, ...) 
## UseMethod("mean")
## <bytecode: 0x00000000155f79d8>
## <environment: namespace:base>
```
show median

```r
median(myActivity_steps_per_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
2. What is the average daily activity pattern?

```r
myActivity_steps_mean <- aggregate(steps ~ interval, data = myActivity, FUN = mean, simplify = TRUE, na.rm = TRUE)
```
Plot result

```r
plot(myActivity_steps_mean$interval, myActivity_steps_mean$steps, type = "l", xlab = "5-Minutes-Intervals", ylab = "Average number of steps taken, averaged across all days ", main = "Average number of steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
myMaxNosteps <-max(myActivity_steps_mean$steps)
myMaxInterval <- myActivity_steps_mean$interval[which(myActivity_steps_mean$steps == myMaxNosteps)]
myMaxInterval
```

```
## [1] 835
```

```r
myMaxNosteps
```

```
## [1] 206.1698
```

## Imputing missing values
3. Imputing missing values
Total amount of missing data

```r
sum(is.na(myActivity))
```

```
## [1] 2304
```
Create a new dataset from the original datasets with no missing data and using mean of the 5-minute interval

```r
myActivityNA<- subset(myActivity, !is.na(myActivity$steps))
myActivityNew <- myActivity
myNdx <- is.na(myActivityNew$steps)
myAvg <- tapply(myActivityNA$steps, myActivityNA$interval, mean, na.rm=TRUE, simplify=T)
myActivityNew$steps[myNdx] <- myAvg[as.character(myActivityNew$interval[myNdx])]
```
Total amount of missing data now (should be 0)

```r
sum(is.na(myActivityNew))
```

```
## [1] 0
```
show histogram with imputed missing values

```r
myActivityStepsNew <- aggregate(steps ~ date, data = myActivityNew, FUN = sum, simplify = TRUE, na.rm = TRUE)
hist(myActivityStepsNew$steps, main = "Total number of steps taken per Day (missing values replaced)", xlab = "Total # Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
show mean with imputed missing values

```r
mean(myActivityStepsNew$steps)
```

```
## [1] 10766.19
```
show median with imputed missing values

```r
median(myActivityStepsNew$steps)
```

```
## [1] 10766.19
```
-> The impact of imputed missing data very low
## Are there differences in activity patterns between weekdays and weekends?
4. Are there differences in activity patterns between weekdays and weekends?

Create a factor variable in the dataset with 2 levels: “weekday” and “weekend”

```r
myActivityNew$dayType <- ifelse(weekdays(as.Date(myActivityNew$date)) == "Samstag" | weekdays(as.Date(myActivityNew$date)) == "Sonntag", "weekend", "weekday")
myActivityNew$dayType <- factor(myActivityNew$dayType)
```
Aggregate a table showing mean steps for all intervals, acrlss week days and weekend days

```r
myStepIntervalDayType <- aggregate(steps ~ interval + dayType, data = myActivityNew, FUN = mean, simplify = TRUE, na.rm = TRUE)
```
add descriptive variables and plot

```r
names(myStepIntervalDayType) <- c("interval", "day_type", "mean_steps")
plot <- ggplot(myStepIntervalDayType, aes(interval, mean_steps))
plot + geom_line(color = "tan1") + facet_grid(day_type~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns on Weekends/-days")
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
