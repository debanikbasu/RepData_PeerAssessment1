# Reproducible Research: Peer Assessment 1


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

## Loading and preprocessing the data

Load the data (i.e. unzip and read.csv())

```r
unzip("activity.zip")
act_data <-read.csv("activity.csv", header=TRUE)
```

## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day


```r
steps_day <- act_data[!(is.na(act_data$steps)),]
steps_hist <- tapply(steps_day$steps,steps_day$date,sum)
hist(steps_hist,breaks=30,xlab="steps",ylab="frequency",main = "Histogram of steps per day")
```

![](PA1_template_files/figure-html/stepsday-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day

```r
mean_steps <- mean(steps_hist,na.rm = TRUE)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps <- median(steps_hist,na.rm = TRUE)
median_steps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Make a time series plot

```r
steps_int <- tapply(steps_day$steps,steps_day$interval,mean)
plot(names(steps_int),steps_int,type="l",xlab="interval",ylab="steps",main="Average steps per interval")
```

![](PA1_template_files/figure-html/activitypattern-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_int[steps_int==max(steps_int)]
```

```
##      835 
## 206.1698
```


## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
sum(is.na(act_data$steps))
```

```
## [1] 2304
```

Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
new_data <- act_data
for(i in 1:nrow(new_data)){
      if(is.na(new_data[i,1])){
            new_data[i,1] <- steps_int[names(steps_int)==new_data[i,3]]
      }
}
steps_new <- tapply(new_data$steps,new_data$date,sum)
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

```r
hist(steps_new,breaks=30,xlab="steps",ylab="frequency",main="Histogram of steps per day")
```

![](PA1_template_files/figure-html/newhist-1.png)<!-- -->

```r
mean_new <- mean(steps_new,na.rm = TRUE)
mean_new
```

```
## [1] 10766.19
```

```r
median_new <- median(steps_new,na.rm = TRUE)
median_new
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot.

```r
new_data$date <- as.Date(new_data$date)
new_data <- mutate(new_data, weektype = ifelse(weekdays(new_data$date) == "Saturday" | weekdays(new_data$date) == "Sunday", "weekend", "weekday"))

week_plot <- new_data %>% group_by(interval,weektype) %>% summarise(means=mean(steps))
g <- ggplot(week_plot, aes(interval,means)) + geom_line() + facet_grid(weektype~.)
g <- g + xlab("Intervals") + ylab("Average steps taken") + ggtitle("Average steps taken by weekdays and weekends")
g
```

![](PA1_template_files/figure-html/weekdaypattern-1.png)<!-- -->
