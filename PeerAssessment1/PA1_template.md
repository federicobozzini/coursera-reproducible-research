# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First we set the locale to english

```r
Sys.setlocale("LC_TIME", "C")
```

then we load and cache the data


```r
data <- read.csv("./activity.csv")
```

and finally we preprocess the data


```r
data$realDate <- as.Date(data[, 'date'], format='%Y-%m-%d')
pdata <- subset(data, !is.na(steps))
```

## What is mean total number of steps taken per day?

We plot an histogram of the total number of steps taken each day

```r
stepsPerDay <- aggregate(pdata$steps, by=list(date=pdata$date), FUN=sum)
hist(stepsPerDay$x, xlab="Steps", main="")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-4-1.png" title="" alt="" style="display: block; margin: auto;" />

We calculate the mean and the median 

```r
mean1 <- format(mean(stepsPerDay$x, na.rm=T), nsmall=2)
median1 <- format(median(stepsPerDay$x, na.rm=T), nsmall=2)
```

The mean number of steps taken per day is around 10766.19.

The median is 10765.


## What is the average daily activity pattern?

```r
stepsPerInterval <- aggregate(pdata$steps, by=list(interval=pdata$interval), FUN=mean)
plot(stepsPerInterval$interval, stepsPerInterval$x, type="l", xlab="Interval", ylab="Steps", main="")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-6-1.png" title="" alt="" style="display: block; margin: auto;" />


On average the interval with the maximum number of steps is the number 835.

## Imputing missing values


```r
isnas <- sum(is.na(data$steps))
```

There are 2304 missing values (where the steps values is NA).

We can replace every NA value with the equivalent interval average.

```r
data$allSteps <- ifelse(is.na(data$steps), stepsPerInterval[match(data$interval, stepsPerInterval$interval), 'x'], data$steps) 
```

We plot an histogram of the total number of steps taken each day

```r
allStepsPerDay <- aggregate(data$allSteps, by=list(date=data$date), FUN=sum)
hist(allStepsPerDay$x, xlab="Steps", main="")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />

We calculate the mean and the median 

```r
mean2 <- format(mean(allStepsPerDay$x, na.rm=T), nsmall=2)
median2 <- format(median(allStepsPerDay$x, na.rm=T), nsmall=2)
```

The mean number of steps taken per day is around 10766.19.

The median is 10766.19.

Replacing the missing values with the mean of the equivalent interval does not alter the mean and the median of steps taken per day.

## Are there differences in activity patterns between weekdays and weekends?

We split the data beased on the fact that the date is a weekday or a day of the weekend.

```r
data$weekday <- ifelse(weekdays(data$realDate, abbreviate=T) %in% c('Sat', 'Sun'), 'weekend', 'weekday')
weekdayData <- subset(data, weekday=='weekday')
weekendData <- subset(data, weekday=='weekend')
```
And we plot the data

```r
stepsPerWeekdayInterval <- aggregate(weekdayData$allSteps, by=list(interval=weekdayData$interval), FUN=mean)
stepsPerWeekendInterval <- aggregate(weekendData$allSteps, by=list(interval=weekendData$interval), FUN=mean)
par(mfcol=c(2,1))
plot(stepsPerWeekdayInterval$interval, stepsPerWeekdayInterval$x, type="l", xlab="Interval", ylab="Steps", main="Weekdays")
plot(stepsPerWeekendInterval$interval, stepsPerWeekendInterval$x, type="l", xlab="Interval", ylab="Steps", main="Weekends")
```

<img src="./PA1_template_files/figure-html/unnamed-chunk-12-1.png" title="" alt="" style="display: block; margin: auto;" />

It appears that during the weekend the steps count increases.

