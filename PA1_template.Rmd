---
output: pdf_document
---
=============================================================================
Course Project 1
title: 'Reproducible Research: Peer Assessment 1'
=============================================================================


## Loading and preprocessing the data

```{r,echo=TRUE}
library(lattice)
setwd("D:/data learning/repdata-data-activity")
raw_data<-read.csv("activity.csv", stringsAsFactors=FALSE)
head(raw_data)
```

## What is mean total number of steps taken per day?

```{r,echo=TRUE}
totalSteps <- aggregate(raw_data$steps, by=list(raw_data$date), FUN=sum, na.rm=TRUE)
head(totalSteps)
names(totalSteps) <- c("Date","Total")
##Histogram of the total number of steps taken each day
hist(totalSteps$Total, main="Total Number of Steps Taken Each Day",xlab = "Steps",col= "red")
##Mean and median number of steps taken each day
summary(totalSteps)
mean(totalSteps$Total)
median(totalSteps$Total)
```

According to the summary, 

Mean is 9354.23

Median is 10395


## What is the average daily activity pattern?

```{r,echo=TRUE}
averageData <- aggregate(raw_data$steps,by=list(raw_data$interval),FUN=mean,na.rm=T)
head(averageData)
names(averageData)<-c("Interval","Average")
plot2 <- plot(x=averageData$Interval,y=averageData$Average, type="l",lwd=2,xlab = "5-minute interval", ylab = "Averaged across all days",main = "Average Daily Activity Pattern")
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps <- which.max(averageData$Average)
max_steps
averageData[104,,]
```

## Imputing missing values

```{r,echo=TRUE}
NA_Data <- sum(is.na(raw_data$steps))
NA_Data
NA_Value <- which(is.na(raw_data$steps))
##mean value
mean_value <- rep(mean(raw_data$steps,na.rm=T),times=length(NA_Value))
raw_data[NA_Value,"steps"] <- mean_value
head(raw_data)
totalSteps1 <- aggregate(raw_data$steps, by=list(raw_data$date), FUN=sum,na.rm=TRUE)
head(totalSteps1)
names(totalSteps1) <- c("Date","Total")
hist(totalSteps1$Total, main="Total Number of Steps Taken Each Day",xlab = "Steps",col= "blue")
summary(totalSteps1)
mean(totalSteps1$Total)
median(totalSteps1$Total)
```

According to the result, 

Mean is 10766.19.

Median is 10766.19.

## Are there differences in activity patterns between weekdays and weekends?

```{r,echo=TRUE}
##set weekday and weekend.
raw_data2<-data.frame(data=raw_data$date,weekday=weekdays(as.Date(totalSteps$Date)),steps=raw_data$steps,interval=raw_data$interval)
head(raw_data2)
raw_data3 <- cbind(raw_data2,daytype=ifelse(raw_data2$weekday == "Saturday" |raw_data2$weekday == "Sunday", "weekend", "weekday"))
head(raw_data3)
mean_data3 <- aggregate(raw_data3$steps,by=list(raw_data3$daytype,raw_data3$weekday, raw_data3$interval), mean)
names(mean_data3) <- c("daytype", "weekday", "interval", "mean")
head(mean_data3)
xyplot(mean ~ interval | daytype, mean_data3, type="l", lwd=1, xlab="Interval", ylab="Number of steps", layout=c(1,2))
```
