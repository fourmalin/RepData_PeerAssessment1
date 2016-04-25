# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

setwd("D:/data learning/repdata-data-activity")

raw_data<-read.csv("activity.csv", stringsAsFactors=FALSE)


raw_data$date<-as.Date(raw_data$date,"%y-%m-%d")

head(raw_data)

##  steps       date interval
##1    NA 2012-10-01        0
##2    NA 2012-10-01        5
##3    NA 2012-10-01       10
##4    NA 2012-10-01       15
##5    NA 2012-10-01       20
##6    NA 2012-10-01       25


## What is mean total number of steps taken per day?
totalSteps <- aggregate(raw_data$steps, by=list(raw_data$date), FUN=sum, na.rm=TRUE)

head(totalSteps)

##     Group.1     x
##1 2012-10-01     0
##2 2012-10-02   126
##3 2012-10-03 11352
##4 2012-10-04 12116
##5 2012-10-05 13294
##6 2012-10-06 15420

names(totalSteps) <- c("Date","Total")

plot1 <- plot(x=totalSteps$Date,y=totalSteps$Total, type="h",lwd=10,lend=2,xlab = "Date", ylab = "Total Stpes",main = "Total number of steps taken per day")

mean(totalSteps$Total)

##9354.23

median(totalSteps$Total)

##10395


## What is the average daily activity pattern?

averageData <- aggregate(raw_data$steps,by=list(raw_data$interval),FUN=mean,na.rm=T)

head(averageData)

##Group.1         x
##1       0 1.7169811
##2       5 0.3396226
##3      10 0.1320755
##4      15 0.1509434
##5      20 0.0754717
##6      25 2.0943396

names(averageData)<-c("Interval","Average")

plot2 <- plot(x=averageData$Interval,y=averageData$Average, type="l",lwd=2,xlab = "Minutes", ylab = "Average Stpes",main = "Average daily activity")

max_steps <- which.max(averageData$Average)

max_steps

#104

averageData[104,,]

##Interval  Average
##104      835 206.1698
##835


## Imputing missing values

NA_Data <- sum(is.na(raw_data$steps))

NA_Data

##2304

NA_Value <- which(is.na(raw_data$steps))

mean_value <- rep(mean(raw_data$steps,na.rm=T),times=length(NA_Value))

raw_data[NA_Value,"steps"] <- mean_value

head(raw_data)

##steps       date interval
##1 37.3826 2012-10-01        0
##2 37.3826 2012-10-01        5
##3 37.3826 2012-10-01       10
##4 37.3826 2012-10-01       15
##5 37.3826 2012-10-01       20
##6 37.3826 2012-10-01       25

totalSteps1 <- aggregate(raw_data$steps, by=list(raw_data$date), FUN=sum,na.rm=TRUE)

head(totalSteps1)

##Group.1        x
##1 2012-10-01 10766.19
##2 2012-10-02   126.00
##3 2012-10-03 11352.00
##4 2012-10-04 12116.00
##5 2012-10-05 13294.00
##6 2012-10-06 15420.00

names(totalSteps1) <- c("Date","Total")

plot11 <- plot(x=totalSteps1$Date,y=totalSteps1$Total, type="h",lwd=10,lend=2,xlab = "Date", ylab = "Total Stpes New",main = "Total number of steps taken per day New")

mean(totalSteps1$Total)

##10766.19

median(totalSteps1$Total)

##10766.19

## Are there differences in activity patterns between weekdays and weekends?

raw_data<-data.frame(data=raw_data$date,weekday=tolower(weekdays(raw_data$date)),steps=raw_data$steps,interval=raw_data$interval)

raw_data <- cbind(raw_data,daytype=ifelse(raw_data$weekday == "saturday" |raw_data$weekday == "sunday", "weekend", "weekday"))

raw_data <-data.frame(date=raw_data$date,weekday=raw_data$weekday,daytype=raw_data$daytype,interval=raw_data$interval,steps=raw_data$steps)

head(raw_data)

##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826

mean_data <- aggregate(raw_data$steps,by=list(raw_data$daytype,raw_data$weekday, raw_data$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")

head(mean_data)

##   daytype  weekday interval     mean
## 1 weekday   friday        0 8.307244
## 2 weekday   monday        0 9.418355
## 3 weekend saturday        0 4.672825
## 4 weekend   sunday        0 4.672825
## 5 weekday thursday        0 9.375844
## 6 weekday  tuesday        0 0.000000

xyplot(mean ~ interval | daytype, mean_data, type="l", lwd=1, xlab="Interval", ylab="Number of steps", layout=c(1,2))
