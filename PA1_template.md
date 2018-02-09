---
title: 'Peer-graded Assignment: Course Project 1'
author: "MD Kinney"
date: "February 8, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
2. date: The date on which the measurement was taken in YYYY-MM-DD format
3. interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Question 1
Code for reading in the dataset and/or processing the data

```{r}
setwd("U:/EA/Course 5/Week 2")
if(!file.exists('activity.csv')){
  unzip('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip')}
activityFile <- read.csv('activity.csv')
str(activityFile)
# 'data.frame':	17568 obs. of  3 variables:
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Question 2
Histogram of the total number of steps taken each day
```{r}
# Create the sums of steps per date
Activitysteps <- tapply(activityFile$steps, activityFile$date, FUN=sum, na.rm=TRUE)

# Perform histogram of steps per day
library(ggplot2)
qplot(Activitysteps, binwidth=1000, xlab="total number of steps taken each day")
```


Question 3
Mean and median number of steps taken each day
```{r}
# Create mean and median of steps per day
stepsMean <- mean(Activitysteps, na.rm=TRUE)
stepsMedian <- median(Activitysteps, na.rm=TRUE)

# Output mean and median
stepsMean
## [1] 9354.23
stepsMedian
## [1] 10395

```
Question 4
Time series plot of the average number of steps taken
```{r}
# Create the means by intervals
averages <- aggregate(x=list(steps=activityFile$steps), by=list(interval=activityFile$interval),FUN=mean, na.rm=TRUE)

ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  ggtitle("Time Series: average number of steps") +
  xlab("5-minute interval") +
  ylab("average number of steps taken")


```
Question 5
The 5-minute interval that, on average, contains the maximum number of steps
```{r}
averages[which.max(averages$steps),]

##     interval    steps
## 104      835 206.1698

```
Question 6
Code to describe and show a strategy for imputing missing data
Idea: Replace the NA by the mean of the corresponding interval.
```{r}
# copy of data frame
ActivityFile2 <- activityFile

# add column for copleating index
ActivityFile2$CI <- "original"

# number of rows to check
l <- nrow(ActivityFile2)

# numbers of NAs
length(which(is.na(ActivityFile2$steps)))
## [1] 2304

# replace NAs by corresponing mean of the same interval --> complete data frame ActivityFile2
for (i in 1:l) {
  if (is.na(ActivityFile2[i,1])) {
    ActivityFile2[i,1] <- averages[averages$interval == ActivityFile2[i,3],2]
    ActivityFile2[i,4] <- "completed"
  }
}

# numbers of NAs / completed (control)
length(which(is.na(ActivityFile2$steps)))
## [1] 0

length(which(ActivityFile2$CI=="completed"))
## [1] 2304

# Recreate the sums of steps per date
ActivityFilesteps2 <- tapply(ActivityFile2$steps, ActivityFile2$date, FUN=sum, na.rm=TRUE )

# Recreate the mean and median of steps per date
stepsMean2 <- mean(ActivityFilesteps2)
stepsMedian2 <- median(ActivityFilesteps2)

c(stepsMean2, stepsMean)
## [1] 10766.19  9354.23
c(stepsMedian2, stepsMedian)
## [1] 10766.19 10395.00

```
Question 7
Histogram of the total number of steps taken each day after missing values are imputed


```{r}
# Preparation environment
library(ggplot2)
library(gridExtra)
require(gridExtra)

# Perform histogram of steps per day
plot1 <- qplot(Activitysteps, 
               binwidth=1000, 
               ylim=c(0,15),
               main="original", 
               xlab="total number of steps taken each day")

plot2 <- qplot(ActivityFilesteps2, 
               binwidth=1000, 
               ylim=c(0,15),
               main="completed", 
               xlab="total number of steps taken each day")

# Plotting 2 plot in grid
grid.arrange(plot1, plot2, ncol=2)

```
Question 8
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
# Create a new factor variable in the dataset with two levels - 
# weekday and weekend 
library(lubridate)

ActivityFile2$weekend <- wday(ActivityFile2$date, label = T, abbr = F)
levels(ActivityFile2$weekend) <- list(
  weekend = c("Saturday","Sunday"),
  weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday")
  )

# Calculate the average number of steps for each interval 
# (across all days, factored by "weekday" and by "weekend")

stepsByIntervalAndDayType <- aggregate(steps ~ interval + weekend, data = ActivityFile2, FUN = mean)

# Make a time series panel plot comparing weekdays to weekends

ggplot(data = stepsByIntervalAndDayType, aes(y = steps, x = interval)) +
  geom_line(aes(color = weekend), show.legend = F) +
  facet_grid(weekend ~ .) +
  ggtitle("Average Number of Steps per Interval\nWeekend vs Weekday") +
  xlab("Interval ID") +
  ylab("Number of Steps")


```
Question 9
All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

The underlying R Markdown document contains all of the R code needed to reproduce the report.
