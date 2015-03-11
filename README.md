<<<<<<< HEAD
# Reproducible Research: Peer Assessment 1

## Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

```{r setup_options, echo=FALSE}
library(knitr, markdown)
setwd("C:/Tahera/Personal/Coursera/Reproducible_Research/PA1")
opts_chunk$set(comment=NA, fig.width=6, fig.height=6, echo=TRUE, fig.path='figures/PA-', fig.show='asis')
```

### 1. Loading and preprocessing the data

```{r}
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "pa1.zip", mode="wb")
dateDownloaded <- date()
unzip("pa1.zip", files = NULL, list = FALSE, overwrite = TRUE,
              junkpaths = FALSE, exdir = ".", unzip = "internal",
              setTimes = FALSE)
data <- read.csv('activity.csv', header = TRUE)
str(data)

## convert date to date data type
data$date <- as.Date(data$date, "%Y-%m-%d") 

```
### 2. What is mean total number of steps taken per day?
```{r}

TotalSteps <- aggregate(steps ~ date, data = data, sum)
hist(TotalSteps$steps, , main = "Total Number of Steps Per Day", xlab = "day", col = "blue")
MeanSteps <- mean(TotalSteps$steps)
MedianSteps <- median(TotalSteps$steps)
print(sprintf("Mean total steps taken per day: %f ", MeanSteps))
print(sprintf("Median total steps taken per day: %f ", MedianSteps))

```
![alt tag](RepData_PeerAssessment1/figures/PA-unnamed-chunk-2-1.png)
### 3. What is the average daily activity pattern?
```{r}
library(dplyr)
# Removes date factors corresponding to NA values
adata <- na.omit(data)
adata$date <- as.factor(as.character(adata$date))

AvgStepsTime <- adata %>% group_by(interval) %>% summarize(avgsteps = mean(steps), na.rm = TRUE)

plot(AvgStepsTime$interval, AvgStepsTime$avgsteps, type = "l",
     xlab = "Time Interval (minutes)", 
     ylab = "Average Number of Steps", 
     main = "Average Daily Activity Pattern") 

MaxAvgInterval <- AvgStepsTime$interval[AvgStepsTime$avgsteps == max(AvgStepsTime$avgsteps)]

print(sprintf("The 5-min time interval with maximum average steps taken per day: %i ", 
              MaxAvgInterval))
```
### 4. Imputing missing values
```{r}
MissingData <- sum(is.na(data))
print(sprintf("The total number of missing values in the dataset: %i ", MissingData))

## Strategy for filling in all of the missing values in the dataset : mean for that 5-minute interval
## will be used to replace missing values

print ("Strategy to replace misiing values: mean for the corresponding 5-minute interval will be used to replace missing values")

newdata <- data

sapply(unique(data$interval), 
       function(x) newdata[!complete.cases(newdata) & newdata$interval ==x, 1]
       <<- AvgStepsTime$avgsteps[AvgStepsTime$interval == x])

NoMissingData <- newdata %>% group_by(date) %>% summarize(TotalSteps = sum(steps, na.rm = TRUE))

hist(NoMissingData$TotalSteps, xlab = "Total Steps Per Day", ylab = "Number of Days", main = "Frequency of Total Steps in a Day", col = "red")

NoMissingMeanSteps <- mean(TotalSteps$steps)
NoMissingMedianSteps <- median(TotalSteps$steps)
print(sprintf("Mean total steps taken per day (missing values replaced): %f ", NoMissingMeanSteps))
print(sprintf("Mean total steps taken per day (missing values not replaced): %f ", MeanSteps))

print(sprintf("Median total steps taken per day (missing values replaced): %f ", NoMissingMedianSteps))
print(sprintf("Median total steps taken per day (missing values not replaced): %f ", MedianSteps))

print("Mean and median of total steps taken per day are same before and after replacing missing values ")

```
### 4. Are there differences in activity patterns between weekdays and weekends?
```{r}
NewData <- newdata %>% mutate(day = ifelse(as.factor(weekdays(newdata$date)) == c("Saturday", "Sunday"), "weekend", "weekday")) %>% group_by (interval, day) %>% summarize(AvgSteps = mean(steps))

library(ggplot2)
qplot(x = interval, y = AvgSteps, data = NewData, geom = c("line"), facets = day~., 
      xlab= "Time Interval (minutes)", ylab = "Average Number of Steps", main = "Activity Patterns Between Weekdays and Weekends")

```
```{r, include=FALSE}
   file.copy(from="PA1_template.Rmd", to="README.md", copy.mode=TRUE)
```


