# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("c:/Users/Ola/Desktop/Coursera/Reproductible_research/Project1")
raw<-read.csv("activity.csv", header=T, na.strings="NA")
raw$Date <- as.Date(raw$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
#Reading the dplyr and lattice package 
library(dplyr)
library(lattice)
```


```r
#and converting the dataset to dplyr dataframe.
data<-tbl_df(raw)

#Calculating the table summarizing the data by day
groupByDay<-group_by(data,Date)
summaryByDate<-summarize(groupByDay,StepsPerDay=sum(steps))

#Plotting the histogram
with(summaryByDate, hist(StepsPerDay, main="Histogram of steps taken per day",
                         col="blue", xlab="Steps taken per day"))
```

![](PA1_template_files/figure-html/convertingData-1.png) 

```r
#Calculating the mean and the median
meanNumberOfSteps<-round(mean(summaryByDate$StepsPerDay, na.rm=T),2)
medianNumberOfSteps<-round(median(summaryByDate$StepsPerDay, na.rm=T),2)
```

**Mean** number of steps taken per day is 1.076619\times 10^{4}. The **median** is almost the same and equals 1.0765\times 10^{4}.





## What is the average daily activity pattern?

```r
#Calculating the table summarizing the data by day
dataWoNa<-filter(data, is.na(steps)==FALSE)
groupByInterval<-group_by(dataWoNa,interval)
summaryByInterval<-summarize(groupByInterval,MeanIntervalByDay=mean(steps))
```

```r
with(summaryByInterval, plot(interval, MeanIntervalByDay, type="l", main="Average daily activity pattern", xlab="Intervals", ylab="Average steps per day"))

maxNumberOfSteps<-max(summaryByInterval$MeanIntervalByDay)
intervalWithMaxSteps<-as.integer(filter(summaryByInterval,MeanIntervalByDay==maxNumberOfSteps)[1])
maxAvgSteps<-as.integer(filter(summaryByInterval,MeanIntervalByDay==maxNumberOfSteps)[2])

abline(v=intervalWithMaxSteps, col="blue", lty=3)
abline(h=maxAvgSteps, col="blue", lty=3)
text(1200,180, paste("Max=" , maxAvgSteps), col="blue")
```

![](PA1_template_files/figure-html/plottingQuestion2-1.png) 

The interval which on average across all the days in the dataset, contains the maximum number of step is 835.




## Imputing missing values

```r
dataNa<-filter(data, is.na(steps)==TRUE)
naNumber<-as.integer(summarize(dataNa,  count = n())[1])
```
Number of missing values equals 2304.

```r
#To input missing values a mean from the given interval was used.

groupByInterval<-group_by(data,interval)
summaryByInterval<-summarize(groupByInterval,intervalAverage=mean(steps,na.rm=T))
dataAll <- merge(data, summaryByInterval, by.x="interval", by.y="interval")
dataAll<-mutate(dataAll, stepsInputted=ifelse(is.na(steps)==T,intervalAverage,steps))
```

```r
#converting the dataset to dplyr dataframe.
dataAll<-tbl_df(dataAll)

#Calculating the table summarizing the data by day
groupByDay<-group_by(dataAll,Date)
summaryByDate<-summarize(groupByDay,inputtedStepsPerDay=sum(stepsInputted))

#Plotting the histogram
with(summaryByDate, hist(inputtedStepsPerDay, main="Histogram of steps taken per day",
                         col="blue", xlab="Steps taken per day"))
```

![](PA1_template_files/figure-html/summarizingDataQuestion3-1.png) 


```r
#Calculating the mean and the median
meanInputtedSteps<-round(mean(summaryByDate$inputtedStepsPerDay),2)
medianInputtedSteps<-round(median(summaryByDate$inputtedStepsPerDay),2)
```
**Mean** number of steps taken per day is 1.076619\times 10^{4}. The **median** is the same and equals 1.076619\times 10^{4}.


```r
rm(groupByDay, groupByInterval, summaryByDate, summaryByInterval)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_ALL","C")
```

```
## [1] "C"
```

```r
dataAll$Weekdays<-weekdays(dataAll$Date)
dataAll<-mutate(dataAll, Weekends=ifelse(Weekdays=="Saturday"|Weekdays=="Sunday","weekend","weekday"))

#summarizing data for the chart
groupedByInterval<-group_by(dataAll, interval, Weekends)
summaryByInterval<-summarize(groupedByInterval, averageInputtedSteps=mean(stepsInputted))
```

```r
chart<-xyplot(averageInputtedSteps ~ interval | Weekends, data=summaryByInterval, 
              layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps",
                  )
print(chart) 
```

![](PA1_template_files/figure-html/chartQuestion4-1.png) 
