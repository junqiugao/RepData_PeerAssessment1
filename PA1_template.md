---
title: "Week2-Course Project"
author: "JUNQIU GAO"
date: "2017-03-13"
output: html_document
---
##Loading and preprocessing the data

Show any code that is needed to  
1. Load the data (i.e. read.csv())  
2. Process/transform the data (if necessary) into a format suitable for your analysis  

```{r}
setwd("C:/Users/john/Desktop/Coursera/Reproducible Research/Week 2")
activity <- read.csv("activity.csv")
head(activity)
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.  

1. Calculate the total number of steps taken per day  
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  
3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
##Calculate the total number of steps taken per day
totalperday <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)

##Make histogram of the total number of steps taken each day
barplot(totalperday, 
        main = "Histogram of the total number of steps taken each day", 
        xlab = "Day", ylab = "Total number of steps")

##Calculate mean and median number of steps taken each day
summary(totalperday)
```
![image](https://github.com/junqiugao/RepData_PeerAssessment1/blob/master/P1.png )

The mean and median numbers of steps taken each day are both equal **10770**

##What is mean total number of steps taken per day?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```{r}
stepsby5min <- aggregate(. ~ interval, data = activity, mean)
plot(stepsby5min$steps, type="l",main="Activity Pattern", xlab="5 Minute Interval", ylab="Time series plot of the average number of steps taken")
```
![image](https://github.com/junqiugao/RepData_PeerAssessment1/blob/master/P2.png)

  The maximum is calucated as followed.  


```{r}
max <- max(stepsby5min$steps)
maxinterval <- stepsby5min[stepsby5min$steps == max,]$interval
```
 
*Interval* `r maxinterval` is the interval with the most steps on average with `r max` *steps*.

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```{r}
na <- sum(is.na(activity))

```

The total number of missing values is `r na`.    


To replace these miss values, a function that loops through the data frame should be written down.  

```{r}
impute<-function(x){
        
        for(i in 1:length(x$steps)){
                if(is.na(x$steps[i])){
                        a <- mean(subset(x, x$date == x$date[i])$steps, na.rm=T )
                        x$steps[i] <- a
                        if(is.na(a)){
                                x$steps[i]<-0
                        }
                }
                
        }
        return (x)
}
activity1<-impute(activity)
```

Check whether the missing values are removed.  

```{r}
sum(is.na(activity1))
```

There is no missing value and the new data frame is **activity1**.  


To get the new histgram and the mean and median total number of steps taken per day:  

```{r}
##Calculate the total number of steps taken per day
totalperday1 <- tapply(activity1$steps, activity1$date, sum )

##Make histogram of the total number of steps taken each day
barplot(totalperday1, 
        main = "Histogram of the total number of steps taken each day", 
        xlab = "Day", ylab = "Total number of steps")

##Calculate mean and median number of steps taken each day
summary(totalperday1)
```

![image](https://github.com/junqiugao/RepData_PeerAssessment1/blob/master/P3.png)


The mean numbers of steps taken each day is **9354** and the median numbers of steps taken is **10400**  


##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.  

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

Separate *weekdays* and *weekends*  

```{r}
activity1$weekday <- weekdays(as.Date(activity1$date, '%Y-%m-%d'))
activity1$weekday1 <- ifelse (activity1$weekday == '星期六' | activity1$weekday == '星期日',
                            'Weekend', 
                            'Weekday') 

```

Make the panel plot:  

```{r}
par(mfrow=c(2,1))

stepsweekday <- activity1[activity1$weekday1 == 'Weekday',]
stepsintervalweekday <- aggregate(stepsweekday$steps ~ stepsweekday$interval,
                                      data = stepsweekday, sum)
plot(stepsintervalweekday, type = "l", main = "Weekday",
     ylab = "Steps", xlab = "", col = "blue")

stepsweekend <- activity1[activity1$weekday1 == 'Weekend',]
stepsintervalweekend <- aggregate(stepsweekend$steps ~ stepsweekend$interval,
                                  data = stepsweekend, sum)
plot(stepsintervalweekend, type = "l", main = "Weekend",
     ylab = "Steps", xlab = "", col = "red")
```
![image](https://github.com/junqiugao/RepData_PeerAssessment1/blob/master/P4.PNG)
