Reproducible Research: Peer Assessment 1
========================================

Author: David Fernandez Jimenez

Loading and preprocessing the data
==================================

Here is the code I have used to load the data

    setwd("/Users/DavidFernandez/ExpR-CP1")

    dir()

    activity<-read.csv("/Users/DavidFernandez/ExpR-CP1/activity.csv")

What is mean total number of steps taken per day?
=================================================

For this part of the assignment, the missing values in the dataset are
ignored

First of all, I calculate the total number of steps taken per day.

    StepData <- with(activity,aggregate(steps,by=list(date),sum))

    head(StepData)

    ##      Group.1     x
    ## 1 2012-10-01    NA
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

Once I have the total number of steps taken per day, I plot the
histogram.

      hist(StepData$x,main = "Number of steps in a day",xlab = "Steps",col = "green") 

![](PA_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Finally I calculate the mean and median of the total number of steps
taken per day.

      StepDataMean <- mean(StepData$x,na.rm = TRUE)

      StepDataMedian <- median(StepData$x,na.rm = TRUE)
      
      print(StepDataMean)

    ## [1] 10766.19

      print(StepDataMedian)

    ## [1] 10765

What is the average daily activity pattern?
===========================================

In this part of the assignment it is necessary to consider the missing
values.

I will start calculating the total number of missing values in the
dataset

     Missing_values_Count<-sum(is.na(activity$steps))
      
     head(Missing_values_Count)

    ## [1] 2304

Imputing missing values
=======================

Once I know the number of missing values I am going to replace those
missing values with the mean of the steps taken for each 5 minute
interval.

      Missing_values_Position<-is.na(activity$steps)
     
      StepDataMean_Interval <-round(with(activity,aggregate(steps,by=list(interval),mean,na.rm=TRUE)))
            
      for (i in 1:17568){
              
              if (Missing_values_Position[i] == TRUE) {
                      
                        for(j in 1:nrow(StepDataMean_Interval)){
                              
                              if (activity[i,3] == StepDataMean_Interval[j,1]){
                                      
                                      activity[i,1] <- StepDataMean_Interval[j,2]     
                              }
                        }
                      
                     
                      
              }
      } 
      
      head(activity)

    ##   steps       date interval
    ## 1     2 2012-10-01        0
    ## 2     0 2012-10-01        5
    ## 3     0 2012-10-01       10
    ## 4     0 2012-10-01       15
    ## 5     0 2012-10-01       20
    ## 6     2 2012-10-01       25

Now the dataset "activity" does not have any missing values so I am
going to plot again an histogram with the number of steps taken each
day.

      StepData_No_NA <- with(activity,aggregate(steps,by=list(date),sum))
      
      hist(StepData_No_NA$x,main = "Number of steps in a day",xlab = "Steps",col = "green") 

![](PA_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

      StepDataMean_NoNA <- mean(StepData_No_NA$x,na.rm = TRUE)
      
      StepDataMedian_NoNA <- median(StepData_No_NA$x,na.rm = TRUE)
      
      head(StepDataMean_NoNA)

    ## [1] 10765.64

      head(StepDataMedian_NoNA)

    ## [1] 10762

As we can see when we compare the two histograms it is more frequent
that in a given day the average steps taken would be between 10000 and
15000 when we consider these new dataset without missing values.

Regarding the mean and the median, imputing missing data causes the mean
and the median to have a lower value than when they are not taken into
account.

Finally, it is shown a time series plot of the 5 minute interval and the
average number of steps taken averaged across. Furthermore, it is
specified the 5 minute interval which contains the maximum number of
steps.

      StepDataMean_Interval_NoNA <-round(with(activity,aggregate(steps,by=list(interval),mean,na.rm=TRUE)))
      
      plot(StepDataMean_Interval_NoNA$Group.1,StepDataMean_Interval_NoNA$x,
           ylab = "Number of steps averaged across all days",xlab = "Time",xaxt="n")
      
      title(main = "Average number of steps taken")
      
      axis(side=1,at=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400),
           labels=c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100","1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200","2300","2400"))

![](PA_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

     Max_Step_Interval <- StepDataMean_Interval_NoNA[which(StepDataMean_Interval_NoNA[,2]==max(StepDataMean_Interval_NoNA$x)),1]
     
     print(Max_Step_Interval)

    ## [1] 835

Are there differences in activity patterns between weekdays and weekend?
========================================================================

In order to study if there are any differences in activity patterns
between weekdays and weekend I am going to modify the dataset to create
a new factor variable.

     activity$type <- weekdays(as.Date(activity$date)) 
     
     activity$type <- factor(ifelse(activity$type=="sábado"|activity$type=="domingo" , "Weekend", "Weekday"), c("Weekend", "Weekday"))
     
     head(activity)

    ##   steps       date interval    type
    ## 1     2 2012-10-01        0 Weekday
    ## 2     0 2012-10-01        5 Weekday
    ## 3     0 2012-10-01       10 Weekday
    ## 4     0 2012-10-01       15 Weekday
    ## 5     0 2012-10-01       20 Weekday
    ## 6     2 2012-10-01       25 Weekday

Once I have the new dataset I am going to make a panel plot containing a
time series plot of the 5-minute interval and the average number of
steps taken averaged across all the weekday days or the weekend days.

     StepDataMean_Interval_NoNA_Wk <-aggregate(steps~type+interval,activity,mean)
     
     StepDataMean_Interval_NoNA_Wk$steps<-round(StepDataMean_Interval_NoNA_Wk $steps)
     
     library(lattice)
     
     xyplot(steps ~ interval  | type, data = StepDataMean_Interval_NoNA_Wk , layout = c(1,2))

![](PA_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)
