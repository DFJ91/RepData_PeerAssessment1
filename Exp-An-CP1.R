## Exploratory Data Analysis ##
    ## Course Project 1 ##

# Loading and preprocessing data

setwd("/Users/DavidFernandez/ExpR-CP1")

dir()

activity<-read.csv("/Users/DavidFernandez/ExpR-CP1/activity.csv")

# What is mean total number of steps taken per day?

  #Total number of steps taken per day

  StepData <- with(activity,aggregate(steps,by=list(date),sum))
 
  #Histogram of the total number of steps taken each day
  
  hist(StepData$x,main = "Number of steps in a day",xlab = "Steps",col = "green") 

  #Calculate and report the mean and the median of the total number of steps
  #taken per day
 
  StepDataMean <- mean(StepData$x,na.rm = TRUE)

  StepDataMedian <- median(StepData$x,na.rm = TRUE)
  
# What is the average daily activity pattern?
  
  #Calculate and report the total number of missing values in the dataset
  
  Missing_values_Count<-sum(is.na(activity$steps))
  
  Missing_values_Position<-is.na(activity$steps)
  
  #Create a dataset that is equal to the original dataset but with the
  #data filled in
  
  StepDataMean_Interval <-round(with(activity,aggregate(steps,by=list(interval),mean,na.rm=TRUE)))
        
  for (i in 1:17568){
          
          if (Missing_values_Position[i] == TRUE) {
                  
                 ## Mytime = activity[i,3]
                  
                    for(j in 1:nrow(StepDataMean_Interval)){
                          
                          if (activity[i,3] == StepDataMean_Interval[j,1]){
                                  
                                  activity[i,1] <- StepDataMean_Interval[j,2]     
                          }
                    }
                  
                 
                  
          }
  } 
  
  # Make a histogram of the total number of steps taken each day
  
  StepData_No_NA <- with(activity,aggregate(steps,by=list(date),sum))
  
  hist(StepData_No_NA$x,main = "Number of steps in a day",xlab = "Steps",col = "green") 
  
  #Report the mean and median total number of steps taken per day
  
  StepDataMean_NoNA <- mean(StepData_No_NA$x,na.rm = TRUE)
  
  StepDataMedian_NoNA <- median(StepData_No_NA$x,na.rm = TRUE)
  
  #Make a time series plot of the 5 minute interval and the 
  #average number of steps taken, averaged across all days
  
  StepDataMean_Interval_NoNA <-round(with(activity,aggregate(steps,by=list(interval),mean,na.rm=TRUE)))
  
  plot(StepDataMean_Interval_NoNA$Group.1,StepDataMean_Interval_NoNA$x,
       ylab = "Number of steps averaged across all days",xlab = "Time",xaxt="n")
  
  title(main = "Average number of steps taken")
  
  axis(side=1,at=c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400),
       labels=c("0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100","1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200","2300","2400"))

 #Which 5-minute interval, on average across all the days in the dataset,
 #contains the maximum number of steps?
  
 Max_Step_Interval <- StepDataMean_Interval_NoNA[which(StepDataMean_Interval_NoNA[,2]==max(StepDataMean_Interval_NoNA$x)),1]
 
 print(Max_Step_Interval)
 
# Are there differences in activity patterns between weekdays and weekend?
 
 # Create a new factor variable in the dataset with two levels "weekday" and 
 # weekend
 
 activity$type <- weekdays(as.Date(activity$date)) 
 
 activity$type <- factor(ifelse(activity$type=="sábado"|activity$type=="domingo" , "Weekend", "Weekday"), c("Weekend", "Weekday"))
 
 # Make a panel plot containing a time series plot of the 5 minutes interval
 # and the average number of steps taken, averaged across all weekday or weekend days
 
 StepDataMean_Interval_NoNA_Wk <-aggregate(steps~type+interval,activity,mean)
 
 StepDataMean_Interval_NoNA_Wk$steps<-round(StepDataMean_Interval_NoNA_Wk $steps)
 
 library(lattice)
 
 xyplot(steps ~ interval  | type, data = StepDataMean_Interval_NoNA_Wk , layout = c(1,2))
  
