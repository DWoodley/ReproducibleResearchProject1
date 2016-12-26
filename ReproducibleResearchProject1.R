## Reproducible Research Graded Project 1

## 1. Read in Dataset and process data.
    #File name activity.csv
echo = FALSE

    filename <- "activity.csv"
    setwd("C:/datasciencecoursera/ReproducibleResearch/Project1/")
    activity <- read.csv(filename,header = TRUE)
    
    # Create datetime column from date and interval columns
    library(lubridate)
    library(stringr)
    
    activity$datetime <- str_c(as.character(activity$date),
        str_c(str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=1,end=2),
          str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=3,end=4),"00",
          sep=":"),sep=" ")
    
    activity$datetime <- as_datetime(activity$datetime,tz=Sys.timezone())

    activity$interval.f <- factor(str_c(str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=1,end=2),
                                 str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=3,end=4),
                                 sep=":"),ordered=TRUE)

## 2. Create histogram of number of steps per day.
    hist(tapply(activity$steps,activity$date,sum,na.rm=TRUE),breaks=50,
         xlab = "Number of steps",
         ylab = "Frequency",
         main = "Frequency of Total Steps Per Day \nOver 61 Day Period (Raw Data)")

## 3. Calculate mean and median of number of steps per day.
    mean.steps <- round(mean(tapply(activity$steps,activity$date,sum,na.rm=TRUE)))
    median.steps <- median(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
    
    print(str_c("Mean number of steps per day:",as.character(mean.steps),sep= " "))
    print(str_c("Median number of steps per day:",as.character(median.steps),sep= " "))
    

## 4. Create time series plot of the average number of steps taken.
    # This is a time series plot of the average number of steps taken
    # by 5 minute intervals.
    avg.steps.by.time <- tapply(activity$steps,activity$interval,mean,na.rm=TRUE,ordered=TRUE)
    
    t.avg.steps <- as.data.table(avg.steps.by.time)
    t.avg.steps$interval <- row.names(avg.steps.by.time)
    t.avg.steps$interval <- str_pad(t.avg.steps$interval,4,side="left",pad="0")
    t.avg.steps$interval <- str_c(str_sub(t.avg.steps$interval,1,2),":",str_sub(t.avg.steps$interval,3,4))
    t.avg.steps$interval <- factor(t.avg.steps$interval,ordered = TRUE)
    
    plot(x = t.avg.steps$interval,
         y= t.avg.steps$avg.steps.by.time,
         main="Average Number of Steps by 5 Minute Intervals",
         ylab = "Number of Steps",
         xlab = "Time of Day",
         type = "l")
    

## 5. Find the 5 minute interval that, on average, contains the maximum
##    number of steps.
    max.interval <- t.avg.steps[t.avg.steps$avg.steps.by.time 
                                == max(t.avg.steps$avg.steps.by.time)]$interval

    print(str_c("Time of maximum number of steps:",as.character(max.interval),sep= " "))

    
## 6. Create code to describe and show a strategy for imputing missing data.
    # Will imput missing data where the value of the steps column is NA.
    # Will assume that 0 values of the steps are legitimate data points.
#activity[activity$interval.f==t.avg.steps$interval&is.na(activity$steps),]$Test 
#    <- t.avg.steps[activity$interval.f==t.avg.steps$interval&is.na(activity$steps),avg.steps.by.time]


## 7. Create histogram of the total number of steps taken each day AFTER 
##    missing values are imputed.
#hist(tapply(activity$steps,activity$date,sum,na.rm=TRUE),breaks=50,
#     xlab = "Number of steps",
#     ylab = "Frequency",
#     main = "Frequency of Total Steps Per Day \nOver 61 Day Period (Imputed Data)")


## 8. Create a panel plot comparing the average number of steps taken per
##    per 5-minute interval across weekdays and weekends.
    

## 9. Include all the R code needed to reproduce the results (numbers, plots,
##    etc) in the report.