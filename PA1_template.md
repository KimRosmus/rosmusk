# PA1_template
Kim Rosmus  
October 22, 2017  

**Course 5 Week 2 assignment- Kim Rosmus  

**Loading and preprocessing the data**

Show any code that is needed to  

- Load the data (i.e. read.csv())
- Process/transform the data (if necessary) into a format suitable for your analysis



```r
ACT <- read.csv ("activity.csv",  
                 header= TRUE,  
                 sep= ",",  
                 na.strings = "NA")  
ACT_conv <-transform (ACT, date=as.Date(date, format="%Y-%m-%d")) 

str(ACT_conv)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
**What is mean total number of steps taken per day?**

For this part of the assignment, you can ignore the missing values in the dataset.  

- Calculate the total number of steps taken per day
- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
- Calculate and report the mean and median of the total number of steps taken per day  

```r
sum(ACT$steps, na.rm=TRUE)  
```

```
## [1] 570608
```

```r
spd <- aggregate(steps ~ date, data = ACT, sum, na.rm = TRUE)  

aspd <- spd$steps  

hist(spd$steps,  
     main = "Total number of steps taken each day",  
     col= "blue",   
     xlab = "Steps")    
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean (spd$steps)  
```

```
## [1] 10766.19
```

```r
median (spd$steps)  
```

```
## [1] 10765
```
**What is the average daily activity pattern?**  

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
averagestep <- aggregate(steps ~ interval, data = ACT, mean, na.rm = TRUE)  

plot(x = averagestep$interval,   
     y = averagestep$steps,   
     type = "l",  
     lwd = 2,  
     col= "orange",  
     main = "Average number of steps taken- Time Series Plot",  
     xlab = "5min interval",  
     ylab = "Average number of steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
averagestep$interval[which.max(averagestep$steps)]  
```

```
## [1] 835
```
##**Imputing missing values**

-Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  

-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
-Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
-Create a new dataset that is equal to the original dataset but with the missing data filled in.  
-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
sum(is.na(ACT))  
```

```
## [1] 2304
```

```r
averagestep_fillmissingvalues <- aggregate(steps ~ interval, data = ACT_conv, mean)   

ACT_conv_merge <- merge(x=ACT_conv, y= averagestep_fillmissingvalues, by = "interval")  

ACT_conv_merge$steps <- ifelse(is.na(ACT_conv_merge$steps.x), ACT_conv_merge$steps.y, ACT_conv_merge$steps.x)  

ACT.conv.na <- ACT_conv_merge[c("steps", "date", "interval")]  

spd_na <- aggregate(steps ~ date, data = ACT.conv.na, sum)  

aspd_na <- spd_na$steps  

hist(spd_na$steps,  
     main = "Total number of steps taken each day- NA terms removed",  
     col= "green",   
     xlab = "Steps")    
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean (spd_na$steps)  
```

```
## [1] 10766.19
```

```r
median (spd_na$steps)
```

```
## [1] 10766.19
```

-By replacing the NA values, the median is equal to the mean. Before the median was less than the mean.  

```r
ACT.conv.na$weekdays <- as.factor(ifelse(weekdays(ACT.conv.na$date) %in% c("Saturday", "Sunday"), "weekend", "weekdays"))

library(lattice)

aspd_weekdays <- aggregate(steps ~ weekdays + interval, data = ACT.conv.na, mean)  

xyplot (steps ~ interval | weekdays, aspd_weekdays,
        type = "l",
        col= "black",
        xlab = "Interval",
        ylab = "Number of Steps",
        main = "Average number of steps taken, across weekdays and weekends",
        layout = c (1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


