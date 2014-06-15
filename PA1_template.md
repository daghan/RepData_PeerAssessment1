# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1- First, get the filename within the zip archive  
2- Then, extract the file  
3- Finally, convert the date column from text to Date format  

```r
filename <- as.character(unzip("./activity.zip", list=T))[1]
unzip("./activity.zip", files=filename, exdir="./")
df <-read.table("./activity.csv", header=T, sep=",")
df$date <- as.Date(df$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
The  histogram of the total number of steps taken each day, with 10 bins is:  

```r
library(plyr)
res <- ddply(df, .(Timestamp=df$date), function(x) sum(x$steps))
colnames(res)[2] <- "Steps"
hist(res$Steps, breaks=10, main="Number of steps per day", xlab="Number of steps", ylab="Days")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean and median total number of steps taken per day are as follows:  

```r
mean(res$Steps, na.rm=T)  
```

```
## [1] 10766
```

```r
median(res$Steps, na.rm=T)  
```

```
## [1] 10765
```

## What is the average daily activity pattern?
The plot of the average daily steps, per 5 min activity intervals are:  

```r
avg.steps <- aggregate(df$steps, by=list(df$interval), function(x) mean(x, na.rm=TRUE))
colnames(avg.steps) <- c("intervals", "steps")  
plot(y=avg.steps$steps, x=avg.steps$intervals, ylab="averega steps", xlab="interval during the day", type='l')  
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The following 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:  

```r
suppressWarnings(avg.steps$interval[which.max(avg.steps$steps)])
```

```
## [1] 835
```

## Imputing missing values
Total number of missing values in the dataset (i.e. the total number of rows with NAs) is:  

```r
sapply(df, function(x) sum(is.na(x)))[1]
```

```
## steps 
##  2304
```

Histogram when NAs are replaced by interval averages across the whole dataset. (ex: if interval 030 was NA for a given day, it is replaced by the mean of interval 030 across all days)  
The steps are:  
1- Create a new dataframe that includes a new row, which contains the average for that given interval  
2- Sort by Date and then interval  
3- Add a new row that has the original steps data  
4- But subsitute the NAs with the average for that inverval from the row added at step #1 above  
5- Re-run the histogram, using the same code-chunk in section 2   


```r
df.merged <- merge(df,avg.steps, by.x="interval", by.y="intervals")
df.merged.sorted <- df.merged[with(df.merged,order(date,interval)),]
df.merged.sorted$corrected.steps = df.merged.sorted$steps.x
df.merged.sorted$corrected.steps[with(df.merged.sorted,which(is.na(steps.x)))] <- df.merged.sorted$steps.y[with(df.merged.sorted,which(is.na(steps.x)))]
res.corrected <- ddply(df.merged.sorted, .(Timestamp=df$date), function(x) sum(x$corrected.steps))
colnames(res.corrected)[2] <- "Steps"
hist(res.corrected$Steps, breaks=10, main="Number of steps per day", xlab="Number of steps", ylab="Days")  
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


The mean and median total number of steps taken per day (after substituting NA steps with the average for the given interval) are as follows:  

```r
mean(res.corrected$Steps, na.rm=T)  
```

```
## [1] 10766
```

```r
median(res.corrected$Steps, na.rm=T)  
```

```
## [1] 10766
```

### The substitution strategy above made a very little impact to the histogram, mean and median values


## Are there differences in activity patterns between weekdays and weekends?
First, we need to add week factor (levels are weekday, weekend) to the dataset  

```r
df.merged.sorted$day <- weekdays(df.merged.sorted$date)
df.merged.sorted$weekfactor <- factor(!(df.merged.sorted$day %in% c("Saturday", "Sunday")), labels = c("weekend", "weekday"))
```

Now, we can subset the dataset into 2 datasets (one for the weekend, and another one for the week)  

```r
df.weekday = subset(df.merged.sorted, df.merged.sorted$weekfactor == "weekday")
df.weekend = subset(df.merged.sorted, df.merged.sorted$weekfactor == "weekend")
```

We can now re-use the original code-chunk for calculating interval averages for steps across all days within the dataset

```r
avg.weekday.steps <- aggregate(df.weekday$corrected.steps, by=list(df.weekday$interval), 
                               function(x) mean(x, na.rm=TRUE))
colnames(avg.weekday.steps) <- c("intervals", "steps")  
avg.weekend.steps <- aggregate(df.weekend$corrected.steps, by=list(df.weekend$interval), 
                               function(x) mean(x, na.rm=TRUE))
colnames(avg.weekend.steps) <- c("intervals", "steps")  
par(mfrow = c(2, 1))
plot(y=avg.weekend.steps$steps, x=avg.steps$intervals, ylab="averega steps", xlab="interval during the day", type='l', main = "Weekend")  
plot(y=avg.weekday.steps$steps, x=avg.steps$intervals, ylab="averega steps", xlab="interval during the day", type='l', main = "Weekday")  
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
