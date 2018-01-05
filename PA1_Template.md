# Reproducible Research PA_1
Ian Yang  
January 4, 2018  



# Loading and preprocessing the data

```r
data = read.csv("activity.csv", header = T)
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

#What is mean total number of steps taken per day? (ignore missing value)

```r
data_omit = na.omit(data)
head(data_omit)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
total_day = aggregate(x = data_omit[c("steps")], FUN = sum, by = list(Group.date = data_omit$date))
```
###Histogram of total steps per day

```r
hist(total_day$steps, breaks=10, xlab = "Steps", main = "Total Steps per Day", xlim = c(0,25000))
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
###Mean/Median of steps taken per day

```r
mean_day = mean(total_day$steps)
mean_day
```

```
## [1] 10766.19
```

```r
median_day = median(total_day$steps)
median_day
```

```
## [1] 10765
```
###What is the average daily activity pattern


```r
interval = aggregate(x = data_omit[c("steps")], FUN = mean, by = list(interval = data_omit$interval))
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
ggplot(interval, aes(x = interval, y = steps)) + geom_line(color = "green", size = 1) +
  labs(title = "Average Steps Taken per Day", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
###Interval with the max. number of steps

```r
interval[which.max(interval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```


#Imputing missing values
###Count missing values

```r
sum(is.na(data))
```

```
## [1] 2304
```
###Filling in all of the missing values by using mean

```r
data2 = data
nas = is.na(data2$steps)
avg_interval = tapply(data2$steps, data2$interval, FUN = mean, na.rm = TRUE, simplify = TRUE)
data2$steps[nas] = avg_interval[as.character(data2$interval[nas])]

total_day2 = aggregate(x = data2[c("steps")], FUN = sum, by = list(Group.date = data2$date))

hist(total_day2$steps, breaks=10, xlab = "Steps", main = "Total Steps per Day", xlim = c(0,25000))
```

![](PA1_Template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean_day = mean(total_day2$steps)
mean_day
```

```
## [1] 10766.19
```

```r
median_day = median(total_day2$steps)
median_day
```

```
## [1] 10766.19
```
###Conclusion: After filling the missing value, I found that both mean and median of total steps per day are the same

#Are there differences in activity patterns between weekdays and weekends?
###Create New factor variables weektype

```r
library(magrittr)
```

```
## Warning: package 'magrittr' was built under R version 3.4.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
head(data2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
data2$date = as.Date(data2$date, format = "%Y-%m-%d")
data2 = data2%>%
  mutate(weektype = ifelse(weekdays(data2$date) =="Saturday"|weekdays(data2$date) =="Sunday", "Weekend", "Weekday"))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.2
```

```r
head(data2)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  Weekday
## 2 0.3396226 2012-10-01        5  Weekday
## 3 0.1320755 2012-10-01       10  Weekday
## 4 0.1509434 2012-10-01       15  Weekday
## 5 0.0754717 2012-10-01       20  Weekday
## 6 2.0943396 2012-10-01       25  Weekday
```

```r
data_weekday = data2[which(data2$weektype == "Weekday"), ]
head(data_weekday)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  Weekday
## 2 0.3396226 2012-10-01        5  Weekday
## 3 0.1320755 2012-10-01       10  Weekday
## 4 0.1509434 2012-10-01       15  Weekday
## 5 0.0754717 2012-10-01       20  Weekday
## 6 2.0943396 2012-10-01       25  Weekday
```

```r
data_weekend = data2[which(data2$weektype == "Weekend"), ]
head(data_weekend)
```

```
##      steps       date interval weektype
## 1441     0 2012-10-06        0  Weekend
## 1442     0 2012-10-06        5  Weekend
## 1443     0 2012-10-06       10  Weekend
## 1444     0 2012-10-06       15  Weekend
## 1445     0 2012-10-06       20  Weekend
## 1446     0 2012-10-06       25  Weekend
```

```r
interval_weekday = aggregate(x = data_weekday$steps, FUN = mean, by = list(interval = data_weekday$interval))
interval_weekday = interval_weekday%>%
  mutate(weektype = "Weekday")
interval_weekend = aggregate(x = data_weekend$steps, FUN = mean, by = list(interval = data_weekend$interval))
interval_weekend = interval_weekend%>%
  mutate(weektype = "Weekend")

colnames(interval_weekday) = c("interval", "avg_steps", "weektype")
colnames(interval_weekend) = c("interval", "avg_steps", "weektype")
interval2 = rbind(interval_weekday, interval_weekend)
head(interval2)
```

```
##   interval  avg_steps weektype
## 1        0 2.25115304  Weekday
## 2        5 0.44528302  Weekday
## 3       10 0.17316562  Weekday
## 4       15 0.19790356  Weekday
## 5       20 0.09895178  Weekday
## 6       25 1.59035639  Weekday
```
###Plot

```r
plot = ggplot(interval2, aes(x = interval, y = avg_steps, color = weektype)) + 
  geom_line() + labs(title = "Average Steps Taken per Day", x = "Interval", y = "Avg. Steps per day") +
  facet_wrap(~weektype, ncol = 1, nrow = 2)

print(plot)
```

![](PA1_Template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

###The test object is more active in the morning during weekday but overall walk more during the weekend. It's intuitively makes sense because people usually go to work during weekday and have more spare time during weekend.

