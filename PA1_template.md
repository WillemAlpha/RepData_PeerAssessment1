---
title: "Activity Data Analysis"
author: "Willem Abrie"
date: "2022-10-04"
output: 
  html_document: 
    keep_md: yes
---




First we load some packages and download the data from the web link...


```r
# Load Packages and get the Data

# Install pacakages
check_and_install_package <- function(package_name){
    if(!package_name %in% installed.packages()){
        install.packages(package_name)
    }
}

check_and_load_package <- function(package_name){
    if(!package_name %in% (.packages())){
        library(package_name, character.only = TRUE)
    }
}

check_and_install_package("tidyverse")
check_and_load_package("tidyverse")


#see to it that a data file exist or is created
if(!file.exists("./data")){dir.create("./data")}

#Download file from the interwebs and unzip
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!(file.exists("data/activData.zip")))
{ download.file(url, destfile ="data/activData.zip", method="curl")}
if (!file.exists("./data/activity.CSV"))
{unzip(zipfile = "data/activData.zip", exdir="data")}
```

Then we load the data into our workspace, do a bit of processing and look at a summary...


```r
data.raw <- read.csv("./data/activity.csv", header = TRUE)
class(data.raw)
```

```
## [1] "data.frame"
```

```r
data.raw$date <- as.Date(data.raw$date, format = "%Y-%m-%d")

summary(data.raw)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
str(data.raw)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
We notice a lot of missing values, and a lot of zeros for steps, also the intervals seems to be set up to show the time (HHMM) during the day at which the interval started. So we transform the interval data to time data.


```r
#Install and load the "chron" package
PG <- "chron"
check_and_install_package(PG)
check_and_load_package(PG)

#convert time with chron package
data.raw$time <- sprintf("%04d", data.raw$interval)
data.raw$time <- paste0(substr(data.raw$time, 1,2), ":", substr(data.raw$time, 3,4), ":", "00")
data.raw$time <- chron(times. = data.raw$time, format = "h:m:s")
class(data.raw$interval)
```

```
## [1] "integer"
```

```r
Sys.setenv(TZ='GMT') #chron assumes GMT time so if you don't change the system TZ the plot will get skewed to translate into your TZ when plotting in ggplot
```

## What is the total number of steps taken per day?

**1. Calculate the total number of steps taken per day**


```r
stepsPerDay <- data.raw %>%
        group_by(date) %>%
        summarise(across(steps, sum)) %>%
        filter(!is.na(steps))
stepsPerDay
```

```
## # A tibble: 53 × 2
##    date       steps
##    <date>     <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # … with 43 more rows
## # ℹ Use `print(n = ...)` to see more rows
```

**2. Make a histogram of the total number of steps taken each day**

```r
hist_dailySteps <- hist(stepsPerDay$steps, xlab = "Total Steps per Day", main = "Histogram of Daily Steps")
```

![](PA1_template_files/figure-html/stepsHist-1.png)<!-- -->

```r
hist_dailySteps
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "stepsPerDay$steps"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

**3. Calculate and report the mean and median of the total number of steps taken per day**


```r
meanDaily <- mean(stepsPerDay$steps)
meanDaily

medianDaily <- median(stepsPerDay$steps)
medianDaily
```

The mean amount of daily steps is 1.0766189\times 10^{4}.
The median amount of daily steps is 10765.


## What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
dailyPattern <- data.raw %>%
        group_by(time) %>%
        filter(!is.na(steps)) %>%
        summarise(across(steps, mean, rm.NA = TRUE))
dailyPattern
```

```
## # A tibble: 288 × 2
##    time      steps
##    <times>   <dbl>
##  1 00:00:00 1.72  
##  2 00:05:00 0.340 
##  3 00:10:00 0.132 
##  4 00:15:00 0.151 
##  5 00:20:00 0.0755
##  6 00:25:00 2.09  
##  7 00:30:00 0.528 
##  8 00:35:00 0.868 
##  9 00:40:00 0     
## 10 00:45:00 1.47  
## # … with 278 more rows
## # ℹ Use `print(n = ...)` to see more rows
```

```r
#plot(x = dailyPattern$time, y = dailyPattern$steps, type = 'l', xlab = "Start time of Interval" , ylab = "Mean Number of Steps accross days", main = "Daily Pattern")
check_and_install_package("ggthemes")
if(!"ggthemes" %in% (.packages())){
        library("ggthemes")
    }
```

```
## Warning: package 'ggthemes' was built under R version 4.2.1
```

```r
df_timePlot <- dailyPattern %>%
        ggplot(aes(x = time, y = steps, color = "red")) +
        geom_point(size = 1, show.legend = FALSE) +
        geom_line(color = "black", alpha = 0.6) +
        labs(
                x = "Start Time of Interval", 
                y = "Mean Number of Steps accross days", 
                title = "Daily Pattern",
                subtitle = "On average, how many steps at intervals throughout the day?"
        ) +
        theme_fivethirtyeight() +
        theme(axis.title = element_text()) + #reinstate axis titled that were removed by chosen theme
        scale_x_chron(n = 16, format = "%H:%M")
df_timePlot
```

![](PA1_template_files/figure-html/dailypattern-1.png)<!-- -->




**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
maxInterval <- dailyPattern %>%
        filter(steps == max(steps)) 
maxInterval
```

```
## # A tibble: 1 × 2
##   time     steps
##   <times>  <dbl>
## 1 08:35:00  206.
```

## Imputing missing values


**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

This was already mostly done above but will be repeated here:


```r
summary(data.raw)[7] #total amount of NA's
```

```
## [1] "NA's   :2304  "
```

```r
colSums(is.na(data.raw)) #check for each column including text columns
```

```
##    steps     date interval     time 
##     2304        0        0        0
```

```r
#calculate NA's as percentage of all observations
100*parse_number(summary(data.raw)[7])/dim(data.raw)[1]
```

```
## [1] 13.11475
```
So 13.1% of the observations have missing step data.Are they mostly in the time intervals that we are not expecting many steps?


```r
#install lubridate package
PG <- "lubridate"
check_and_install_package(PG)
check_and_load_package(PG)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:chron':
## 
##     days, hours, minutes, seconds, years
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
missingValsPerInterval <- data.raw %>%
        group_by(time) %>%
        dplyr::summarise(count_NA = sum(is.na(steps)))
missingValsPerInterval
```

```
## # A tibble: 288 × 2
##    time     count_NA
##    <times>     <int>
##  1 00:00:00        8
##  2 00:05:00        8
##  3 00:10:00        8
##  4 00:15:00        8
##  5 00:20:00        8
##  6 00:25:00        8
##  7 00:30:00        8
##  8 00:35:00        8
##  9 00:40:00        8
## 10 00:45:00        8
## # … with 278 more rows
## # ℹ Use `print(n = ...)` to see more rows
```

```r
missingdays <- data.raw %>%
        group_by(date) %>%
        dplyr::summarise(countMissingIntervals = sum(is.na(steps))) %>%
        dplyr::filter(!countMissingIntervals == 0) %>%
        mutate(weekday = wday(date, label = TRUE))
missingdays
```

```
## # A tibble: 8 × 3
##   date       countMissingIntervals weekday
##   <date>                     <int> <ord>  
## 1 2012-10-01                   288 Mon    
## 2 2012-10-08                   288 Mon    
## 3 2012-11-01                   288 Thu    
## 4 2012-11-04                   288 Sun    
## 5 2012-11-09                   288 Fri    
## 6 2012-11-10                   288 Sat    
## 7 2012-11-14                   288 Wed    
## 8 2012-11-30                   288 Fri
```
We can see there were 8 days where no values were recorded. All other days in the study were complete.



**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

*Since the missing values are in full days are shown above, I'll populate each missing day from the average of that weekday.*

If the person had a weekly routine, this should have a good result.
If they were someone who, for example, were working irregular shifts, this would not be such a good approach.
But since the former is more common, I'll go with that.
To check this assumption we'll do a heatmap for each day of the week, showing the activity across the 2 months...


```r
weekdayActivity <- data.raw %>%
        mutate(weekday = wday(date, label = TRUE)) %>% ##add day of week labels
        dplyr::filter(!date %in% missingdays$date) %>% ##remove the bad days
        group_by(weekday, time) 

weekdayActivity %>%
        dplyr::filter(weekday == "Mon") %>%
        ggplot(aes(date, time, fill = steps )) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "red", limits = c(0, 810)) #+
```

```
## Don't know how to automatically pick scale for object of type times. Defaulting to continuous.
```

![](PA1_template_files/figure-html/activityByWeekday-1.png)<!-- -->

```r
        #facet_grid(. ~ weekday)
```
The same can be done for every other weekday and shows that the strategy should be okay.


**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```r
#Generate data of what average days look like, per day of the week.

#start by filtering out the NAs and processing a little until we get mean steps for each day of the week at each interval
dataToImpute <- data.raw %>%
        mutate(weekday = wday(date, label = TRUE)) %>% ##add day of week labels
        dplyr::filter(!date %in% missingdays$date) %>% ##remove the bad days
        group_by(weekday, interval) %>% ##group by weekday and interval and take the mean steps
        summarise(across(steps, mean))
```

```
## `summarise()` has grouped output by 'weekday'. You can override using the
## `.groups` argument.
```

```r
#visualise the mean steps matrix
visWeekDay <- dataToImpute %>%
        spread(weekday, steps) %>% ##spread steps by day
        select(-interval) #%>%
        #heatmap(as.matrix())

heatmap(as.matrix(visWeekDay))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#create a df with values for the days with missing steps
data_filled_subset <- data.raw %>%
        mutate(weekday = wday(date, label = TRUE)) %>% ##add day of week labels
        dplyr::filter(date %in% missingdays$date) %>% ##take only the bad days
        left_join(dataToImpute, by = c("interval", "weekday")) %>%
        mutate(steps = coalesce(steps.x, steps.y), .before = date) %>% #keep only the non-missing values as "steps"
        select(-steps.x, -steps.y, - weekday) 
  
data_missingImputed <- data.raw %>%
        anti_join(data_filled_subset, by = c("date", "interval")) %>% #discard all the observations that are in the filled subset
        bind_rows(data_filled_subset) %>% #add in the rows with imputed data
        arrange(date, interval)

#install arsenal package
PG <- "arsenal"
check_and_install_package(PG)
check_and_load_package(PG)
```

```
## 
## Attaching package: 'arsenal'
## 
## The following object is masked from 'package:lubridate':
## 
##     is.Date
```

```r
summary(data_missingImputed)
```

```
##      steps             date               interval           time         
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :00:00:00  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:05:58:45  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Median :11:57:30  
##  Mean   : 37.57   Mean   :2012-10-31   Mean   :1177.5   Mean   :11:57:30  
##  3rd Qu.: 19.04   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.:17:56:15  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :23:55:00
```

```r
comparedf(data.raw, data_missingImputed)
```

```
## Compare Object
## 
## Function Call: 
## comparedf(x = data.raw, y = data_missingImputed)
## 
## Shared: 4 non-by variables and 17568 observations.
## Not shared: 0 variables and 0 observations.
## 
## Differences found in 0/3 variables compared.
## 0 variables compared have non-identical attributes.
```



**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. **
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
#Calculate the total number of steps taken per day
stepsPerDay2 <- data_missingImputed %>%
        group_by(date) %>%
        summarise(across(steps, sum)) %>%
        filter(!is.na(steps))
stepsPerDay2
```

```
## # A tibble: 61 × 2
##    date        steps
##    <date>      <dbl>
##  1 2012-10-01  9975.
##  2 2012-10-02   126 
##  3 2012-10-03 11352 
##  4 2012-10-04 12116 
##  5 2012-10-05 13294 
##  6 2012-10-06 15420 
##  7 2012-10-07 11015 
##  8 2012-10-08  9975.
##  9 2012-10-09 12811 
## 10 2012-10-10  9900 
## # … with 51 more rows
## # ℹ Use `print(n = ...)` to see more rows
```

```r
#generate histogram - I took some tips here: #https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/

hist_dailySteps2 <- hist(stepsPerDay2$steps, xlab = "Total Steps per Day", main = "Histogram of Daily Steps - Missing vals Imputed")
```

![](PA1_template_files/figure-html/TotalSteps2-1.png)<!-- -->

```r
rx <- range(c(hist_dailySteps$breaks, hist_dailySteps2$breaks)) # Get range for x-axis
y_max <- max(c(hist_dailySteps$count, hist_dailySteps2$count)) # Get range for y-axis
c1 <- rgb(173,216,230,max = 255, names = "lt.blue") #color 1
c2 <- rgb(255,192,203, max = 255, names = "lt.pink") #color 2

plot(hist_dailySteps2, col = c1, xlim = rx, ylim = c(0, y_max), xlab = "Mean Steps per Day")
plot(hist_dailySteps, add = TRUE, col = c2)
legend(17000, 30, legend=c("Missing Vals Imputed", "with missing vals"), 
       fill = c(c1, c2))
```

![](PA1_template_files/figure-html/TotalSteps2-2.png)<!-- -->


Calculate and report the mean and median of the total number of steps taken per day


```r
meanDaily2 <- mean(stepsPerDay2$steps)
meanDaily2
meanPercentChange <- 100*(meanDaily2 - meanDaily)/meanDaily
meanPercentChange

medianDaily2 <- median(stepsPerDay2$steps)
medianDaily2
medianPercentChange <- 100*(medianDaily2 - medianDaily)/medianDaily
medianPercentChange
```

The mean amount of daily steps is 1.082121\times 10^{4} vs 1.0766189\times 10^{4} before imputation. This is a 0.5110529% increase.
The median amount of daily steps is 1.1015\times 10^{4} vs 10765 before imputation.This is a 2.3223409% increase.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**


```r
#install new package to access isWeekend() function
PG <- "timeDate"
check_and_install_package(PG)
check_and_load_package(PG)


#Calculate the mean number of steps taken per average day, grouped by day type (i.e. weekday or weekend)
data_dayTypePlot <- data_missingImputed %>%
        mutate(dayType = ifelse(isWeekend(date) == TRUE,"average weekend day", "average weekday")) %>%
        mutate(across(dayType, as.factor)) %>% #change into factor
        group_by(interval, dayType) %>%
        summarise(across(steps, mean, rm.NA = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
data_dayTypePlot
```

```
## # A tibble: 576 × 3
## # Groups:   interval [288]
##    interval dayType              steps
##       <int> <fct>                <dbl>
##  1        0 average weekday     2.31  
##  2        0 average weekend day 0     
##  3        5 average weekday     0.45  
##  4        5 average weekend day 0     
##  5       10 average weekday     0.175 
##  6       10 average weekend day 0     
##  7       15 average weekday     0.2   
##  8       15 average weekend day 0     
##  9       20 average weekday     0.0889
## 10       20 average weekend day 0     
## # … with 566 more rows
## # ℹ Use `print(n = ...)` to see more rows
```

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).* See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**



```r
#install new package
PG <- "lattice"
check_and_install_package(PG)
check_and_load_package(PG)


#generate lattice xyplot
lattice::xyplot(data_dayTypePlot$steps~data_dayTypePlot$interval|data_dayTypePlot$dayType, 
                        type = "l",
                        main = "Average Steps by Working Week or Weekend",
                        ylab = "Number of Steps", xlab="Interval",
                        layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



Remember to restore the system timezone


```r
Sys.setenv(TZ="Australia/Brisbane") #set back after plotting
```

