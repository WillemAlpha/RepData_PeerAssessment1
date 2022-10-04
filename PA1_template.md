---
title: "Activity Data Analysis"
author: "Willem Abrie"
date: "2022-10-04"
output: html_document
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
[1] "data.frame"
```

```r
data.raw$date <- as.Date(data.raw$date, format = "%Y-%m-%d")

summary(data.raw)
```

```
     steps             date               interval     
 Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
 Median :  0.00   Median :2012-10-31   Median :1177.5  
 Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
 3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
 Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
 NA's   :2304                                          
```

```r
str(data.raw)
```

```
'data.frame':	17568 obs. of  3 variables:
 $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
 $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
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
[1] "integer"
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
# A tibble: 53 x 2
   date       steps
   <date>     <int>
 1 2012-10-02   126
 2 2012-10-03 11352
 3 2012-10-04 12116
 4 2012-10-05 13294
 5 2012-10-06 15420
 6 2012-10-07 11015
 7 2012-10-09 12811
 8 2012-10-10  9900
 9 2012-10-11 10304
10 2012-10-12 17382
# ... with 43 more rows
# i Use `print(n = ...)` to see more rows
```

**2. Make a histogram of the total number of steps taken each day**

```r
hist_dailySteps <- hist(stepsPerDay$steps, xlab = "Total Steps per Day", main = "Histogram of Daily Steps")
```

![plot of chunk stepsHist](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAA2AAAAKgCAMAAADK0+6WAAAAvVBMVEUAAAAAADoAAGYAOjoAOmYAOpAAZpAAZrY6AAA6ADo6AGY6OgA6Ojo6OmY6OpA6ZpA6ZrY6kLY6kNtmAABmADpmOgBmOjpmkJBmkLZmkNtmtttmtv+QOgCQZgCQZjqQkGaQkLaQtpCQttuQtv+Q27aQ2/+2ZgC2Zjq2ZpC2kGa225C227a229u22/+2/9u2///T09PbkDrbkGbbtmbbtpDb27bb29vb/9vb////tmb/25D/27b//7b//9v///8M5Z0AAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAcwElEQVR4nO3da0MjyXWH8WYyBJyxHRt2HduQ3dgJ2twmE+0lQQzS9/9Y7mupoRCoW3X0Pzo8vxe7LMMUZ6r0rKQWA9UGgJlKPQAQGYEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIrDS1rfV+X3374v0H6Nf/vcvpT/hD5dV9bv2zWXVOvvHzy/MlM/S/dp/fNr+jvLTvXcEVtrrgX399kPpm/CqaeqqfbMPrA7mJp/p5cDq925/h8F07x2BlfYksMyiKn4TXlZnd+nNVNhd9nEvB5Z+SzOXwXTvHYGV9tI92I/Do7DFcFP++Q9V9Xd/bG/v6x+q6uNde9uuW/nLt9WHz5ufv60/7jd1JI/X1cXPn6qzP29+vKw/avtp0gKLUU/LLpBf6t/dxJ1WSfdg/99XX9/r9fdxi2btzdfb5h3DdM1EZ7+7b3/1w3/Vn+hj+wDyx08vPPrE6wistBcCW6b7iOEm3L+n+cDuMdrZp+27z+9X6W6oDqzzaUizs13ghcCa31X/ynaV7UPE/iO2d1X9Ww+X9aPMfrr+c3780tfbr7/9U2B/BFba8KSmqobAmnuh+81P7ROl7vZc35y77q7aO5OL+/Vi6O788+b/6t/19/fdbb65sV81v7f6c//xrfECTx4i9jf/5o3RKtvA6v+86e4X02+pzv743/1/LIYHip+739cEVr/9U/NHGf8psDcCKy0PrL6tfsxuwul+pHu7vvl2gQ1XJ35pLu5d9PdF3T+72/xomc3wuPKFwLr39atsA+ueGW4fIQ7jfvzr/bBe/cmuhrUW3Qc27x//KbA3AistD6x7T/eMq70JD48imwyGt5+0sv5uWKC7r+n+uQ1svMDuwLarjK4i9t2MLnf89Gn0iLD+xfrTpMeFfcj9oOlPgb0RWGkvPAf7+m13g/3X7X1EutQwvD0OrLktf/yXX653BjZeYGdgo1VGgTWPEfv7qO3Iv3x/2V4WaYdYVVlg7ecZ/SmwNwIr7cXXwb5+/2m4JLHHPVhX0uPuwN66B6t/vX1MN6wyCqwZa/Q70prbh6vd07TN9r3pEWf6U2BvBFbarhea1/+8fca1+zlYe0PuniINFzleCOyN52DNRfeL8SrjF5qXzRXLFEm98tmf6v/4+TLNMroCMn4ONv5TYG8EVloeWH1L/337OLF9CtTUsPsq4nAPdn7fVbIjsJ1XEUcP70arjANrn2JtHyFuf8vFsFT70tjX6/41gOEq4vhPgb0RWGkv3IP9sH0xafXG62DpOdirFznGr4O9GFjzhU+jVcaBNe8e3Qmlj2retxq/Dta1tv3F0Z8CeyOw0l56iNheqPtN8zUQ7ZdtfO6/EONP3W+o3/Xbz9lVxI9/XXY39hcDGy2QB9Z/ucV2lSdfi7iqnnwN17odrrs62E/39bv6Xq5do57qf77r3x79KbA3AnNicbSHXqMXwd7EFyceisDUFu3fNanvnV740mAL9bOo/aMhsEMRmNrwstNxntu0z6/2/2InAjsUgcl9/cPl8b5Kvbks//v9P5zADkVggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIFJVX6otyIo9lWq+l8vuCHYYF+lCCw69lWKwKJjX6UILDr2VYrAomNfpQgsOvZVisCiY1+lCCw69lWKwKJjX6UILDr2VYrAomNfpQgsOvZVisCiY1+lCCw69lWKwKJjX6UILDr2VYrAomNfpQgsOvZVisCiY1+lCCw69lWKwKJjX6UILDr2VYrAomNfpQgsOvZVisCiY1+lCCw69lWKwKJjX6UILDr2VYrAomNfpQgsOvZVisCiY1+lCCw69lWKwKJjX6UILDr2VYrAomNfpQgsOvZVisCiY1+lCCw69lWKwKJjX6UILLop+/p4fVX/c1VV1YcvVvO8MwQW3eTAluf3zVs3ZhO9KwQW3dTA+rTazHAwAotuamAPl21gKx4kFkFg0XEPJkVg0U0LrKpdbIbLHTgYgUU3cV/rxs7uNquKvsogsOjYVykCi67MvlZJkeXeDwKLbtq+LuuEuoscO64ickzTEFh0k/Z1WT//erxurnIQWBkEFt2UfV3fXrX/PL8nsEIILLppl+m7L5BanN8TWBkEFt30e7Da4oLAyiCw6KY9B+uzerze9fX0HNM0BBbd1KuI3YPE9S2BFUFg0RXeV45pGgKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCw6ApMisOgITIrAoiMwKQKLjsCkCCy6afu6vq1aH74UWQ4EFt2kfV1WV90bq+GNg5YDgYU3ZV/Xtymr5fn9wcuBwOKbsq+P1zfDm6sdDxI5pmkILDruwaQILLqJz8H6uzCegxVCYNFN29fH6+4q4o77LwKbisCi43UwKQKLrsy+VkmR5d4PAotu2r4u64Tap2FLriIWQWDRTbvIcXZXPw272BBYKQQW3fTL9Ovb83sCK4TAopvzQvPi/J7AyiCw6Ga90Ly4ILAyCCy6ac/B+qwer3d9PT3HNA2BRTf1KmL3IHF9S2BFEFh0vNAsRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDREZgUgUVHYFIEFh2BSRFYdAQmRWDRpX19vK4uCi6HvRBYdKN9XVZVdVVuOeyBwKJ7uq8HN8YxTUNg0T3f1yaxD1+KLYfXEVh0T/Z1Vdd1s1nfnt8XWQ5vIrDotvv6eF1VXVmr+XdhHNM0BBbd6Cri2V3B5bAXAouO18GkCCy60b4u6geIqwMv1HNM0xBYdNt9XbRPwB6vD3q5mWOahsCiGz0Hu2n/fcAVjg2BTUVg0aV9Xd92Dw6XBHZEBBbddl+XVXMX9nDJV3IcEYFFN9rXh8uqql67Vv943cS3eu1rPTimaQgsuin72ga2bK6FDE/YDloOBBbf1MD6tJY7vpqKY5qGwKLb7uv6tmrtvsjRBPZw2Qa261ojxzQNgUU3eh3szcuH3IMVR2DRjV4He/PyYfPlwFX79553fjDHNA2BRZe90Py6urGzu1e+oIpjmobAohu90FzgW3IQ2EQEFt12X1fVPndhO1ZJSsz0jhBYdOPvKvXWVcQpy2EvBBYdfx9MisCim/ZC8/ahIK+DFUFg0Y32te7n/H7x2sX69e1bjx85pmkILLrRRY6zu+X5/esvh715qZFjmobAonvy98Gar894/e+DvXWpkWOahsCie/JCcxMYf6P5mAgsuuwebDH/u45uCGwqAovu+XOw5QEvN28IbCoCi+7pVcTX/0bztOWwBwKLjheapQgsOgKTIrDo+FpEKQKL7vm+HnaVnsAmIrDosn1d8K2zj4jAosv2lReaj4nAosv2lW+dfUwEFt3zfeWnqxwVgUWXXUU86CulCGwiAouO18GkCCw6ApMisOjyF5oPeq2ZY5qGwKJ7/m3b+Gr6oyKw6EZ/H6wra9d3nZ+4HPZCYNFtHyJ+0/1FFV5oPiYCiy67B+NvNB8TgUX35G801/9cHvY3LjmmaQgsuud/o/mw15kJbCICi47XwaQILDoCkyKw6CZ96+xJy2EPBBbdtG+dPWE57IPAopv4rbP3Xg57IbDo+NbZUgQWHd86W4rAouNbZ0sRWHR862wpAouO18GkCCy6J8/BCi6HvRBYdE+uIhZcDnshsOhGFzkO+6bZz5fDPggsOn74gxSBRcdFDikCi47ApAgsum5fy1zh2BDYVAQW3SiwEhfqOaZpCCw6ApMisOgITIrAonuXgVV+qLtKTuPkTs/7DEx9a04cTaI+lKAITMrRJOpDCWoIbPug5R18JYejm7V6gOQ0Tu70vMsXmh3drNUDJKdxcqeHwKQcTaI+lKAITMrRJOpDCYrApBxNoj6UoAhMytEk6kMJisCkHE2iPpSgCEzK0STqQwmKwKQcTaI+lKAITMrRJOpDCYrApBxNoj6UoAhMytEk6kMJisCkHE2iPpSgCEzK0STqQwmKwKQcTaI+lKAITMrRJOpDCYrApBxNoj6UoAhMytEk6kMJisCkHE2iPpSgCEzK0STqQwmKwKQcTaI+lKCm7ev69o1vjHMax+ToZq0eIDmNkzs9k/Z1WfXf2G1V7fgOb6dxTI5u1uoBktM4udMzZV9H3zhxeX5/8HI6jm7W6gGS0zi50zNlX0c/5GjXz5s9jWNydLNWD5CcxsmdHu7BpBxNoj6UoCY+B+vvwngOVoijSdSHEtS0fR2+xfaO+y8Cm8rRJOpDCYrXwaQcTaI+lKDK7Ov2R0cUWc6ao5u1eoDkNE7u9HAPJuVoEvWhBEVgUo4mUR9KUNNeB3vzp4idxjE5ulmrB0hO4+ROz6R9Xd++9dP5TuOYHN2s1QMkp3Fyp2fqF/telFxOxdHNWj1Achond3om7uuqunn110/jmBzdrNUDJKdxcqeHixxSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgS9aEERWBSjiZRH0pQBCblaBL1oQRFYFKOJlEfSlAEJuVoEvWhBEVgUo4mUR9KUAQm5WgSP9Q3j6IITIpJcqdxG9oXgUkxSe40bkP7IjApJsmdxm1oXwQmxSS507gN7YvApJgkdxq3oX0RmBST5E7jNrQvApNiktxp3Ib2RWBSTJI7jdvQvghMiklyp3Eb2heBSTFJ7jRuQ/siMCkmyZ3GbWhfBCbFJLnTuA3ta8qf5vH6qv7nqqqqD18KLKfj6MakHiBxNIn65lHU5MCW5/fNWzeHL6fj6MakHiBxNIn65lHU1MD6tNrMDlxOx9GNST1A4mgS9c2jqKmBPVy2ga2ePkjc6+/yyP5+UU59G0qYJPe+AzvgHszREaoHSJgk954Da/7vf7EZLndMXM7REaoHSJgk934D27SNnd1tVtWOvghsIibJvevADlnO0RGqB0iYJEdgM5dzdITqARImyRHYzOUcHaF6gIRJcgQ2czlHR6geIGGSHIHNXM7REaoHSJgkR2Azl3N0hOoBEibJEdjM5RwdoXqAhElyBDZzOUdHqB4gYZIcgc1cztERqgdImCRHYDOXc3SE6gESJskR2MzlHB2heoCESXIENnM5R0eoHiBhkhyBzVzO0RGqB0iYJEdgM5dzdITqARImyRHYzOUcHaF6gIRJcgQ2czlHR6geIGGSHIHNXM7REaoHSJgkR2Azl3N0hOoBEibJEdjM5RwdoXqAhElyBDZzOUdHqB4gYZIcgc1cztERqgdImCRHYDOXc3SE6gESJskR2MzlHB2heoCESXIENnM5R0eoHiBhkhyBzVzO0RGqB0iYJEdgM5dzdITqARImyRHYzOUcHaF6gIRJcgQ2czlHR6geIGGSHIHNXM7REaoHSJgkR2Azl3N0hOoBEibJEdjM5RwdoXqAhElyBDZzOUdHqB4gYZIcgc1cztERqgdImCRHYDOXc3SE6gESJskR2MzlHB2heoCESXIENnM5R0eoHiBhkhyBzVzO0RGqB0iYJEdgM5dzdITqARImyRHYzOUcHaF6gIRJcgQ2czlHR6geIGGSHIHNXM7REaoHSJgkR2Azl3N0hOoBEibJEdjM5RwdoXqAhElyBDZzOUdHqB4gYZIcgc1cztERqgdImCRHYDOXc3SE6gESJslVfhgXUXg5R0eoHiBhkpyjSYyLKLyco41TD5AwSc7RJMZFFF7O0capB0iYJOdoEuMiCi/naOPUAyRMknM0iXERhZdztHHqARImyTmaxLiIwss52jj1AAmT5BxNYlxE4eUcbZx6gIRJco4mMS6i8HKONk49QMIkOUeTGBdReDlHG6ceIGGSnKNJjIsovJyjjVMPkDBJztEkxkUUXs7RxqkHSJgk52gS4yIKL+do49QDJEySczSJcRGFl3O0ceoBEibJOZrEuIjCyznaOPUACZPkHE1iXETh5RxtnHqAhElyjiYxLqLwco42Tj1AwiQ5R5MYF1F4OUcbpx4gYZKco0mMiyi8nKONUw+QMEnO0STGRRReztHGqQdImCTnaBLjIgov52jj1AMkTJJzNIlxEYWXc7Rx6gESJsk5msS4iMLLOdo49QAJk+QcTWJcROHlHG2ceoCESXKOJjEuovByjjZOPUDCJDlHkxgXUXg5RxunHiBhkpyjSYyLKLyco41TD5AwSc7RJMZFFF7O0capB0iYJOdoEuMiCi/naOPUAyRMknM0iXERhZdztHHqARImyTmaxLiI3Pq2+6kTH77MWM7RxqkHSJgk52iSSXFMLyKzrK66N1bDG1OWc7Rx6gESJsk5mmRKHLtu9RM+dn2bslqe309eztHGqQdImCTnaJIJcey81U/42Mfrm+HN1dMHiXv9yDLNz1ADZpta00u3+gkfu8c9GICxic/B+ruwnc/BAIxNuxd8vO7uOrn/AvYS60e6A84QGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMHTEwLR/+xuYrMStvsAa/j7VG5gkxyQ5ApuJSXJMkiOwmZgkxyQ5ApuJSXJMkiOwmZgkxyQ5ApuJSXJMkiOwmZgkxyQ5ApuJSXJMkiOwmZgkxyS5EwsMeH8IDDBEYIAhAgMMERhgiMAAQwQGGCIwwBCBAYYIDDBEYIAhAgMMERhg6FiBrarq7O5In+u5x+vmW3BdjKfI3ziCh3/48vLnPvo03STyfVnf1p//6uXPK5qk+J4cKbBVPdlKVdjDr+6eTZG/cQSP1x++vD7EsabpJ1Hvy/q2/gzL5tas3pPtJMX35DiBrW+b/zssLo7yyTKr9uY0miJ/4xhTVFUzxytDHGuafhL5vjxc3tT/XH74It+TNEn5PTlOYNs/gMLy4tkU+Rv2Q6yqq/bwXhniSNMMk/jYl+ZOQb8nwyTl9+RIgbV3vCtRYItfdw+w0xT5G0eZowts9xDHm6b7HD72ZfHqVhx5kvJ7cpzAuoeuoidhj9fn9/XuXW2nyN84yiDt4bwyxPGmaSfxsS/1/amPPWknKb8n7yCwfoQPXwjsySTpTekkwzUO/Z5U6elVyT15Dw8RuxEub3iI+GSSjnZfVu21cQ970k3SKbkn7+EiRzfCr157Ln2UEZxc5Ng8DUy5L8vuVu1gT5ajvoruyTu4TN/tzOrVq8FHsXJymf5J6sp9WVY37b/1ezJMUn5P3sMLze2m1E9cxS809/cb6hdVt5Oo9+XhcrjXUO/JdpLie3KsL5VaCr9UarOoqu7/UGmK/I0j6B+YvTLEsabpJxHvy7L7MZLN5xHvyWiS0nvCF/sChggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECAwwRGGCIwABDBAYYIjDAEIEBhggMMERggCECU3q87r4pejX6oTirm/Gv34w+sPvZETeb+RbdZztkCUxDYGrdD3ZLUlPj/1i1TSzO7p7+8mSL5gekbh4uNT9H6l0iMLV9Aut+KtX69qJIYNqf5fvOEJjaENiq/fH2D5dVVVfQ/xCdPqemrP6Du19eth9b/9a/XFbdD2G9TI/8tu8cPuzxm+/7x6B9YN16/edo411yn2aEwNT6wJpHgY/X/V1Uc6Nfjh4Qbn9Ad/ue5peanxlXZ3XT/sb2BzN2jyO370wf9njddZUCa/89fI7mZ4Wtb3laZoTA1LrAuh9QWj92awp6/Oau+2mm6QFhcw/V3gm1v3zdfmzzY4ObN5bn9+MfzZ3emT6se6MxBLas3zn+HM8epqIcAlPrbtzdDweu/7m906punjzjWt9W/X1a9xSq/tj+Jwo3VY5+qvnwzvRh21VGgaXP0dyVLYe7OJRGYGrjwOoUuseA9f3Vf17ePL+ksWjujW7aZ2vt1fbutzYh1fVtn4P170wflge26J7HtZ+juY9bXG1gg8DUXrgHa/9j9BCx+8X0CDJdBNwG1lh0/94GNnxYFlhzkSN9jvoB6b99wyNEKwSm9sJzsPYp1Wr7EHG4irjq7sFGz8ya3zTcLaUa+3eOXqV+Ftiqv7bRXQtZ3/6aR4hmCEwtu4p41d95VVejq4hNAqv2PVfdxb/mDuvhsiml6u/T+nus9M70Yc8DaxNMn6N5QMojRDMEpvb0dbDm5anz+/r50dndOI32S6XagBbD62D1PdDD5T9ddk+9VsMvb0bvHD5sFNjoS6WGz5G91I2SCOyEDc/N3n7n68v8lkeIZgjshBUKbMkjRDsEdsKKBPZwySUOQwQGGCIwwBCBAYYIDDBEYIAhAgMMERhgiMAAQwQGGCIwwBCBAYYIDDBEYIAhAgMMERhgiMAAQwQGGCIwwBCBAYYIDDBEYIChvwHQfocIgw3IWQAAAABJRU5ErkJggg==)

```r
hist_dailySteps
```

```
$breaks
[1]     0  5000 10000 15000 20000 25000

$counts
[1]  5 12 28  6  2

$density
[1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06

$mids
[1]  2500  7500 12500 17500 22500

$xname
[1] "stepsPerDay$steps"

$equidist
[1] TRUE

attr(,"class")
[1] "histogram"
```

**3. Calculate and report the mean and median of the total number of steps taken per day**


```r
meanDaily <- mean(stepsPerDay$steps)
meanDaily

medianDaily <- median(stepsPerDay$steps)
medianDaily
```

The mean amount of daily steps is 1.0766189 &times; 10<sup>4</sup>.
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
# A tibble: 288 x 2
   time      steps
   <times>   <dbl>
 1 00:00:00 1.72  
 2 00:05:00 0.340 
 3 00:10:00 0.132 
 4 00:15:00 0.151 
 5 00:20:00 0.0755
 6 00:25:00 2.09  
 7 00:30:00 0.528 
 8 00:35:00 0.868 
 9 00:40:00 0     
10 00:45:00 1.47  
# ... with 278 more rows
# i Use `print(n = ...)` to see more rows
```

```r
#plot(x = dailyPattern$time, y = dailyPattern$steps, type = 'l', xlab = "Start time of Interval" , ylab = "Mean Number of Steps accross days", main = "Daily Pattern")
check_and_install_package("ggthemes")
if(!"ggthemes" %in% (.packages())){
        library("ggthemes")
    }

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

![plot of chunk dailypattern](data:image/png;base64,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)




**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
maxInterval <- dailyPattern %>%
        filter(steps == max(steps)) 
maxInterval
```

```
# A tibble: 1 x 2
  time     steps
  <times>  <dbl>
1 08:35:00  206.
```

## Imputing missing values


**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

This was already mostly done above but will be repeated here:


```r
summary(data.raw)[7] #total amount of NA's
```

```
[1] "NA's   :2304  "
```

```r
colSums(is.na(data.raw)) #check for each column including text columns
```

```
   steps     date interval     time 
    2304        0        0        0 
```

```r
#calculate NA's as percentage of all observations
100*parse_number(summary(data.raw)[7])/dim(data.raw)[1]
```

```
[1] 13.11475
```
So 13.1% of the observations have missing step data.Are they mostly in the time intervals that we are not expecting many steps?


```r
#install lubridate package
PG <- "lubridate"
check_and_install_package(PG)
check_and_load_package(PG)


missingValsPerInterval <- data.raw %>%
        group_by(time) %>%
        dplyr::summarise(count_NA = sum(is.na(steps)))
missingValsPerInterval
```

```
# A tibble: 288 x 2
   time     count_NA
   <times>     <int>
 1 00:00:00        8
 2 00:05:00        8
 3 00:10:00        8
 4 00:15:00        8
 5 00:20:00        8
 6 00:25:00        8
 7 00:30:00        8
 8 00:35:00        8
 9 00:40:00        8
10 00:45:00        8
# ... with 278 more rows
# i Use `print(n = ...)` to see more rows
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
# A tibble: 8 x 3
  date       countMissingIntervals weekday
  <date>                     <int> <ord>  
1 2012-10-01                   288 Mon    
2 2012-10-08                   288 Mon    
3 2012-11-01                   288 Thu    
4 2012-11-04                   288 Sun    
5 2012-11-09                   288 Fri    
6 2012-11-10                   288 Sat    
7 2012-11-14                   288 Wed    
8 2012-11-30                   288 Fri    
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
Don't know how to automatically pick scale for object of type times. Defaulting to continuous.
```

![plot of chunk activityByWeekday](data:image/png;base64,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)

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
`summarise()` has grouped output by 'weekday'. You can override using the `.groups` argument.
```

```r
#visualise the mean steps matrix
visWeekDay <- dataToImpute %>%
        spread(weekday, steps) %>% ##spread steps by day
        select(-interval) #%>%
        #heatmap(as.matrix())

heatmap(as.matrix(visWeekDay))
```

![plot of chunk example-base-factor-22](data:image/png;base64,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)

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

summary(data_missingImputed)
```

```
     steps             date               interval           time         
 Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :00:00:00  
 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:05:58:45  
 Median :  0.00   Median :2012-10-31   Median :1177.5   Median :11:57:30  
 Mean   : 37.57   Mean   :2012-10-31   Mean   :1177.5   Mean   :11:57:30  
 3rd Qu.: 19.04   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.:17:56:15  
 Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :23:55:00  
```

```r
comparedf(data.raw, data_missingImputed)
```

```
Compare Object

Function Call: 
comparedf(x = data.raw, y = data_missingImputed)

Shared: 4 non-by variables and 17568 observations.
Not shared: 0 variables and 0 observations.

Differences found in 0/3 variables compared.
0 variables compared have non-identical attributes.
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
# A tibble: 61 x 2
   date        steps
   <date>      <dbl>
 1 2012-10-01  9975.
 2 2012-10-02   126 
 3 2012-10-03 11352 
 4 2012-10-04 12116 
 5 2012-10-05 13294 
 6 2012-10-06 15420 
 7 2012-10-07 11015 
 8 2012-10-08  9975.
 9 2012-10-09 12811 
10 2012-10-10  9900 
# ... with 51 more rows
# i Use `print(n = ...)` to see more rows
```

```r
#generate histogram - I took some tips here: #https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/

hist_dailySteps2 <- hist(stepsPerDay2$steps, xlab = "Total Steps per Day", main = "Histogram of Daily Steps - Missing vals Imputed")
```

![plot of chunk TotalSteps2](data:image/png;base64,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)

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

![plot of chunk TotalSteps2](data:image/png;base64,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)


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

The mean amount of daily steps is 1.082121 &times; 10<sup>4</sup> vs 1.0766189 &times; 10<sup>4</sup> before imputation. This is a 0.5110529% increase.
The median amount of daily steps is 1.1015 &times; 10<sup>4</sup> vs 10765 before imputation.This is a 2.3223409% increase.

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
`summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
data_dayTypePlot
```

```
# A tibble: 576 x 3
# Groups:   interval [288]
   interval dayType              steps
      <int> <fct>                <dbl>
 1        0 average weekday     2.31  
 2        0 average weekend day 0     
 3        5 average weekday     0.45  
 4        5 average weekend day 0     
 5       10 average weekday     0.175 
 6       10 average weekend day 0     
 7       15 average weekday     0.2   
 8       15 average weekend day 0     
 9       20 average weekday     0.0889
10       20 average weekend day 0     
# ... with 566 more rows
# i Use `print(n = ...)` to see more rows
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

![plot of chunk example-base-factor-24](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAA2AAAAKgCAMAAADK0+6WAAABR1BMVEUAAAAAAC4AADoAAFIAAGYAMy4AM3MAOjoAOmYAOpAAXJEAZpAAZrYAgP86AAA6AC46ADo6AFI6AGY6My46M3M6OgA6Ojo6OmY6OpA6ZmY6ZpA6ZrY6gJE6gK86kJA6kLY6kNtmAABmAC5mADpmAGZmOgBmOjpmXABmXFJmZgBmZjpmZmZmkLZmkNtmo5Fmo8xmtrZmtttmtv+QMwCQOgCQOjqQZgCQZjqQgFKQkDqQkGaQkLaQo3OQtpCQttuQtv+QxZGQxcyQ29uQ2/+2XAC2ZgC2Zjq2ZpC2gC62kDq2kGa2o1K2xXO225C227a229u22/+25ZG25cy2/7a2/9u2///bgC7bkDrbkGbbtmbbtpDbtrbb25Db27bb29vb5ZHb5czb////o1L/tmb/xXP/25D/27b/5ZH/5a//5cz//7b//9v///9MBi7wAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAgAElEQVR4nO2d/X/cyH2YIVq5HFWCPp99zV0TNVzpnLht0qt5dO26jhuVWjdp3i5OyG18dWOK4um0xP//czGDd2DwujODmdnn+UjcXQDzxXex8+wMBlggSgDAGNHaCQCEDIIBGATBAAyCYAAGQTAAgyAYgEEQDMAgCAZgkKMQ7D6Knt5ojPf4vz+JopN/91X24n9ND73tzWO/iU7vkuThPDq5TmNe9SxYBUgXEQX6mRhxKCmj8WYs6DNHIdg2iqJLfeHSyiU5ETG/+eGMWtJfpfIKm34VRBeyMp+NBBgVbGLEyfVcdzwEC4a0MkS91WEBuyhH1I9ZtWRg4Z1sGLZZpmkzcTE3wMKI02PqjodgoXAfnfzH7LPMP9J72aA9/ipthP5QtAJp1fmLH0ZPv0p+88O0+vzBtSgk5n54nRWolpRso5Mfp03XlYiyzUWrFkmL/P2fpGVlB/L/1PqS1bx0QtoenCVlKtmTi6xtTKPlU3+TLvv7f1bPUOaTfmE8/SprwdLp/1Mm2k5ZHbH2RhoJ5zGLNLvrXRqvsY3rmZ5HH/4SwQJhG53+3+wLN6+4W1E3ZbuWfuI3RZN0enefd/2ui27gySd5ZSmWzOPJiiG/xHPBaotsoyrKrmrp8pLlhF1DeRntLF3Tyc9FwV29tOgJFhmKxdPU0rmFYOqU1RFrWTYSLmLmKNa7MF5nG9czjTTvGbvJEQiWfsoX+R5L1m6ICbIefJX3dNIP//Sr5P+lc//NXT4pde3s7nEr60BtSUm6+Mmf/UP+Ylt0FItFRJmvkn+JZG1Mg4inF+XCYkVyXrrwZX1XRmZ4Hz39u0zbU5lIplCVoVjPP19ljUchWNpUZsvUU1ZHbGRZf5rHzFCtd2G85jZuZforBAuD+6pNyNqN4tv3oppQVq/figHCs8Ib0dG5qS8pyb+AP/xlvUNULrIt69ZNWrU+/Id6Ktssi/w7/qzWQ8wm7qIzMT2bt621cUWG6Yv/kPlaCnaZty31lNURa1k2Ey5iVsXa610Wr7uNezINmCMQrGgOxGct2o2saqbPyj5Lbl/y+NNs0lk5RpdrUvVuMv7lk0aHqBEsr58iZmZitjeTp1LOKypjORSYuvbjqzTJdPo/bbK9ndO7auFs5dsyj3If7DpvB+spqyPWsmwm3HhvyvUui9fdxkWmZ0kj04AJX7DyUy76iGUfsC2Y8OHD//7bTd65E4VFHagvWfL425+fV01dfZH6QMo3P8ym/o+81LZWd4Xr+Vd8kee/36TT0wb3L86zJlamIAM1BCveSVOwesrqiLUsmwlX+1lJol7vsnjdbdyTacCEL1g5qC47O+m373+TH3W2D1QsUnzhXiiag/qSSVIbod7mHaKbRrB6K5Uk3/z8k1rtrc8TrtfrrxwQyLqb/1aOS/a0YKd/3dgHG2jB2hFrWTYTLmJK+luw2fG627gn04AJXrByxCo7FCbbM/Hx1o+U5tXovqgVF+19sLN6xPT1yZ+nEX5zXgpWX6S2D5Zn8F9re0W1eTsx4lc7WJwfYZIJi8nNfaFCMJlPOrsjmGLPphWxlmUz4SJm9bq93mXxutu4L9NwCV6wbLe6OrenPKtDHs36ZlNV02z4TBzeOmuPIpZLSnZ1ZYvDr+UitVHENMgf34l+YtWClfNy12tHa3f5y+KxOZpXCZZ9EXQFa48idiM2sqw9rY68JT3rXRivs437Mg2X4AUrx8HyJ+V5ifkxmuyAU7kPVnijOA5WVrXGYZz7qHYcrDjVoZj5q9bOW32eDFOvYfdRmWGWcuN4VE0w6VZXsPZxsG7EWpaNhGsd0p71LozX2cZ5m9bYDmETumDVSanZF3Ot2/LNT8/lSRVVNRKjiB/+cleevfFpduZEbck8qBxFzEYH5ekTX9UWEYeBflosLhf8A8WZHIL75glcRZdJdEGzWi3PqPjzJGkJJnuxXcEaKasj1t5II+Gb5rlP3fUujdfexkWmf30e/f6P2QeDbX0naWqRqdXmPmqOnuhhQcpgDgRTk3Zi/rDagZtXcqJg6b6Z1m/w5SmDORBMTXEIp95Dmsb0n1c1hjgOZ3nKYA4E6+GbPzlvnAg/memCnfzx/LSGWJwymAPBAAyCYAAGQTAAgyAYgEEQDMAgCAZgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBEAzAIAgGYBAEAzAIggEYBMEADIJgAAZBMACDIBiAQRAMwCAIBmAQBAMwCIIBGATBAAyCYAAGQTAAg6wrmBt6u5EFaTRwI43Ds0AwV7IgjQZupIFgOnAjC9Jo4EYaCKYDN7IgjQZupIFgOnAjC9Jo4EYaCKYDN7IgjQZupIFgOnAjC9Jo4EYaDgoWAYTGIT5oM6sI+B4gLBAMwCAIBmAQBAMwCILB+3eb50Oz3zx51fMCRkEwQDCDIBggmEEQzCNeR1H0xfu351+8zyr6bfo6VePdi59F3/l1Pjdb7MnPhAf5/JRvv3yWFhGzX3/wu2p6EUAIdhs9r034z5ss2G0R6339Rb6q18/ExGdrbApvQDB/EPX59skrKYsQJX2e2vY81SGVppibPkllexMJ//L5glvp1bPUtOfV9CpAOlEYU03Ii5ex5PqLF8Wq3qQTvv3yixU3ifsgmDe8e5HWc9F83ab1OnUi69illVw+KedmDdzrJ6/K+aKwaPBe/+iD37397qtmufyJbN2ac9JAZSwRogpcrOrdJl3dd+kyDoFgXvFGdM1EvU6FyfaG0oouXldzM6Pq88WsdJl3L37xvV+nc8vptQDfl13GZsT0TxlLRm+8KPqIommEfhDMH9JdoO/87XlWr9Me4pv8ZLdcsGLubSlYMV8Wfv387Q/+9cWrtGg5vRbgyS9EQ9SMmP65rTtVvSgTSZ17PTg+AgjmDbItepvV63/MGrFshtShnNtqwQpuP/irVMs/+rJWrhbguRz8aEbsbcGqRN69+MsX9BAHQTBvkBU831f6kdwNy3uG8kk5N+sT3op9sPrww9vv/enz97e/lzZUzXLZk+flPlU1J/1TxpIRihdVIt9++X16iMMgmDdkCsidpdeRHCCXY4a5SdXcxihiMUKRDQy+iYQP5fQqQF6sEVH8kWOFxShi8aKWyG1ED3EYBPMHcRjqlRTmTbZjJY5alU1ZNfd1OvVvRCuTz88QAuWD9uX0MoA89vWsGVH+UR4Hq1bFGOIYCBYmb0qxjPL2B/QQh0Gw0MhGH4ZPftLGLT3EEdwSDCA0DvFBm1mmAgKsDIIBGATBAAzihmAa+qsALnJI1aYFAxjBjRbMVECAlUEwAIMgGIBBEAzAIAgGYBAEAzAIggEYBMEADIJgAAZBMACDIBiAQRAMwCAIBmAQBAMwCIIBGATBAAyCYAAGQTAAgyAYgEEQDMAgCAZgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBEAzAIAgGYBAEAzAIggEYBMEADIJgAAZBMACDIBiAQRAMwCAIBmAQBAMwCIIBGATBAAyCYAAGadfph/MoukySx6vo6U35MLGs9mQAfKdVp/eby+T+5DrZnol/xcO0stqTAfCeVp2+P71Lm63L/cvr5OHjm/xhYtlDssjRFhDADRRVO23FhFWpXflDb1ntyegOCLAyijq9O71DMAAtdOv07ulNgmAAWujU6d1JKhSCAWihXad3clje9iCHqYAAK9Oq0w8fZe0Vw/QAOmjV6a0cUrxM9ht5hDl/mFZWezIA3sOpUgAGQTAAgyAYgEEQDMAgCAZgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBEAzAIAgGYBAEAzAIggEYBMH8Il47AZgHgvkFgnkGgvkFgnkGgvkFgnkGgvkFgnkGgvkFgnkGgvkFgnkGgvkFgnkGgnlFjGCegWBegWC+gWBegWC+gWBegWC+gWBegWC+gWBegWC+gWAO07UJwXwDwRwm7viEYL6BYA4Td9owBPMNBHMYhWC+n8rhefrzQTCHCU+w42uBEcxhEMx/unX64eOb9M95FJ3eJY9X3KN5RYITzPP0l9Cp0/uNUOr+TL7Ynol/k8tqT+bIQTD/adfpXfSZaMG2F+LF/uV11qBNK6s9mWMnNMFiBjmSr++EUY8/uRYvxFMh2cSyB2SRoy1gGCCY/3SrdmbVJ9HJtT3BTAX0HBuC2azxRylYZ4qw6uGj62T/+Q2CrQuC+U/PKGIid8AQbF0QzH8GBWOQY126FRLBfEMt2P3TtJv46R3D9OuCYP7T04LtInmEeb/hQPOKIJj/cKqUwyCY/yCYu8RJWIIp3k/4IJi72BDM5tm3CGaxrJ2AfuOQYFrWimAWy9oJ6DfdCql/JwbBDINg7oJgAYBg7mJesHiyYDpWi2AWy9oJ6DcIFgAI5i4WBOteGK4nE12CHZ1hCOYuCBYACOYu3Qq5VhdRz3oRzGJZOwH9xoZg0+JpWW9c+3s8IJi7WBFsUkAEWwyCOUvsuWDt/TsEs1nWTkCvcUuw+StuJ4tgNsvaCeg1cbdG+iZYvVDceDgaEMxZECwEEMxZQhCsVgrBrJa1E9BnioO76wkWN5/MXXOrFIJZLWsnoM/Ercfiuc4aakGw9vtAMDtl7QT0mVAEa5mliBK0cwjmKu2+VfEUwbwCwVzFOcHmrrlZXNUg51MQzEBZOwF9xnfBetJGMDtl7QT0mYAEq49+IpidsnYC+kxIgiVxv2B+3/JsFARzlc4h2mRtweatGsEkCOYoseqpZsEaow/duRoFq4L1CRaqZQjmKH4LFncW7osRj6XhOQjmKKEJ1o7VXrK/hN8gmKMgWBggmKPEqucrCzZ93ZMFi8v/gQ52IJij9Aumrx4imHkQzFEcEkx1wGA88pBgceclgmkuayegx6gEy2qj94LFzW+MYlqYZ3QgmKM4Kdhkw+YKFh+RYPIm6I9X8u7n+cPkstqTOV5ixYu4O+fwdbghWHI0gu03QqntmfhXPEwtqz2ZI2YFwVpV3Ixgci2tyLHunUuHaNfpXfRZ2oLtX16Llix/mFpWezLHzCLBZlbR7oBDHFf3g4ibq521gpmC6e77OkS7Tn99J4x6yCTLH6aWPSCLHG0B/WcdwfInUjKVYFPXgGAF3aq9hmCmAnpMv2ADlVyTYEmxW6SaPjXyoGCtNzTwzeG7depBDgRbHWuCtUcciueHCdZ/VCuumrC4Wro3NoIZTeZ4iRWvAhEsmSGY94ef1YIxyLE6jgo2cRVDgiVdwfpDLxbMFS97joMxTL82bggWt6frFKy9YrVgC1VxXLD9Rh5hzh8ml9WezPFiX7CWRwimBU6VcpSVBUuqgfpOJhPWMk8wZQJzVqcu6AQI5iix4qULgk3bKxoQLOmJi2Bay9oJ6DHLBJtXrQYFk+EWCjby45PpgimXm4Qrh60RzFEsCNYKN12w0bUgWAmCOYq7gk1owkZ/Pqns+SGYxrJ2AvqLcgBArUJ9qTm1qh1tomBT+ojrC+bMAWoEcxNl/0mrYO1BcqVgKhN0CKacj2Aay9oJ6C+rC5aHMyZYj02KSQimDQST9I4AjAk2p1bFnWfKJkU9TD62Gl2CxerJE1B+NawBgrlHEIKNLIJghsvaCegnNgSLu0/1CRZPEGzSNATTC4JJ4r4xtgmCTdwLixXPfRNsZJhyyvE6CyCYe/QJ1u3VdRaZ+q2tEExd0JRgY0nVJiCYNhBMoh4i0ClYrHjhm2DDaxB5I5jxgH4SK2uPRsFi1St9gsU2BBvRB8GsBPQTTwTrXZMFwdS96GZRBDMf0E8OEmyCYbHy5fqCTU4sHn+j2TZ0wDAEcw+1YPVpWgUb7on1j/f1C7Z0gKFfsMagzOA3QlV2rJGzA4K5h2HB5u3qrC9Y0xTFXct6QiGY+YB+YlYwVeezv9yQYH1rWnyQV7lv2DClvk4EWz+gn/SMVTgkWJaifcHi/qWVoRDMcEAv6dNEj2AzhywWCLa88dAn2JTdNDsgmHP01vcRwXqu1dQbZHyFCYIdDII5x2LBJoxeaxSsdxB8ee+sxx8E0wWCCZa3YP3zVEHGV5gg2MEgmHNMaIeOUbC4f4HeSAhmOKCXTKgVLgt2QN1GMH1l7QT0Ep2CxZ0T8nwRLG48VQ2cDKxh0skedkAw59AjWHm3ZTOCdW/ypU5jFr2C5b8kHX8rVXqLk9AMgjnHQsHi2mPcf8xMVwtWVngEGwTBnEOHYP3L6hEsqbpsPX3QAwVrvoPZgo0vYgsEcw5PBCtXhWBDIJhz+CZYa1ilf/FREExfWTsBvcSAYD01tz7JecF628qhQAhmNqCXLBMsDkqwVsj6DW2nrArBLAX0Ev2CjfyKakiwwV9Oxona56UcIpj6hXuCPZxH0eld8ng1dPfznrLakzlKptSJ7jJNhdr1c1yw3rV6IlisTqK742abdp2+P5MP2zPxb2ZZ7ckcJSYE66u61aSlgnVaPwOClXd5GRRM3Q9e/cI37Tq9vRB/9y+vk4ePx5owfT5EOdoCeswiwfrrWtK8xFnf8MhBgo3s402mtKFtUtyY21m87Xk9nbWv3daq2o8/uRYPQi4h2UhZ7cnoDugj5gTrE0mnYAdV5z7BkjHBWsUaW2NtwZov9y8/iU6uEWxFlgjW3sO3LZimUYVyb1EZpStY3DZK4adjgj18dJ3sP79BsPVYIFhrj6W7B+OLYINXPehk0hWs22N1bpAjkTtgCLYak+rDPMGK/8cp2MqGqQWzPchhKqCHTKsOQ9/RKsGkC8sEG86ip5O2jLmCKdaqKO+UYPdPb5KHT+8Ypl+N+YL117vmQr2CdfbapubTHsYbWXwUDYIpvi2cEizZRfII837DgeZVmFgbYuXTYoItwbqXajysMsfDrWl3ukrrzptxSzBbZe0E9A8EO1QwdRd5NRDMKaZWhnbFbs7rEaxXpKHTHSwLNiS7+m1158QIZiugb0yuC0Ojdj2Cxf2VV4dgQ3tPc1giWHfnbKSYRRDMJWYLph4UnC3YwHrtCzYnxpR2E8HMBfSM6VVhtmDF4WbNgrVfaBBs1qkXRyjYfnMhhiJP7ywnEwKzBZvR4fNGsAVLH5Vg29O7h/MpR9E0JxMAM2rCYMVS+jI0gOCSYPOYNLSyqmG6BdtvLpP76FIesbaajP/M7hrNrjh9twVfLFhnUQRrY0KwbSrXDsHmYl6wPpWGAs0SzHpVPj7B0r7hfnN6t9+cWU7Ge2ZVA8VFzAytGsEOw8AgR3Ry/Xh1ZjsZ35m5c++iYCv89mqGYHF5tX6rMEzvCK4KNiuOo4LVzEKwI2XmBx/3jVfYWLmFQPNWOHv41SL6BUv7iAsPgyHY9MW1CqaNdQSbf2TaItoFuz8RP4TenYz9HFp3Mv6ypLu3xt6Ek8w9+Oa9YI9Xl/Jxu6QNOzbB8u/fBY0RguUcnWDFlTw40DxMXP8zX5YVjum6ydEJVrRgO1qwIfJLaQ6dvzRcHsEy5m4I7wWT50mJKw9cWk7GK4prTyz2BMFy5vaVvRdMjiHmzO4lHolg1YUKDxEMcmaff2/1rGSOgxml77x2BFsLBAsKxedYOza6aACxLy5Mwn/B0k7i6V12kxabybhJ53Ns3ScPwWwT5/0GW1vQxIHmnTibfolh4QsW158d8EEj2FJ8F+zx6kIO0fN7MEn/FcTihUfA4DB8F0z84FIIxoFmSUOg5gvL+wKQUQhma8ubasE4VUrSf3dkBFuHuNjznfQjsmlThzC0D8aB5ozi5qdJ57NBsHUoBRvZ9PMvrqDG0M9VFp1Mf4yCgWXiuPGp9C+nVMwFwVwKuDb5V6WqS4Jg61AJNrz91W3c+oJxNn2dcixeccack7+YPAJmCDZwGHMypgRjmF5QDAir+hv8omsdagdHhj6A3MF2v2NlwbbVmb5BHmge3byqfmDPqDB6rUbcehxYpLnMgl6HqRZMW0CnGNs5brdUHEp2mimCNRZyQLCDWBDQRvWtzh/sFazW5+g/9AWOMUmw1l0G536k7Tr9eDX9Z1wKHx7OL8RvLu0N0xuowq0DwrUxwGJ/qrdMu+OOYG7T//m0OyLlk0MF2864MUrXh4fzy+w3zfLJbOYL1nc3gwUrL8s2vryK8aZ81Fba1h6eiIvf/sftGAjmNmNfle3lFpwa0KrTYhfq4eOJTZhikONM/BFNoKXbF1UHmhqXby1aEvXRwrGAteLF1Forlv+tXyw2juvlaikgmNuU9aX8PNVjUvUKcZhgQq7J4xQdH+SPVLLr0s87DlYMPc4oUtbxuP674GJLVdur3HrTSCp/GitTjqrXrO7O5spq7tO6Xn1Robpfy/XF5n2srap9qGCXuWUcaAYQaG7BLvP9MC7bBiDQKpjc85JXzV52/yIEg9DQO8hxf3Itbr+XD3QcmgyA9+gdpheHwFK/Hs65uwqAoF2n95uDDjTrTQbAd3w/VQrAaRAMwCAIBmAQBAMwiFbB9pvLsH8PBjATzYJdIBhADb1dxN0BNwc7MBkAF+GSAQAGcWuQAyA0DvFBOfXhPFp8yYD3AGFh4Cbo4udgC69Nv/bWANCMiburCJb9HmztrQGgGRP3BxMs+0Xz2lsDQDO0YNDm3eZ57dWbJ69WyyQA2AeDNgimEUYRoQ2CacSt42Brbw2/eJ1+k33x/u35F+8zDW7T16ka7178LPrOr/O52WJPfiYsyeenfPvls7SImP36g99V04sAQrDbSP7Ji+bBXj8TSz1b7R17CIJ5i6jtt09eSVmEKOnz1LbnqR+pNMXc9Ekq25tI+JfPF9xKr56lpj2vplcB0onSp6JoEexNOuHbL79Y9337BYL5yrsXqT2i+bpNa33qRNaxSxWQT8q5WQP3+smrcr4oLBq81z/64Hdvv/uqWS5/Ilu3qmgR7N0mDfhduowzQDCfeSM6bqLWp8Jk+0qpBuJ1NTczqj5fzEqXeffiF9/7dTq3nF4L8P2okjHfB3tT9BFF4weTQTBvSXeQvvO351mtT3uIb/Jz33LBirm3pWDFfFn49fO3P/jXF6/SouX0WoAnvxDNVFm0WlXq3OvnQ0lBC1PHwZYFXHtr+IRsi95mtf4fs0YsmyEFK+e2WrCC2w/+KtXyj76slasFeC4HP8qi1arevfjLF/QQ52DqTI5lAdfeGj4hq7/cV3q3+ZHcDct7hvJJOTfrE96KfbD64MTb7/3p8/e3v5c2VM1y2ZPntd23tGi1qm+//D49xFnoP9C86Jq+ecC1t4ZPZIMOcmfpdSSHz+WYYW5SNbcxiijmy9JpN/BVOlnYUk6vAuTF5MBhlLVg+apuI3qIs9DfguU9ec5FNI04SPVKCvMm27ESh7HKpqya+zqd+jeiDcrnZwiB8kH7cnoZQB4Me1YdB6uCMYY4EwY5joI3pVgH8vYH9BBngWCBk41NbHR17G7pIc5Dv2BpJ/H0brtoLNHkL7cBVkG3YPcn17vTu+w2l1oCAniMieNg4rdgOy7bBmDgsm2bSymY+ZugA3jAIVV7qAXbco9mAAPXRcz2wRb+ovmAZABcxMgo4uJfNB+QDICLuHUcTHdAgJVBMACDGOoiLhniQDAIDwOXbRPDG7tFO2EIBqFh6geXDNMDJGYONAuWXTr7gGQAXER/C3YmH5ddOvuAZABcxNCls+/ZBwNItN8EvTpLny7iocRrJwAa4DiYsyBYCCCYsyBYCGgXrOgm0kU8FAQLAf2jiMtO4ugNeLwgWAi4deHR5UUDBMFCwK1LZy8vGiAIFgLa98EePlr0U7D+gEcLgoWAfsHOGeTQQoxhIWDqVCltAY8WBAsCBjlcBcGCgEEOV0GwIGCQw1UQLAjcun3RAckEB4IFAeciugqCBQGCuQqCBQFdRFdBsCAw1IItu1MzgtVAsCAw1UXcnmkOeHQgWBCYEoyrSh0KggWBKcG4Ad+hIFgQGBJsvznTG/D4QLAgMDWKuOh3zQhWA8GCgONgroJgQYBgjhLzi8sg4MKjjoJgYWCoBdtGS361gmAVCBYGRgRLW7JFP7tEsAoECwMTgt0v6h8OBDxGECwMDAi2W3gDWQSrg2BhoF2wx6tFu1/9AY+SbIwew/zHwIHmnt0vcT23Symg6EDmD/qSCQsECwXNgvXvfonLTYnb8m3P5Jn2+YO+ZMICwUJBr2C76Kxv4ft0x+zx6nL/8jp5+Pgmf9CYTFggWChYPdCctmLCqtSu/KGMlHNAMmGBYKFwSNWeXWh3eqcWbGnAUIkRLBRsnosofiOGYFMoBUMx37Eo2O4kFQrBpoBgwWBPsOw3zgxyTAHBgsGaYMUltRmmn0BuVhwjmO9YE2wrx1IuxUijaMryB33JBAWCBQM/uHQRBAsGBHMRBAsGBHORQjAGObwHwVwEwYIBwRyk+C0YvwnzHwRzkJpYCOY5COYgsfIp+AiCOQiChQOCOQiChQOCOQiChQOCOQiChQOCOQiChQOCOUhdMAzzGwRzkIZUnJDoNQjmIG2jcMxfEMxBFDrhmKcgmIOoXUIxH0EwB+kxCcE8BMEcBMHCAcEcBMHCAcEcBMHCAcEcBMHCAcEcBMHCAcEcBMHCAcEcBMHCAcEcBMHCAcEcBMHCAcHco1ckDPMPBHMPBAsIBHMPBAsIBHMPBAsIBHMPBAsIBHMPBAsIBHMPBAsIBHMPBAsIm4I9fHyT/jmPotO75PGKezT3gWABYVGw/UYodX8mX2zPxD+NyQQEggWEPcF20WeiBdteiBf7l9dZg6YtmYBAsICwJ9jXd8Kox59cixfiqZBMXzIBgWABYXsfbP/yk+jkui1YlHNAMgGBYAFxSNVeItjDR9fJ/vMbWrABECwgrI8iJnIHDMEGQLCAWEkwBjkGQLCAsC3Y/dO0m/jpHcP0AyBYQFhvwXaRPMK833CguQ8ECwhOlXIPBAsIBHMPBAsIBHOPfo8wzDsQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLHfVEwsAAA2mSURBVCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QLCAQzD0QTImfbx7B3APBlPj55hHMPRBMiZ9vHsHcA8GU+PnmEcw9EEyJn28ewdwDwZT4+eYRzD0QTImfbx7B3GOgJvlZyfTg53tHMPdAMCV+vncEcw8EUxJ7+eYRzD0QTAmCHQyCSRBMCYIdDIIJhuqRl3VMEwh2MAgmQDAlsZ9vHsGcY0QwL6uZBhBsFHkT9Mcreffz/EFfMuGAYEoQbIz9Rii1PRP/igd9yYQDgilBsBF20WdpC7Z/eS1asvxBYzLhgGBKEGyEr++EUQ+ZZPlDGSnngGTCYVgwP8fSNOCrYAdU7SX7YGrBFgYMEwRT4qtgFssi2CRGBPOznh2Op71jBHMOBFOCYKM8MMgxhUAE050mgo0ijWKYfowwBNO+r4hgo0jB9ht5hDl/0JdMOAQimO4848RLwzhVyjkQrCcggh0KggkGa5E3HSUzgvnx3usgmHMgmDpe9uDHu69AMOdAMHW84okf778AwZwDwdTxqqc+OYZgzjFWe/yoXZoFawXzRzEEW5tOVQlFMK2JdreSH5sBwVYHwSaGG53iJAi2NusKZqya6hOs5wAYgjkQ0AfaexdhCLbwsLCqCIJpA8HErgWCTQqFYA4E9IHG3vqUU6G07tr4IVifdx6AYGvTEmy8zh+ZYHHf3pwfhiHY2tTr+KTz7fQOzhmqpssEU713BNNHeIJNqAS1ShW3Xi+OORkEMw2CGWW8EtSqePYEwbrTEEwPxy3Y5PPF9dUsc2c2xuWfWaUQTFdZOwHXZkIDgWD1UmrB1HEQbP2AazMkWNxapNVTHIyqCy8E6/vKQbD1A67NJMHalRHBWpEQTBfHJVg+ptEZEDhWwWJlOv1HBhFs/YBrM1mwWZUFwapVOA+CmSTudyyu1EKwvAiC6StrJ+DKZH2/CYLNrYm6MCzY3O+NPsGGVuE6CGaQYcGqGjWzqoQpWDxXMD8MQzCDSHn6OzjqGjUtrB6WCT4xcu1hUgkE01nWTsCVyXZE+gVbeJ1eBJsdfD0QzCD5nr6yu+SCYHFiWrAZofsFG1uH0yCYQRpf462WrNg/Q7BiSQTTWdZOwJVpCtasELlgSyqJB4LFimejRZSn9SKYRgIXrFEjlM3arLCHUxysckGwvt1BBNNIYIJV3aRuU1G4Z12w9g+oTQs2NfgCwbwwDMHM0RWsqhLz91G6cRcmVR04QDDzIJg5aoKVtbio3CsKJrOodDct2MToPYINFkawtQNapzOOkT3WfzQYt+YduJKlZeMip4UjLRNX01jlSAkE01jWTkDrtK/Blj82KnHcnLdgJYtS61bg2I5g097rEsF8MMy6YA/nUXR6lzxehXgT9GZ3q/7V3bk422FrWVyuWzbudFs1gGAF1gW7P5MP2zPxT2MyTqAcKewIdnCjsbBwO412MqYES6bE71kGweayvRB/9y+vk4eP203YsQh2aMVYKphS65pg+uqrSrDh8Aimp+zjT67Fg5BLSFZEyjkgGSdQD8Uf3CdUrmZBMWU5C4IVQ5cTCoQn2AFVe0mh/ctPopPrtmAHBHSKnhFCNwTrOe+xOiFy4omRUxZCsALbLdjDR9fJ/vOboAWbtM+hYTV6Ss0VbFI7p/JkkmBKNSevxkHWGKZPzQpbsFZLZmw1WkrVBZsQeZKFnUUQzGbZ1KyABznKJ+Y+fa2CVUfBpwg2SULlwQAEs1L2/ulN8vDpXcDD9OUzXwQrZylyVnb1lqxq7KdvywQz/EWmA+st2C6SR5j3m0APNFfPfBRspH5P7foqlui9h0OrBILpKWsnoG3i+lOjgqmq8HipwVlxdyHFr7Bbq1cHtS6Yu5IhmE7qn7Oxu7PK9ShPyRgrNDxL0TxpFGykTUcwzWXtBLRN3PtC93oUDaRxwWpjpLF6iTK5eRkMGDtFMKPd8UNBMJ3Y+pxVgo21mKNDDHF3uVg5Y0yw3kymCTbnWwrB1g1om9UEi8d3+hYIFtdnqAdwFJ3VAwVrvo+pgjlrGIJpxNqn3BYsNiVYtYPTnVwu3dlTmy1Y69dyzXUNUbxvBFsnoGWCEKzbVHVGEWpCxOX66yVmptBp82LFs76ICLZiQMvYEyxufc2bFqzVRrUFKyf13lB5MIVup7KbS29EBFsxoGVWEiwuK9qoREPzOtLMESxJiiuBaxFM0V/ti4hgKwa0zHqCJaOVe3SeQpp+wdRNzNhwQ/++WXdOPJp0mSGCrRbQMusIljc/sbIlqJUZitec3dyjUnT7yinz3nFfx+9AwRIj1+3RBIJpxN6n3CPYkv5ZUhbvLNpbfQ8XrNnyqeI0u639ERFsxYCWWUWw4qt+RLCxERDbgsXdyYqlJwrW6cS6A4JpxKpgzU5U7ZbP/SWG4rWalOKhr33IJy4VrLMP2bP4aPx4tmCWTUQwjawp2FiTMiJYaxSjfOgVrHbB/clUXwaNUZWBnMcFKyIh2BoBLWP3s2vspOgVrCZvz+DkIsGq7myr9Z2aVs8Sg4J1u7dWQTCN2BesGulodL86y43m1hGsiN4n2LR9pE6pOKmnOJradMGGOprdEvZAMI04IVjHh6nDca3XxZm+vSdnLBEsbgo2LbfhiMWf3rGS9uZAMG+xLphqKE7u9NfPnl0o2GjBAwQ7KEgniVaPs7POVgEE8xXLvfsewYrXca7ZgtG+IsLo+hfsg7Ur+PDZi1MiVmkoAyGYwYB2sS5YuylQLJP38BwRLFHuI2oTTJlOd4DU6ueEYNqw7Nf0+0kYEmxZ3AUXE5kQtD9UO0kE8xYEmxZ2QSrTY3aiI5jJgFaxLtj0BZelZkYwEwztiybNr4oiaVuZI5g2HKlsXcIXrI7qIEXztzgI5icOVrYMc4I5+ZY7gxqdxzixdzQMwbThZG2TLKxNngrW2efKn1QN16BgmtVDMF24WdkkC1PzVbC+wxdxPEEw3W8KwXThaGUTmErN1Z859h4fLC7N07v7OHbdhfkgmC4crWwCY6k5+p5rh8BVg4q1tqxTUvvADYLpwtHKdowMCNZZoDUdwZwFwZyh2sfq+VD6BesvsxAE0wWCuUNctEXzzm8ZbPUWgmCawC+HyC8lPjwI03s9YQRzDjfPaTheimv1TzgXRfEqCMEer/y8R7Ni48eH/qYJdDN1X6rxy9QhwZZ/vGsJtj0T/zQGtILqAkvZJdkRzDkmfiTVL1NVBeO4nL/suN9Kgu1fXicPH7ebsFkB487f9gJTfm8xdWVxEVFxyQvk8p+4dUOLhlitE0Bmfd4rCSbkEpIdEjDbi21W+mKzZNsmbvUB8mLFPlOcNDZiLYJiVcXNQ9qlsCs8VDfArlqxeZ+4G4JFOfOC1E4tS7pfOOV3Tdwk17K2pZqzFUXqTlZrV9gJQdB/O6X5LKvaedmlK9XTggG4jxstmIZkAFzE30EOAA9gmB7AIGsJtt/4eaAZYBacKgVgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBEAzAIAgGYBC3BAMIjUN80GaWf2svcCML0mjgRhqHZ4FgrmRBGg3cSAPBdOBGFqTRwI00EEwHbmRBGg3cSAPBdOBGFqTRwI00EEwHbmRBGg3cSAPBdOBGFqTRwI00fBcMIHAQDMAgCAZgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBVhRMeYtnezycR9HpXZHFWsnIu2c0c1gjFZnG2ltErP/Sha2hkxUFU947wh73Z/UsVkpmvxG1p5nDCqlkaay8Rfaby+T+5Hr9raGV9QRT3/3IHtuLWhYrJbOLPstvslblsEIqWRprb5F72Xperr419LKeYOr791nj8SfXtSxWSubrO7HiZg4rpJKl4cIWSVux1beGXo5WsP3LT6KT65UFy7aCA1UqW+n6W2R3eufA1tDJ0Qr28NF1sv/8Zu3P0SHBHNgiu6dObA2dHK1gAgc+R4cEE6ybxi5tQF3YGjo53kGOLIW196WLCrTybn1NsBXHWuR4vANbQydHO0x/L3ojn96tPRosK8/6A9MijbW3iOiiCtbfGjpZUTDlLZ7tsYvk6vMs1kpGCtbMYY1UZBorb5GtvIjupQNbQyecKgVgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBEAzAIAgGYBAEAzAIggEYBMEADIJgAAZBMACDIBiAQRAMwCAI5jziYpzl88uRhcXVO8EhEMx5aoKN64NgjoFgzoNgPoNgzpMKtt/8l00UXeZ3Z9hFUXQhLr/0c3kNi0zBbfYcwRwDwZxHCpYqtBOPl9nVAx/OL1KXTu8er87SJband+LKS2IGgjkGgjmPFOxC3N3nUugjn4trrMkn4lqC6RN5bc50AQRzDQRznvu85Ur/FHf4KW1LqinJfXbNMwRzCgRznrZgUUYmmLwwZ7Zf9vTvaMHcA8GcR9mCJcV4xv3Tf0ofRe+QLqKLIJjztAQrFcqe7Df/6Wl22eu0bUMw50Aw52kIdpHfg2SbTxRXnD4rGq/oAsFcA8GcpyZYalN+HEwOHkqX7uWhsHTayXVlHbgCggEYBMEADIJgAAZBMACDIBiAQRAMwCAIBmAQBAMwCIIBGATBAAyCYAAGQTAAgyAYgEEQDMAgCAZgEAQDMAiCARgEwQAMgmAABkEwAIMgGIBBEAzAIAgGYBAEAzDI/wfOlZxgoMqjnwAAAABJRU5ErkJggg==)



Remember to restore the system timezone


```r
Sys.setenv(TZ="Australia/Brisbane") #set back after plotting
```

