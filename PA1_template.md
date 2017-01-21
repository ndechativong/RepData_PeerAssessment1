Reproducible Research: Course Project 1
=======================================

Loading and preprocessing the data
----------------------------------

### Code for reading in the dataset and/or processing the data

    step1 <- read.table("C:/Users/ndechativong/Documents/R/coursera/data/activity.csv", header = TRUE, sep = ",")
    head(step1)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

### Histogram of the total number of steps taken each day

    step1_sum <- aggregate(step1$steps, by=list(date=step1$date), sum, na.rm = TRUE)
    hist(step1_sum$x, main = "Histogram of the total number of steps taken each day", xlab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/hist_stepday-1.png)

### Mean and median number of steps taken each day

    step1_mean <- mean(step1_sum$x, na.rm = TRUE)
    step1_mean

    ## [1] 9354.23

    step1_median <- median(step1_sum$x, na.rm = TRUE)
    step1_median

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

### Time series plot of the average number of steps taken

    library("lattice")
    step1_int_avg <- aggregate(step1$steps, by=list(interval=step1$interval), mean, na.rm = TRUE)
    xyplot(step1_int_avg$x ~ step1_int_avg$interval, type = "l", xlab = "5-minute interval", ylab = "Average number of steps")

![](PA1_template_files/figure-markdown_strict/timeplot_avg-1.png)

### The 5-minute interval that, on average, contains the maximum number of steps

    apply(step1_int_avg, MARGIN = 2, function(x) max(x, na.rm = TRUE))

    ##  interval         x 
    ## 2355.0000  206.1698

Imputing missing values
-----------------------

### Calculate and report the total number of missing values in the dataset

    sum(is.na(step1$steps))

    ## [1] 2304

### Code to describe and show a strategy for inputing missing data

    # Average steps per day
    step1_day_avg <- aggregate(step1$steps, by=list(date=step1$date), mean, na.rm = TRUE)

    # Number of missing inputs per day
    aggregate(is.na(step1$steps), by=list(date=step1$date), sum, na.rm = FALSE)

    ##          date   x
    ## 1  2012-10-01 288
    ## 2  2012-10-02   0
    ## 3  2012-10-03   0
    ## 4  2012-10-04   0
    ## 5  2012-10-05   0
    ## 6  2012-10-06   0
    ## 7  2012-10-07   0
    ## 8  2012-10-08 288
    ## 9  2012-10-09   0
    ## 10 2012-10-10   0
    ## 11 2012-10-11   0
    ## 12 2012-10-12   0
    ## 13 2012-10-13   0
    ## 14 2012-10-14   0
    ## 15 2012-10-15   0
    ## 16 2012-10-16   0
    ## 17 2012-10-17   0
    ## 18 2012-10-18   0
    ## 19 2012-10-19   0
    ## 20 2012-10-20   0
    ## 21 2012-10-21   0
    ## 22 2012-10-22   0
    ## 23 2012-10-23   0
    ## 24 2012-10-24   0
    ## 25 2012-10-25   0
    ## 26 2012-10-26   0
    ## 27 2012-10-27   0
    ## 28 2012-10-28   0
    ## 29 2012-10-29   0
    ## 30 2012-10-30   0
    ## 31 2012-10-31   0
    ## 32 2012-11-01 288
    ## 33 2012-11-02   0
    ## 34 2012-11-03   0
    ## 35 2012-11-04 288
    ## 36 2012-11-05   0
    ## 37 2012-11-06   0
    ## 38 2012-11-07   0
    ## 39 2012-11-08   0
    ## 40 2012-11-09 288
    ## 41 2012-11-10 288
    ## 42 2012-11-11   0
    ## 43 2012-11-12   0
    ## 44 2012-11-13   0
    ## 45 2012-11-14 288
    ## 46 2012-11-15   0
    ## 47 2012-11-16   0
    ## 48 2012-11-17   0
    ## 49 2012-11-18   0
    ## 50 2012-11-19   0
    ## 51 2012-11-20   0
    ## 52 2012-11-21   0
    ## 53 2012-11-22   0
    ## 54 2012-11-23   0
    ## 55 2012-11-24   0
    ## 56 2012-11-25   0
    ## 57 2012-11-26   0
    ## 58 2012-11-27   0
    ## 59 2012-11-28   0
    ## 60 2012-11-29   0
    ## 61 2012-11-30 288

    # Create new data frame to capture made-up input values
    step2 <- step1

    for (n in 1:nrow(step1)) {
        s <- step1[n, ]
        
        if (is.na(s$steps))
            steps <- subset(step1_int_avg, interval == s$interval)$x
        else
            steps <- s$steps

        step2[n, 1] <- round(steps)
    }

### Histogram of the total number of steps taken each day after missing values are inputed

    step2_sum <- aggregate(step2$steps, by=list(date=step2$date), sum, na.rm = TRUE)
    hist(step2_sum$x, main = "Histogram of the total number of steps taken each day", xlab = "Number of steps")

![](PA1_template_files/figure-markdown_strict/hist_summiss-1.png)

### Mean and median number of steps taken each day after missing values are inputed

    step2_mean <- mean(step2_sum$x, na.rm = TRUE)
    step2_mean

    ## [1] 10765.64

    step2_median <- median(step2_sum$x, na.rm = TRUE)
    step2_median

    ## [1] 10762

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

    step2$weekday <- weekdays(as.Date(step2$date))
    step2$daytype <- ifelse((step2$weekday == "Saturday" | step2$weekday == "Sunday"), "Weekend", "Weekday")
    step2_int_avg <- aggregate(step2$steps, by=list(interval=step2$interval, step2$daytype), mean, na.rm = TRUE)
    names(step2_int_avg) <- c("interval", "daytype", "step")

    xyplot(step ~ interval | daytype, step2_int_avg, type = "l", layout = c(1, 2), xlab = "5-minute interval", ylab = "Average number of steps")

![](PA1_template_files/figure-markdown_strict/plot_avg_week-1.png)
