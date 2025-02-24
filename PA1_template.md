---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


``` r
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(lubridate)
```

## Loading and preprocessing the data


``` r
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              destfile = "repdata_data_activity.zip")

df<- unzip(zipfile = "repdata_data_activity.zip")

df<- read_csv(df)
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## What is mean total number of steps taken per day?

### Histogram of the total number of steps taken each day


``` r
df %>%
  group_by(date) %>% 
  summarise(n_steps= sum(steps, na.rm= F)) %>% 
  ggplot(mapping = aes(x= n_steps))+
  geom_histogram(binwidth = 5000, color= "black", fill= "grey80")+
  xlab("Number of steps")+
  ylab("Frequency")+
  ggtitle("Total Steps per Day")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```
## Warning: Removed 8 rows containing non-finite outside the scale range
## (`stat_bin()`).
```

![](https://github.com/LuisD0623/RepResearch_Project1/blob/master/figure/fill_hist-1.png?raw=true)<!-- -->

### Mean and median number of steps taken each day


``` r
mean<- df %>%
  group_by(date) %>% 
  summarise(n_steps= sum(steps, na.rm= F)) %>% 
  ungroup() %>% 
  summarise(avg_steps = mean(n_steps, na.rm = T)) %>% 
  pull()

mean
```

```
## [1] 10766.19
```

``` r
median<- df %>%
  group_by(date) %>% 
  summarise(n_steps= sum(steps, na.rm= F)) %>% 
  ungroup() %>% 
  summarise(med_steps = median(n_steps, na.rm = T)) %>% 
  pull()

median
```

```
## [1] 10765
```


## What is the average daily activity pattern?

### Time series plot of the average number of steps taken


``` r
df %>%
  group_by(interval) %>% 
  summarise(n_steps= mean(steps, na.rm= T))  %>% 
  ggplot(mapping = aes(x= interval, y= n_steps))+
  geom_line()+
  xlab("Interval")+
  ylab("Number of Steps")+
  ggtitle("Average Steps per Day by Interval")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](https://github.com/LuisD0623/RepResearch_Project1/blob/master/figure/time_series-1.png?raw=true)<!-- -->

### The 5-minute interval that, on average, contains the maximum number of steps


``` r
max_int<- df %>%
  group_by(interval) %>% 
  summarise(n_steps= mean(steps, na.rm= T)) %>% 
  arrange(desc(n_steps)) %>% 
  head(1)

max_int[, 1]
```

```
## # A tibble: 1 × 1
##   interval
##      <dbl>
## 1      835
```

## Imputing missing values


``` r
sum(is.na(df)) # Calculate and report the total number of missing values in the dataset
```

```
## [1] 2304
```

``` r
fill_df <- df %>%
  group_by(interval) %>%  
  mutate(steps = ifelse(is.na(steps), 
                        mean(steps, na.rm = TRUE), # I used  the mean for that 5-minute interval
                        steps)) %>% 
  ungroup()

head(fill_df)
```

```
## # A tibble: 6 × 3
##    steps date       interval
##    <dbl> <date>        <dbl>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```


### Histogram of the total number of steps taken each day after missing values are imputed


``` r
fill_df %>%
  group_by(date) %>% 
  summarise(n_steps= sum(steps, na.rm= F)) %>% 
  ggplot(mapping = aes(x= n_steps))+
  geom_histogram(binwidth = 5000, color= "black", fill= "grey80")+
  xlab("Number of steps")+
  ylab("Frequency")+
  ggtitle("Total Steps per Day")+
  labs(caption = "*The average for the corresponding interval was used to fill the null observations")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](https://github.com/LuisD0623/RepResearch_Project1/blob/master/figure/fill_hist-1.png?raw=true)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


``` r
df %>% 
  mutate(w_day= wday(date, label = T, abbr = F),
         w_day= case_when(w_day == "sábado" ~ "Weekend",
                          w_day == "domingo" ~ "Weekend",
                          T ~ "Weekday")) %>%
  summarise(.by = c(w_day, interval),
            steps= mean(steps, na.rm= T)) %>% 
  ggplot(mapping = aes(x= interval, y= steps))+
  geom_line()+
  xlab("Interval")+
  ylab("Number of steps")+
  ggtitle(label = "Total Steps per Day by Interval",
          subtitle = "Weekdays vs Weekends")+
  facet_wrap(~w_day, nrow= 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
```

![](https://github.com/LuisD0623/RepResearch_Project1/blob/master/figure/weekends-1.png?raw=true)<!-- -->
