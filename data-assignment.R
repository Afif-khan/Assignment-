#Loading and preprocessing the data

library(dplyr)
library(ggplot2)

activity<-read.csv("activity.csv")
activity$date<- as.Date(activity$date)

#What is the mean total number of steps taken per day?

daily_steps<-activity %>%
  group_by(date) %>%
  summarise(total_steps=sum(steps, na.rm=TRUE))
hist(daily_steps$total_steps,
     main="Total Steps Per Day",
     xlab="Steps",
     col="lightblue")
mean(daily_steps$total_steps)
median(daily_steps$total_steps)

#What is the average daily activity pattern?

interval_steps<-activity %>%
  group_by(interval) %>%
  summarise(avg_steps=mean(steps, na.rm=TRUE))
plot(interval_steps$interval,
     interval_steps$avg_steps,
     type="1",
     xlab="5-minute Interval",
     ylab="Average Steps")
interval_steps[which.max(interval_steps$avg_steps), ]

#Imputing missing values

sum(is.na(activity$steps))
activity_failed<-activity
interval_mean<-tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
na<-is.na(activity_failed$steps)
activity_failed$steps[na]<-interval_mean[as.character((activity_failed$interval[na]))]
daily_steps2<-activity_failed %>%
  group_by(date) %>%
  summarise(total_steps=sum(steps))
hist(daily_steps2$total_steps,
     main="Steps Per day (Missing Value Failed)",
     xlab="Steps",
     col="orange")
mean(daily_steps2$total_steps)
median(daily_steps2$total_steps)

#Weekday vs Weekend

activity_failed$daytype<-ifelse(weekdays(activity_failed$date)
                                %in%
                                  c("Saturday","Sunday"),"Weekend","Weekday")
pattern<-activity_failed %>%
  group_by(interval,daytype) %>%
  summarise(avg_steps=mean(steps),groups="drop")
library(lattice)
xyplot(avg_steps-interval|datatype,
       data=pattern,
       type="1",
       layout=c(1,2),
       xlab="Interval"
       ylab="Average Steps")


