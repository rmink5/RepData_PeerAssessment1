library(ggplot2)

activity <- read.csv("activity.csv")

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)

summary(activity)

##Plot1
total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(total_steps) <- c("date", "steps")
hist(total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "blue", ylim = c(0,15), breaks = seq(0,25000, by=1250))


mean(activity_total_steps$steps)
median(activity_total_steps$steps)

##Plot2
avg_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(avg_daily_activity) <- c("interval", "mean")
plot(avg_daily_activity$interval, avg_daily_activity$mean, type = "l", col="blue", lwd = 2, xlab="Interval", ylab="Average Number of Steps", main="Average Number of Steps Per Intervals")

avg_daily_activity[which.max(avg_daily_activity$mean), ]$interval



sum(is.na(activity$steps))
missed_steps <- avg_daily_activity$mean[match(activity$interval, avg_daily_activity$interval)]


activity_missed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = missed_steps, no = activity$steps))
total_steps_missed <- aggregate(steps ~ date, activity_missed, sum)
names(total_steps_missed) <- c("date", "daily_steps")


##Plot 3

hist(total_steps_missed$daily_steps, col = "blue", xlab = "Total Steps Per Day", ylim = c(0,30), main = "Total Number of Steps Taken Each Day", breaks = seq(0,25000,by=2500))


activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

#Plot4
activity_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average Daily Steps By Date Type", x = "Interval", y = "Average Number of Steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
