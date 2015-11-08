library(plyr)
library(lattice)
# load data
indata <- read.csv('~/Documents/RepData_PeerAssessment1/activity.csv', header = TRUE)

# Part 1

total_steps <- aggregate(. ~ date, data=indata, FUN=sum)

hist(total_steps$steps, xlab="Number of steps per day", main="Histogram of the total number of steps taken every day")
mean_steps <- mean(total_steps$steps)
median_steps <- median(total_steps$steps)

# Part 2
# average steps per interval
average_steps <- aggregate(. ~ interval, data=indata, FUN=mean)

plot.ts(average_steps[,1], average_steps[,2], xlab='Interval', ylab='Average steps per interval', type='l')

maximum_steps_per_interval <- max(average_steps$steps)

# Part 3
# report missing values

newdata <- indata

count_NA <- sum(is.na(indata$steps))

for (i in 1:nrow(indata)){
  if (is.na(indata[i, "steps"])){
    newdata[i, "steps"] <- average_steps$steps[average_steps$interval==newdata[i, "interval"]]
  }
}

new_total_steps <- aggregate(. ~ date, data=newdata, FUN=sum)

hist(new_total_steps$steps, xlab="Number of steps per day", main="Histogram of the replaced total number of steps taken every day")
new_mean_steps <- mean(new_total_steps$steps)
new_median_steps <- median(new_total_steps$steps)

# part 4

day <- as.numeric(weekdays(as.Date(indata$date))=='Sunday') + as.numeric(weekdays(as.Date(indata$date))=='Saturday')

augmentdata <- cbind(newdata, day)
augmentdata$day[augmentdata$day == 0] <- "weekday"
augmentdata$day[augmentdata$day == 1] <- "weekend"

weekday_data <- augmentdata[augmentdata$day == "weekday", ]
weekday_average_steps <- aggregate(steps ~ interval, data=weekday_data, FUN=mean)
day <- vector(, nrow(weekday_average_steps))
day[] <- 'weekday'
weekday_average_steps <- cbind(weekday_average_steps, day)

weekend_data <- augmentdata[augmentdata$day == "weekend", ]
weekend_average_steps <- aggregate(steps ~ interval, data=weekend_data, FUN=mean)
day <- vector(, nrow(weekend_average_steps))
day[] <- 'weekend'
weekend_average_steps <- cbind(weekend_average_steps, day)

finaldata <- rbind(weekday_average_steps, weekend_average_steps)

xyplot(steps ~ interval | day, data=finaldata, type='l', layout=c(1,2))


