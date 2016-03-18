# Load data.
activity <- read.csv("activity.csv")

# Calculate mean total number of steps taken per day.
Totalsteps <- tapply(X = activity$steps, INDEX = activity$date, FUN = sum)
hist(totalsteps, breaks=10, main="Histogram of Total Steps Per Day", col="gray")

#Calculate average daily activity pattern.
intervalsteps <- tapply(X = activity$steps, INDEX = activity$interval, 
                         FUN = mean, na.rm = T)
plot(intervalsteps, type="l", xaxt="n",
     main="Daily Activity Pattern", 
     xlab="Time", ylab="Average # of Steps")
intervalsteps[which(intervalsteps == max(intervalsteps))]

#Assess missing values
sum(is.na(activity$steps))
activity2 <- activity
for (i in 1:nrow(activity2)){
  if (is.na(activity2[i, 1])){
    int_nr <- activity2[i, "interval"]
    
  replacement <- intervalsteps[names(intervalsteps) == int_nr]
  replacement <- round(replacement)
  activity2[i, 1] <- replacement
  }
}

# Show some rows of the resuting data frame. Previously were NA --
activity2[109:120,]

# Check deltas between the files.
hist(aggregate(steps ~ date, FUN=sum, data=activity2)$steps, breaks=10,
     main="Histogram of Steps Per Day (Inc. Missing Values)", col = "gray",
     xlab = "Steps")

# Deltas in activity patterns with weekends vs. weekdays
week <- rep(NA, times = nrow(activity2))
for (i in 1:nrow(activity2)){
  day <- as.POSIXlt(activity2[i, "date"])$wday
  # 0 means sunday, 6 means saturday
  if (day == 0 | day == 6) week[i] <- "Weekend"
  else week[i] <- "Weekday"
}
week <- factor(week) # convert to factor
activity2 <- cbind(activity2, week) # merge
str(activity2)

# Plot data.
library(reshape2)
library(ggplot2)
plotdata <- with(activity2, tapply(steps, INDEX = list(interval, week), mean))
plotdata <- melt(plotdata, id=c("Weekday", "Weekend"),
                 varnames = c("Interval", "Week"), value.name = "Steps")
head(plotdata)
ggplot(plotdata, aes(Interval, Steps)) + geom_line() + facet_wrap(~Week, nrow=2)

