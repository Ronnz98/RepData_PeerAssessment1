library(ggplot2)

#load data
myActivity <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(myActivity)

#convert date variable from char to date type
myActivity$date <- as.Date(myActivity$date)
str(myActivity)

# Check the data
summary(myActivity)

#1.What is mean total number of steps taken per day?
myActivity_steps_per_day <- aggregate(steps ~ date, data = myActivity, FUN = sum, simplify = TRUE, na.rm = TRUE)

#show histogram
hist(myActivity_steps_per_day$steps, main = "Total number of steps taken per Day", xlab = "Total # Steps per Day")

#show mean
mean(myActivity_steps_per_day$steps)
#show median
median(myActivity_steps_per_day$steps)

#2. What is the average daily activity pattern?
myActivity_steps_mean <- aggregate(steps ~ interval, data = myActivity, FUN = mean, simplify = TRUE, na.rm = TRUE)
#Plot result
plot(myActivity_steps_mean$interval, myActivity_steps_mean$steps, type = "l", xlab = "5-Minutes-Intervals", ylab = "Average number of steps taken, averaged across all days ", main = "Average number of steps per interval")
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
myMaxNosteps <-max(myActivity_steps_mean$steps)
myMaxInterval <- myActivity_steps_mean$interval[which(myActivity_steps_mean$steps == myMaxNosteps)]
myMaxInterval
myMaxNosteps

#3. Imputing missing values
#Total amount of missing data
sum(is.na(myActivity))

#Create a new dataset from the original datasets with no missing data and using mean of the 5-minute interval
myActivityNew <- myActivity
myActivityNew[which(is.na(myActivityNew$steps)),1] <- myMaxNosteps[as.character(myActivityNew[which(is.na(myActivityNew$steps)),3])]

#Total amount of missing data now (should be 0)
sum(is.na(myActivity))

#show histogram with imputed missing values

myActivityStepsNew <- aggregate(steps ~ date, data = myActivityNew, FUN = sum, simplify = TRUE, na.rm = TRUE)
hist(myActivityStepsNew$steps, main = "Total number of steps taken per Day (missing values replaced)", xlab = "Total # Steps per Day")

#show mean with imputed missing values
mean(myActivityStepsNew$steps)
#show median with imputed missing values
median(myActivityStepsNew$steps)

#-> The impact of imputed missing data very low

#4. Are there differences in activity patterns between weekdays and weekends?

#Create a factor variable in the dataset with 2 levels: “weekday” and “weekend”
myActivityNew$dayType <- ifelse(weekdays(as.Date(myActivityNew$date)) == "Saturday" | weekdays(as.Date(myActivityNew$date)) == "Sunday", "weekend", "weekday")
myActivityNew$dayType <- factor(myActivityNew$dayType)

#Aggregate a table showing mean steps for all intervals, acrlss week days and weekend days
myStepIntervalDayType <- aggregate(steps ~ interval + dayType, data = myActivityNew, FUN = mean, simplify = TRUE, na.rm = TRUE)
#add descriptive variables and plot
names(myStepIntervalDayType) <- c("interval", "day_type", "mean_steps")
plot <- ggplot(myStepIntervalDayType, aes(interval, mean_steps))
plot + geom_line(color = "tan1") + facet_grid(day_type~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns on Weekends/-days")