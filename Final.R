Data<-read.csv("~/Desktop/Reproducible Research/activity.csv", header=T)
Data<-data.frame(Data)
Data$date <- as.Date(Data$date)
library(reshape2);library(ggplot2)

#What is mean total number of steps taken per day?
stepsd <- aggregate(steps ~ date, Data, sum)
ggplot(stepsd, aes(x=date , y=steps , fill=steps)) + geom_bar(stat = "identity", position="identity", colour="orange", size=0.25)  +geom_text(aes(x = date, 
                                                                                                                                                  y = steps, 
                                                                                                                                                  label = steps, 
                                                                                                                                                  angle  = 90, 
                                                                                                                                                  size = 4, 
                                                                                                                                                  hjust = -0.1), 
                                                                                                                                              color = "orange", 
                                                                                                                                              show_guide  = F)                                                                                                                                                                                      


paste("Mean Steps per Day =", mean(stepsd$steps, na.rm=TRUE))
paste("Median Steps per Day =", median(stepsd$steps, na.rm=TRUE))

#What is the average daily activity pattern?
stepsi <- aggregate(steps ~ interval, Data, mean)
ggplot(stepsi, aes(x=interval , y=steps )) +geom_area(fill="blue", alpha=.2) + geom_line() + xlim(-2, 2500)
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  
max<-subset(stepsi, steps==max(stepsi$steps))
max

# Imputing missing values
incomp <- sum (is.na(Data$steps))
incomp
#2. Devise a strategy for filling in all of the missing values in the dataset.
meansteps<-mean(stepsi$steps)
Data$steps[is.na(Data$steps)] <- meansteps
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_filled <- transform(Data, (is.na(Data$steps)==meansteps))
data_filled[as.character(data_filled$date) == "2012-10-01", 1] <- 0
stepsdi<- aggregate(steps ~ date, data_filled, sum)
hist(stepsdi$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps")
hist(stepsd$steps, main = paste("Total Steps Each Day"), col="yellow", xlab="Number of Steps", add=T)
legend("topright", c("Filled", "Non-Filled"), col=c("green", "yellow"), lwd=8)


weekday_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
Data$date <- as.Date(Data$date)
Data$day <- sapply(Data$date, FUN=weekday_weekend)
Data$day<- factor(Data$day)
Mean_week <- aggregate(steps ~ interval + day, data=Data, mean)



ggplot(Mean_week, aes(interval, steps, fill=factor(day))) + geom_line()+ geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(Mean_week$day)))



