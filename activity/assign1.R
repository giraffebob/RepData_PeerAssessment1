## Reproducible Research Assignment 1
## First load the data. It is assumed that the data has been unzipped and the file activity.csv is in the working directory.
activity <-read.csv("activity.csv",header=T,stringsAsFactors=FALSE)
## For the first parts, we ignore data containing NAs.
activityComplete <- activity[complete.cases(activity),]
## We load some libraries
library(reshape2,quietly=T)
library(lubridate,quietly=T)
library(ggplot2,quietly=T)

##Sum the steps for each day, plot histogram, show mean and median.
StepsByDate <- recast(activityComplete[c("date","steps")],formula=date~...,fun.aggregate=sum,id.var="date")
hist(StepsByDate$steps,breaks=10,main="Histogram of Total Steps in a Day ",xlab="Total Steps", ylab="Number of Days")
print("Mean daily steps")
print(mean(StepsByDate$steps))
print("Median daily steps")
print(median(StepsByDate$steps))


##Calculate the mean number of steps for each time interval and plot it
meanStepsByInterval <- recast(activityComplete[c("interval","steps")],formula=interval~...,fun.aggregate=mean,id.var="interval")
## plot the data and find the maximum
with(meanStepsByInterval,plot(interval,steps,type='l',main="Mean Number of Steps by Interval",ylab="Number of Steps"))
print("The interval with highest mean steps")
print(with(meanStepsByInterval,interval[which.max(steps)]))

##missing data
## Report number of incomplete rows
nrow(activity)-nrow(activityComplete)
## Impute missing values, using the mean for each interval, rounded to be an integer
activityImputed <-activity

for (i in 1:length(activity$steps)) {
   if (is.na(activityImputed$steps[i])) {
      activityImputed$steps[i]<-round(meanStepsByInterval$steps[meanStepsByInterval$interval==activityImputed$interval[i]],0)
   }
}
## Show mean, median and histogram for altered data
StepsByDate <- recast(activityImputed[c("date","steps")],formula=date~...,fun.aggregate=sum,id.var="date")
print("Mean daily steps after imputation")
print(mean(StepsByDate$steps))
print("Median daily steps after imputation")
print(median(StepsByDate$steps))
hist(StepsByDate$steps,breaks=10,main="Histogram of Total Steps in a Day After Imputation ",xlab="Total Steps", ylab="Number of Days")
## Slight change in mean due to rounding of imputed values.
## slight change in median, due to insertion of new values.
## Histogram has 8 extra days in modal class.

## Add a column identifying data as weekday or weekend
weekend <- weekdays(ymd(activityImputed$date),abbreviate=T) %in% c("Sat","Sun")
activityImputed$dayType <- factor(weekend,levels=c(TRUE,FALSE),labels=c("weekend","weekday"))

## Calculate mean steps by interval and type of day
meanStepsByInterval <- recast(activityImputed[c("interval","steps","dayType")],formula=interval+dayType~...,fun.aggregate=mean,id.var=c("interval","dayType"))
## Produce a faceted plot using ggplot
p <- ggplot(meanStepsByInterval,aes(x=interval,y=steps))+geom_line()+facet_grid(dayType~.)
p <- p + ggtitle("Mean Number of Steps in each Interval for Weekend and Week Days")
print(p)


