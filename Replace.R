ina <- function() {
        
        setwd("C:\\Users\\Automation\\Documents\\R\\RepData_PeerAssessment1")
        unzip("activity.zip")
        dataStep <- read.csv("activity.csv")
        
        aggSum <- aggregate(steps ~ date, data = dataStep, FUN = sum)
        hist(aggSum$step, 53, main = "Steps per day", xlim = c(0,25000))
        mean(aggSum$steps)
        
        meanStep <- mean(aggSum$step)
        medianStep <- median(aggSum$step)
        
        meanStep
        medianStep
        
        avgStp <- aggregate(steps ~ interval, data = dataStep, FUN = mean)
        plot(avgStp$interval, avgStp$steps, type = "l")
        maxStp <- avgStp[max(avgStp$steps),1]
        maxStp
        
        sum(is.na(dataStep))
        
        dataNoNa <- dataStep
        for (i in 1:dim(dataNoNa[1])) {
                if (is.na(dataNoNa[i,1] == TRUE)) {
                        dataNoNa[i,1] <- avgStp[avgStp$interval == dataNoNa[i,3],2]
                }
        }
        avgNoNa <- aggregate(steps ~ interval, data = dataNoNa, FUN = mean)
        
        plot(avgNoNa$interval, avgNoNa$steps, type = "l")
        aggNoNa <- aggregate(steps ~ date, data = dataNoNa, FUN = sum)
        hist(aggNoNa$step, 53, main = "Steps per day no Na", xlim = c(0,25000))
        
        stepsDayNoNa <- aggregate(steps ~ date, data = dataNoNa, FUN = sum)
        meanNoNa <- mean(stepsDayNoNa$step)
        medianNoNa <- median(stepsDayNoNa$step)
        meanNoNa
        medianNoNa
        dataNoNa$weekday <- as.character(weekdays(as.Date(dataNoNa$date)))
        
        dataNoNa$weekday[dataNoNa$weekday == "Monday"] <- "Weekday"
        dataNoNa$weekday[dataNoNa$weekday == "Tuesday"] <- "Weekday"
        dataNoNa$weekday[dataNoNa$weekday == "Wednesday"] <- "Weekday"
        dataNoNa$weekday[dataNoNa$weekday == "Thursday"] <- "Weekday"
        dataNoNa$weekday[dataNoNa$weekday == "Friday"] <- "Weekday"
        dataNoNa$weekday[dataNoNa$weekday == "Saturday"] <- "Weekend"
        dataNoNa$weekday[dataNoNa$weekday == "Sunday"] <- "Weekend"
        dataNoNa$weekday <- as.factor(dataNoNa$weekday)
        

}