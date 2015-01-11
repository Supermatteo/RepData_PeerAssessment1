ina <- function() {
        
        dataNoNa <- dataStep
        for (i in 1:dim(dataNoNa)) {
                if (is.na(dataNoNa[i,1] == TRUE)) {
                        dataNoNa[i,1] <- avgStp[avgStp$interval == dataNoNa[i,3],2]
                }
        }
        avgNoNa <- aggregate(steps ~ interval, data = dataNoNa, FUN = mean)
        plot(avgNoNa$interval, avgNoNa$steps, type = "l")
}