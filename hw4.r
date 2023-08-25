library(reshape2)

temps <- read.table("temps.txt", header=TRUE)
temps_reshaped <- melt(temps,id="DAY")

temp_ts <- ts(temps_reshaped["value"],
              start=c(1996, 1),
              end=c(2015,123),
              frequency=123)
hw.model <- HoltWinters(temp_ts, seasonal="additive")
fitted <- hw.model$fitted[,"xhat"]
dim(fitted) <- c(123, 19)

write.csv(fitted, "fitted.csv", row.names = FALSE)