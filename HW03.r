library(outliers)

# import dataset (for me the first row is column header)

crimes.per.capita <- as.numeric(uscrime[2:nrow(uscrime),"V16"])

# visualize distribution
hist(crimes.per.capita)
boxplot(crimes.per.capita)

# test the most extreme outlier
grubbs.test(crimes.per.capita, type=10)

# p-value isn't sufficient to dismiss as an outlier