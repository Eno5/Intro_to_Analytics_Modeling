# Q14.1
dataset <- read.csv('breast-cancer-wisconsin.data.txt', header = F)
library(caTools)

# Find missing values
str(dataset)
unique(dataset$V7)

# Missing values stored as "?" in V7
missing <- dataset$V7 == "?"
nrow(missing) *100 / nrow(dataset)

# less than 5% of values are missing, so good to go

dataset$V11 <- dataset$V11 / 2 # values are now 1 or 2 instead of 2 or 4, easier for classifier
set.seed(101)
training <- sample.split(dataset$V11, SplitRatio = 0.8)
dataset$Is.Training <- training
excl <- c(1,12) # exclude V1 and Is.Training columns from models

# Q14.1.1
getmode <- function(x){
  uniqv <- unique(x)
  return(uniqv[which.max(tabulate(match(x, uniqv)))])
}

non_missing <- subset(dataset, missing == F)
non_missing$V7 <- as.integer(non_missing$V7)
q1.data <- dataset
q1.data$V7 <- as.integer(ifelse(q1.data$V7=="?",
                                getmode(non_missing$V7),
                                q1.data$V7))

q1.replace <- q1.data[missing,]

# Q14.1.2
set.seed(101)
library(DAAG)

model <- lm(V7~., data=non_missing[,-excl])
summary(model)
step(model)

model <- lm(formula = V7 ~ V3 + V4 + V5 + V8 + V9 + V11, data = non_missing[,-excl])
summary(model)

model_cv <- cv.lm(non_missing[,-excl], model, m=5)
SST <- sum((non_missing$V7 - mean(non_missing$V7))^2)
R2_cv <- 1 - attr(model_cv,"ms")*nrow(non_missing)/SST
# R2_cv is 0.694

v7_new <- round(predict(model, dataset[missing,]))
q2.data <- dataset
q2.data[missing,]$V7 <- v7_new
q2.data$V7 <- as.integer(q2.data$V7)

# make sure everything is in original range
q2.data[q3.data$V7 > 10,]$V7 <- 10
q2.data[q3.data$V7 < 1,]$V7 <- 1
q2.replace <- q2.data[missing,]


# Q14.1.3
v7_pert <- round(rnorm(nrow(dataset[missing,]), v7_new, sd(v7_new)))

q3.data <- dataset
q3.data[missing,]$V7 <- v7_pert
q3.data$V7 <- as.integer(q3.data$V7)

# make sure everything is in original range
q3.data[q3.data$V7 > 10,]$V7 <- 10
q3.data[q3.data$V7 < 1,]$V7 <- 1
q3.replace <- q3.data[missing,]


# Q14.1.4
library(kknn)

accuracies = data.frame(matrix(nrow = 50, ncol = 5))
names(accuracies) <- c("Mode", "Linear Regression",
                       "Perturbation", "Missing Removed", "Binary Variable")

bin.data <- dataset
bin.data$Indicator <- missing
bin.data$V7 <- as.integer(ifelse(bin.data$V7=="?", # replace "?" with 0's
                                 0,
                                 bin.data$V7))


datas <- list(q1.data, q2.data, q3.data, non_missing, bin.data)

for (col in 1:ncol(accuracies)){
  acc = rep(0, 50)
  data = as.data.frame(datas[col])
  for (k in 1:50){
    knn_model <- kknn(V11~., data[data$Is.Training,-excl],
                      data[-data$Is.Training, -excl], k=k)
    pred <- round(fitted(knn_model)) # multiply by 2 for original label
    acc[k] <- sum(pred == data[-data$Is.Training,]$V11) / nrow(data[-data$Is.Training,])
  }
  accuracies[,col] <- acc
}

accuracies

write.csv(accuracies, "model accuracies.csv")
