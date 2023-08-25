## Q10.1
# Import dataset
dataset = read.csv('uscrime.txt', sep="\t")

# Install dependencies
set.seed(101)
library(caTools)
library(Metrics)
library(tree)
library(randomForest)

get_r2 <- function(yhat, y){
  SSres <- sum((yhat-y)^2)
  SStot <- sum((y-mean(y))^2)
  R2 <- 1-SSres/SStot
  return(R2)
}

get_summary <- function(model, data, train, test){
  plot(model)
  text(model)
  tree.pred <- predict(model, data[,-16])
  print("R-squared")
  print(get_r2(tree.pred, data$Crime))
  print("RMSE Train")
  tree.pred <- predict(model, train[,-16])
  print(rmse(actual = train$Crime, predicted = tree.pred))
  print("RMSE Test")
  tree.pred <- predict(model, test[,-16])
  print(rmse(actual = test$Crime, predicted = tree.pred))
}

# Splitting into Training and Test sets
split = sample.split(dataset$Crime, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

# Basic tree model
model.tree <- tree(Crime~., data=training_set)
get_summary(model.tree, dataset, training_set, test_set)


# Pruned tree
model.prune <- prune.tree(model.tree, best=4)
get_summary(model.prune, dataset, training_set, test_set)

# Best tree model
model.best <- prune.tree(tree(Crime~., data=dataset), best=4)
summary(model.best)
plot(model.best)
text(model.best)
tree.pred <- predict(model.best)
get_r2(tree.pred, dataset$Crime)

# Random Forest Model
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)
num_tree <- c(500, 1000, 1500)
num_pred <- c(3:7)

get_forest <- function(train, test, ntree, npred){
  results = data.frame(matrix(nrow = 1,ncol = 5))
  names(results) <- c("trees", "variables", "R2", "RMSE - Train", "RMSE - Test")
  for (i in ntree){
    for (j in npred){
      rf <- randomForest(Crime~.,
                         data = train,
                         importance = T,
                         mtry = j,
                         ntree = i)
      yhat <- predict(rf)
      r2 <- get_r2(yhat, train$Crime)
      
      rf.pred <- predict(rf, train[,-16])
      rmse.train <- rmse(actual = train$Crime, predicted = rf.pred)
      
      rf.pred <- predict(rf, test[,-16])
      rmse.test <- rmse(actual = test$Crime, predicted = rf.pred)
      
      results[nrow(results) + 1,] = c(i, j, r2, rmse.train, rmse.test)
    }
  }
  return(results[-1,])
}
get_forest(training_set, test_set, num_tree, num_pred)

# Choose best parameters
rf <- randomForest(Crime~.,
                   data = training_set,
                   importance = T,
                   mtry = 6,
                   ntree = 500)
varImpPlot(rf)

# Perform PCA first
pca = prcomp(x=dataset[-16], scale=T)
pca.data = data.frame(cbind(pca$x, dataset$Crime))
names(pca.data)[16] <- "Crime"
training_set = subset(pca.data, split == T)
test_set = subset(pca.data, split == F)

# Then train tree model on transformed data
components = names(pca.data)[1:5]
num_pred <- c(3:5)
get_forest(training_set[,c("Crime", components)],
           test_set[,c("Crime", components)],
           num_tree, num_pred)

rf <- randomForest(Crime~.,
                   data = training_set[,c("Crime", components)],
                   importance = T,
                   mtry = 5,
                   ntree = 500)
varImpPlot(rf)
pca$rotation[,1:5]

### Logistic Regression
# Import dataset
dataset = read.csv('germancredit.txt', sep=" ")
dataset$X1.1 <- dataset$X1.1 -1

# Splitting into Training and Test sets
split = sample.split(dataset$X1.1, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

# Build model
model.log <- glm(X1.1~.,
                 family=binomial(link = 'logit'),
                 data = training_set)
summary(model.log)

# Retrain using only most important predictors
predictors = c("A11","X6","A34","A43","X1169","A65","X4","A93","A101","A143","A201")
model.log <- glm(X1.1 ~.,
                 family=binomial(link = 'logit'),
                 data = training_set[,c("X1.1",predictors)])
summary(model.log)

# Retrain on all data
model.log <- glm(X1.1 ~.,
                 family=binomial(link = 'logit'),
                 data = dataset[,c("X1.1",predictors)])
summary(model.log)

## Model threshold
cost <- function(y, pred_prob, threshold){
  # 1 means bad, 0 means good
  # Bad Customer
  if(y==1){
    if(pred_prob < threshold){ # predicted as good
      return(5)
    }
    else{ # true negative
      return(0)
    }
  }
  # Good Customer
  else{
    if(pred_prob >= threshold){ # predicted as bad
      return(1)
    }
    else{ # true positive
      return(0)
    }
  }
}

pred <- predict(model.log, training_set[,predictors], type='response')
ans <- training_set$X1.1

costs <- vector()
for (t in 1:100){
  x <- vector()
  for(i in 1:length(pred)){
    x[i] <- cost(ans[i], pred[i], t/100)
  }
  costs[t] <- sum(x)
}

plot(costs, x=c(1:100)/100, xlab = "Threshold", ylab = "Total Cost", main="Cost vs Threshold")

# get best threshold
index <- which.min(costs)
threshold <- index/100
costs[index]
threshold

# Confusion matrices
# on training data
yhat.train <- pred
ans <- training_set$X1.1
yhat.train[yhat.train >= threshold] <- 1
yhat.train[yhat.train < threshold] <- 0
table(yhat.train, ans)


# on test data
yhat.test <- predict(model.log, test_set[,predictors], type='response')
ans <- test_set$X1.1
yhat.test[yhat.test >= threshold] <- 1
yhat.test[yhat.test < threshold] <- 0
table(yhat.test, ans)