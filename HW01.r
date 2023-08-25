# load packages
library(kernlab)
library(kknn)

# read data with headers
data = read.table("credit_card_data-headers.txt", header=TRUE, sep="\t")

# function to train the svm model
model.svm <- function (C, kernel="vanilladot", return.type="performance"){
  # call ksvm.  Vanilladot is a simple linear kernel.
  # returns percent of correct predictions
  model = ksvm(as.matrix(data[,1:10]), # predictors
               as.factor(data[,11]), # target
               type="C-svc", # default classifier
               kernel=kernel, # kernel to use
               C=C,
               scaled=TRUE) # scales features
  
  if (return.type == "model"){
    return(model)
  }
  else{
    pred = predict(model, data[,1:10])
    performance = sum(pred == data[,11]) / nrow(data)
    
    return(list(performance, model@nSV, model@error))
  }
}

# measure performance for different values of C
for(i in -6:6){
  perf = train.svm(10^i)
  print(list(i, perf))
}

# using C = 10, find coefficients of the svm classifier
model <- train.svm(10, return.type="model")
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a0 <- model@b*-1

# still using C = 10, test the impact of using different kernels
for (kernel in list("vanilladot",
                "laplacedot",
                "splinedot")){
  perf = train.svm(10, kernel)
  print(list(kernel, perf))
}


# KNN model
model.knn <- function(k){
  preds = array(0, nrow(data))
  for (i in 1:nrow(data)){
    model = kknn(as.factor(R1)~., train=data[-i,], test=data[i,], k=k, scale=TRUE)
    preds[i] = round(as.numeric(predict(model))) -1 # predictions are between 1 & 2, so subtracting to compare
  }
  
  performance = sum(preds == data[,11]) / nrow(data)
  return(performance)
}


perfomances = array(0, 130)
for (i in 1:130){
  if(i%%2==1){
    perfomances[i] = model.knn(i*5)
  }
}