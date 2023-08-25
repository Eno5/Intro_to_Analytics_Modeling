# Import dataset
dataset = read.csv('uscrime.txt', sep="\t")

pca = prcomp(x=dataset[-16], scale=T)

# Splitting into Training and Test sets
library(caTools)
set.seed(101)
split = sample.split(dataset$Crime, SplitRatio = 0.8)


# Find ideal number of PCA components to include in model
library(Metrics)
pca.data = data.frame(cbind(pca$x, dataset$Crime))
names(pca.data)[16] <- "Crime"
training_set = subset(pca.data, split == T)
test_set = subset(pca.data, split == F)

scores = data.frame(matrix(nrow = ncol(pca$rotation), ncol = 4))
names(scores) <- c("AIC", "BIC", "RMSE - Train", "RMSE - Test")
for(i in 1:ncol(pca$rotation)){
  components = names(pca.data)[1:i]
  model.pca <- lm(Crime ~ ., training_set[c("Crime", components)])
  scores$AIC[i] <- AIC(model.pca)
  scores$BIC[i] <- BIC(model.pca)
  
  preds <- predict(model.pca, newdata = training_set[c("Crime", components)])
  scores$`RMSE - Train` <- rmse(actual = training_set$Crime,
                                predicted = preds)
  preds <- predict(model.pca, newdata = test_set[c("Crime", components)])
  scores$`RMSE - Test` <- rmse(actual = test_set$Crime, predicted = preds)
}

plot(scores$AIC, xlab = "Number of Components", type = 'b',
     main = "AIC Score vs Number of Components")
plot(scores$BIC, xlab = "Number of Components", type = 'b',
     main = "BIC Score vs Number of Components")
plot(scores$`RMSE - Train`, xlab = "Number of Components", type = 'b',
     main = "Training RMSE vs Number of Components")
plot(scores$`RMSE - Test`, xlab = "Number of Components", type = 'b',
     main = "Test RMSE vs Number of Components")


# Compare 5-component model with 15-component and non-PCA model
components = names(pca.data)[1:5]
model.5comp <- lm(Crime ~ ., training_set[c("Crime", components)])
model.scores = scores[c(5,15),]

# from HW05: Ed, Po1, Nw, Ineq, Prob were significant factors,
# so we build a model from only those features
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)
feats <- c("Ed", "Po1", "NW", "Ineq", "Prob")
model.old <- lm(Crime ~ ., training_set[c("Crime", feats)])

# I'm using same seed and split as last week's assignment
# below are the scores from then
old.scores <- c(497.33, 508.61, 166.09, 403.51)

model.scores <- rbind(model.scores, old.scores)
model.scores = cbind(model.scores, c("5-Component", "15-Component", "Old Model"))
names(model.scores)[5] = "Model"

barplot(model.scores$AIC, names.arg=model.scores$Model, xlab="Model",
        ylab="AIC", main="AIC Scores")
barplot(model.scores$BIC, names.arg=model.scores$Model, xlab="Model",
        ylab="BIC", main="BIC Scores")
barplot(model.scores$`RMSE - Train`, names.arg=model.scores$Model, xlab="Model",
        ylab="RMSE - Train", main="Training RMSE")
barplot(model.scores$`RMSE - Test`, names.arg=model.scores$Model, xlab="Model",
        ylab="RMSE - Test", main="Test RMSE")


# final prediction based on entire dataset
model.final <- lm(Crime ~ ., pca.data[c("Crime", components)])
summary(model.final)
AIC(model.final)
BIC(model.final)

# PCA back to original feature space
beta0 <- model.final$coefficients[1]
betas <- model.final$coefficients[-1]
alphas <- pca$rotation[,1:5] %*% betas # only using first 5 components

originalAlpha <- alphas/sapply(dataset[,-16],sd)
originalBeta0 <- beta0 - sum(alphas*sapply(dataset[,-16],mean)/sapply(dataset[,-16],sd))

pred.point <- c(M = 14.0,
                So = 0,
                Ed = 10.0,
                Po1 = 12.0,
                Po2 = 15.5,
                LF = 0.640,
                M.F = 94.0,
                Pop = 150,
                NW = 1.1,
                U1 = 0.120,
                U2 = 3.6,
                Wealth = 3200,
                Ineq = 20.1,
                Prob = 0.04,
                Time = 39.0)

prediction <- t(pred.point) %*% originalAlpha + originalBeta0