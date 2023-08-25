# Import dataset
dataset = read.csv('uscrime.txt', sep="\t")


# Splitting into Training and Test sets
library(caTools)
set.seed(101)
split = sample.split(dataset$Crime, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

plot(dataset)

model.base <- lm(Crime ~ ., training_set)
print("Base Model Stats")
summary(model.base)
base.aic <- AIC(model.base)
base.bic <- BIC(model.base)

# we see Ed, Po1, Nw, Ineq, Prob are significant factors,
# so we build a model from only those features

feats <- c("Ed", "Po1", "NW", "Ineq", "Prob")

model.refine <- lm(Crime ~ ., training_set[c("Crime", feats)])
print("Refined Model Stats")
summary(model.refine)
refine.aic <- AIC(model.refine)
refine.bic <- BIC(model.refine)


# compare models
print("AIC comparison")
print(exp((refine.aic - base.aic)/2))
print("BIC Comparison")
print(abs(refine.bic - base.bic))


# compare predictive accuracy
library(Metrics)
print("Base Model RSME")
preds <- predict(model.base, newdata = training_set)
print(paste("Training Set: ", rmse(actual = training_set$Crime,
                                   predicted = preds)))
preds <- predict(model.base, newdata = test_set)
print(paste("Test Set: ", rmse(actual = test_set$Crime, predicted = preds)))

print("Refined Model RSME")
preds <- predict(model.refine, newdata = training_set[c("Crime", feats)])
print(paste("Training Set: ", rmse(actual = training_set$Crime,
                                   predicted = preds)))
preds <- predict(model.refine, newdata = test_set[c("Crime", feats)])
print(paste("Test Set: ", rmse(actual = test_set$Crime, predicted = preds)))


# final prediction based on entire dataset
model.final <- lm(Crime ~ ., dataset[c("Crime", feats)])
pred.point <- c(10.0, 12.0, 1.1, 20.1, 0.04) # only using Ed, Po1, NW, Ineq, Prob
predict(model.final, newdata = pred.point)