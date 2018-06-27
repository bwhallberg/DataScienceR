library(caTools)
library(ROCR)
theSplit = sample.split(eligible$Indicator, SplitRatio = 0.7)
TrainSet = subset(eligible, theSplit == TRUE)
TestSet = subset(eligible, theSplit == FALSE)
fit.allstar <- glm(Indicator ~ allstar, data = TrainSet, family = "binomial")
summary(fit.allstar)
predTrain <- predict(fit.allstar, type = "response")
table(TrainSet$Indicator, predTrain > 0.5)

#  Not this does very good with those not inducted, but we miss most of those that were inducted

ROCRpred <- prediction(predTrain, TrainSet$Indicator)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))

#  See what we get if we use a different point from this ROCR plot

table(TrainSet$Indicator, predTrain > 0.2)

# Better but not great.  Let's add predictors

fit.all <- glm(Indicator ~ allstar + hit + hr + rbi + run + ba + w + gpitch + sopitch, data = TrainSet, family = "binomial")
summary(fit.all)

#  Lots of extras, let's remove some and see how it changes

fit.all1 <- glm(Indicator ~ allstar + hit + hr + rbi + run + w + gpitch + sopitch, data = TrainSet, family = "binomial")
summary(fit.all1)

#  Drop another

fit.all2 <- glm(Indicator ~ allstar + hr + rbi + w, data = TrainSet, family = "binomial")
summary(fit.all2)
predTrain <- predict(fit.all2, type = "response")
table(TrainSet$Indicator, predTrain > 0.5)

#  Not this does very good with those not inducted, but we miss most of those that were inducted

ROCRpred <- prediction(predTrain, TrainSet$Indicator)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))