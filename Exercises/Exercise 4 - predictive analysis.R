########################################################################
#                                                                      #
# Statistics 101                                                       #
# Exercise 4: Predictive analyses                                      #
#                                                                      #  
########################################################################

# Load packages needed for analyses
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(rattle)
library(randomForest)
library(gbm)

# Set seed so that we all have the same results
set.seed(42)

###################### Import Data #####################################

data_abpred <- read.csv("data_workshop_part3.csv")
data_abpred$users <- factor(data_abpred$users, levels=c(1,5,10,50))
data_abpred$system <- factor(data_abpred$system, levels = c("KittenInc", "DragonInc"))

summary(data_abpred)


###################### Predicting with regression ######################

# Split the data in a training and testing part
# For this model, we are going to predict cycle duration with the
# regression model we ended up with in the previous exercise (fit5;
# with the three way interaction system*users*process_B)

inTrain <- createDataPartition(y = data_abpred$cycle, p = 0.6, list=FALSE)
train_ab <- data_abpred[inTrain,]
test_ab <- data_abpred[-inTrain,]

# Create a feature plot that shows all the two-way relations between
# the preditors and the outcome variable

featurePlot(x=train_ab[,c("system", "users", "process_B")],
            y = train_ab$cycle,
            plot="pairs")

# You can also create one plot that shows all the three-way
# relations that exist between the variables we are including in
# the model.

ggplot(train_ab, aes(process_B, cycle, colour = system)) +
  geom_point(size=3, alpha = .7) +
  facet_grid(users ~.)

# Train your model using your training data and the final regression model.
# This syntax automatically uses bootstrapping (resampling) to cross-validate
# your model. It averages out over all bootstraps.

modFit <- train(cycle ~ system + users + process_B + system*users*process_B, 
                method = "lm", data = train_ab)

# Let look at some results. The first line shows you the final model fit after
# resampling. You can use different methods and compare these fit indices to
# see which worked better.
# The second line gives you the regression output, and shows you which predictors
# add significantly to the model. With prediction, you can also choose to keep
# non-significant predictors in your model, since they still add to the 
# accuracy of your predictions.
print(modFit)
summary(modFit)

# To see the separate results for each bootstrap use:
modFit$resample

# To see how well the model is able to predict other values, we now use
# the test_ab dataframe to predict new outcome values.
pred <- predict(modFit, test_ab)

# To plot the observed outcome values from the test data against the
# predicted values, use the syntax below. For perfect prediction,
# all points should be on the diagonal line.
qplot(cycle, pred, colour = system,data=test_ab) +
  ylab("Predicted values") + xlab("Observed values") +
  geom_abline(intercept=0, slope=1, linetype="dashed")

# How well does this model predict new outcome values?

# We can also use the MSE and RMSE to assess model accuracy.
# Lower Mean Square Error and Root MSE values indicate less
# mismatch between predicted and observed values.

# Compare MSE and RMSEA accross models (with the same outcome) 
# to find the one that predicts best.

# MSE
MSE <- 1/length(pred)*sum((pred-test_ab$cycle)^2)
MSE 

# RMSE
sqrt(MSE)

# Let's now try to predict cycle from two variables that
# are not really related to cycle duration: process_A and
# process_C, to see what happens when you estimate a bad
# prediction model.

modFit2 <- train(cycle ~ process_A + process_C, 
                method = "lm", data = train_ab)

# Take a look at the output from the model. Compare the R-squared of
# this model to the R-squared of the previous model (from the print output)
print(modFit2)
summary(modFit2)

# Lets compute and plot new predicted values against the observed outcome values
# of the test data.
pred2 <- predict(modFit2, test_ab)

qplot(cycle, pred2, colour = system,data=test_ab) +
  ylim(c(0,20)) +
  ylab("Predicted values") + xlab("Observed values") +
  geom_abline(intercept=0, slope=1, linetype="dashed")

# Does the model predict new values well?
# Lets compare this model to the previous one using the MSE and RMSE
# values.

# MSE
1/length(pred2)*sum((pred2-test_ab$cycle)^2)

# RMSE
sqrt(1/length(pred2)*sum((pred2-test_ab$cycle)^2))

# What do you conclude? Which model predicts better?

###################### Predicting with trees ######################

# Let's try to predict a categorical outcome, users, with a tree model
# First we split the data on the outcome variable users.

inTrain2 <- createDataPartition(y = data_abpred$users, p = 0.6, list=FALSE)
train_ab2 <- data_abpred[inTrain2,]
test_ab2 <- data_abpred[-inTrain2,]

# Create a feature plot of outcome variable users and all predictors in the data
featurePlot(x=train_ab2,
            y = train_ab2$users,
            plot="pairs")

# Run the model with the following syntax:
modFit3 <- train(users ~ ., 
                method = "rpart", data = train_ab2, tuneLength = 10)

# cp is the cost-complexity parameter RPART uses to figure out what 
# tree fits the data best without becoming overly complex. By default,
# the function estimates a model for three levels of cp. By adding
# tuneLength to the command above, we now estimate 10 levels of cp.
# The model with the highest Accuracy get's chosen as the best tree.
print(modFit3)

# To plot the tree, and understand which variables it uses to predict
# number of users, use:
finMod3 <- modFit3$finalModel
fancyRpartPlot(finMod3)

# Lets compute new predicted values and make a crosstable with the observed outcome values
# of the test data.
pred3 <- predict(modFit3, newdata=test_ab2)
table(pred3, test_ab2$users)

# Where does the model make mistakes in classifying observations?

# To test the accuracy of the prediction model, we use the
# formulas from the presentation.

correct = sum(test_ab2$users == pred3)
false = length(pred3) - correct
total = length(pred3)

# Misclassification error
false/total

# Gini index
1-((false/total)^2 + (correct/total)^2)

# Information
-(false/total*log2(false/total) + correct/total * log2(correct/total))

# Does this model have pure predictions? Remember the more pure, the closer
# these three accuracy indices are to 0.

###################### Predicting with random forests #############

# Lets now try a random forest to predict the number of users from all
# other variables in the dataset. 

inTrain3 <- createDataPartition(y = data_abpred$users, p = 0.6, list=FALSE)
train_ab3 <- data_abpred[inTrain3,]
test_ab3 <- data_abpred[-inTrain3,]

# Create a feature plot.
featurePlot(x=train_ab3,
            y = train_ab3$users,
            plot="pairs")

# Run the model with the syntax below.
modFit4 <- train(users ~ ., 
                method = "rf", data = train_ab3, prox=TRUE)

# Print the model results, here you can see that a model with 6
# randomly chosen predictors per random tree leads to the highest
# accuracy (0.952).
print(modFit4)

# To see how well this model is able to predict values in the training
# data, we use the syntax below. Here we see that the error rate is very
# low. For the training data, this model only misclassifies one case.
modFit4$finalModel

# With the syntax below, you can see where the class centers are estimated
# by the model for any combination of two predictors. Here we look at
# cycle duration and process_B duration as predictors of number of users.
trainplot <- classCenter(train_ab3[,c("cycle", "process_B")],
                         train_ab3$users, modFit4$finalModel$prox)
trainplot <- as.data.frame(trainplot)
trainplot$users <- rownames(trainplot)
qplot(cycle, process_B, col=users, data=train_ab3) +
  geom_point(aes(x=cycle, y=process_B,col=users), size = 6, shape = 4, data=trainplot)

# Lets compute new predicted values and make a crosstable with the observed outcome values
# of the test data. 
pred4 <- predict(modFit4, newdata=test_ab3)
table(pred4, test_ab3$users)

# Where does the model make mistakes in classifying observations?

# To test the accuracy of the prediction model, we use the
# formulas from the presentation.

correct = sum(test_ab3$users == pred4)
false = length(pred4) - correct
total = length(pred4)

# Misclassification error
false/total

# Gini index
1-((false/total)^2 + (correct/total)^2)

# Information
-(false/total*log2(false/total) + correct/total * log2(correct/total))


## Which model would you use? Regular Trees or a Random Forest?

###################### Predicting with Boosting ###################

# Finally, lets try to predict the cycle duration outcome, using regression
# with boosting.

inTrain4 <- createDataPartition(y = data_abpred$cycle, p = 0.6, list=FALSE)
train_ab4 <- data_abpred[inTrain4,]
test_ab4 <- data_abpred[-inTrain4,]

# Run the model with the syntax below.
modFit5 <- train(cycle ~ ., 
                method = "gbm", data = train_ab4, verbose = FALSE)


# Let look at some results. The first line shows you the final model fit after
# resampling and boosting. The n.trees collumn shows you the number of boosting-steps
# were taken. The model with the lowest RMSE is chosen as the best fitting model.

# The second line gives you an overview of the variables in the boosting model,
# and which variables had the highest relative influence on the prediction of
# cycle duration.

print(modFit5)
summary(modFit5)

# To see how well the model is able to predict other values, we now use
# the test_ab dataframe to predict new outcome values.
pred5 <- predict(modFit5, test_ab4)

# To plot the observed outcome values from the test data against the
# predicted values, use the syntax below. For perfect prediction,
# all points should be on the diagonal line.

qplot(cycle, pred5, colour = system,data=test_ab4) +
  ylab("Predicted values") + xlab("Observed values") +
  geom_abline(intercept=0, slope=1, linetype="dashed")

# How well does this model predict new outcome values?

# We can also use the MSE and RMSE to assess model accuracy.
# Lower Mean Square Error and Root MSE values indicate less
# mismatch between predicted and observed values.

# Compare MSE and RMSEA accross models (with the same outcome) 
# to find the one that predicts best.

# MSE
MSE2 <- 1/length(pred5)*sum((pred5-test_ab4$cycle)^2)
MSE2

# RMSE
sqrt(MSE2)

# Which of the two regression prediction models would you pick? The
# linear regression model above, or the final Boosting model? Compare
# the MSE and RMSE values.




