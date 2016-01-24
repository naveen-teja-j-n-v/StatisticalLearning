library(MASS)
library(ISLR)
## Simple Linear Regression
## Dataset Boston Data
names(Boston)

## Building Linear Model between variables:
## medv: median value of owner-occupied homes in \$1000s.
## lstat: lower status of the population (percent)
## Plot a graph between desried variables to see the relationship
plot(medv~lstat, Boston)
model1 <- lm(medv ~ lstat, data = Boston)

## to analyze the model print sumamry
summary(model1)
abline(model1, col= "blue")

## to determine the confidence interval of the model
## use confint
confint(model1)

## Use Predict on 3 new values of lstat and ask for confidence interval.
## Below function gives the fit, lower confidence interval and upper confidence interval
predict(model1, data.frame(lstat=c(5,10,15)), interval = "confidence")

## Multiple Linear Regression
## model medv as dependent on lstat and age
model2 <- lm(medv ~ lstat + age, data = Boston)
summary(model2)

## model medv as dependent on all features
model3 <- lm(medv ~ ., data = Boston)
summary(model3)

## From summary of model3, age and indus are not statistically significant.
model4 <- update(model3, ~.-age-indus)
summary(model4)

## Non-linear terms and interactions
## Interaction is represented by '*'
model5 <- lm(medv ~ lstat * age, data = Boston)
summary(model5)
model6 <- lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(model6)

## Ploting data and model6
attach(Boston)
plot(medv ~ lstat)
points(lstat, fitted(model6), col= "blue", pch=21)

## Generating model with 4 degree polynomial of lstat
model7 <- lm(medv ~ poly(lstat, 4))
summary(model7)
points(lstat, fitted(model7),col="red", pch = 21)

## Qualitative Predictors
fix(Carseats)
names(Carseats)

## Model with interactions
model8 <- lm(Sales ~.+Income:Advertising+Age:Price, Carseats)
summary(model8)

## ShelveLoc is a qualitative variable.
## contrasts will tell how are qualitative variables used in models
contrasts(Carseats$ShelveLoc)
