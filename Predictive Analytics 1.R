# Task 1 

# a: What is the prediction for x = 3 if the linear regression model is used?
# Use a calculator (or use R as a calculator) to answer this question.
# The data
x=1:5
y=c(1.88,4.54,10.12,9.14,11.26)
n=length(x)
# Fit model
a1 = (n * sum(x * y) - sum(y) * sum(x)) / (n * sum(x^2) - sum(x)^2)
a0=mean(y)-a1*mean(x)
# Prediction for x=3
a0+a1*3

# b:  Use the lm-function in R to fit the same model as in a).
# Use the predict-function to compute the prediction of x = 3.

# The data
x=1:5
y=c(1.88,4.54,10.12,9.14,11.26)
# Fit model
reg1=lm(y~x)
# Prediction for x=3
predict(reg1,data.frame(x=3))

# c: What is the prediction for x = 3 if KNN with K = 3 is used? For K = 1 and K = 5?
# Use a calculator (or use R as a calculator) to answer this question.

# KNN for K=3
(4.54+10.12+9.14)/3

# KNN for K=1
10.12

# KNN for K=5
mean(y)

# d: Consider the R-function:
knn=function(x0,x,y,K=20)
{
  d=abs(x-x0)
  o=order(d)[1:K]
  ypred=mean(y[o])
  return(ypred)
}

# Experiment with R to see and explain what happens in each row of the function if x0 = 3 and K = 3.

# if x0 = 3 and K = 3
K=3
x0=3
x=1:5
y=c(1.88,4.54,10.12,9.14,11.26)
d=abs(x-x0)
d

K=3
x0=3
x=1:5
y=c(1.88,4.54,10.12,9.14,11.26)
d=abs(x-x0)
d # Distances between x0 and the different x-values

o=order(d)[1:K]
o # Indices of the three smallest distances

ypred=mean(y[o])
ypred # Average of the y's for the 3 observations with smallest d



# Task 2
# a: Load, summarize, and read the help to the dataset College from the R-package ISLR by

# Load, summarize the dataset College from the R-package ISLR
library(ISLR)
summary(College)

# b: Divide the data into a 50/50 training and test data.

# divide the data into a 50/50 training and test data
set.seed(123)
n=nrow(College)
train_indicator=sample(1:n,size=floor(n/2))
train=College[train_indicator,]
test=College[-train_indicator,]

# c: Fit a linear prediction model, on the training data, for number of applications,
# Apps, with the predictors Private and Accept by


# fit a linear prediction model on the training data
# for number of applications, Apps with the predictores private and accept
m1=lm(Apps~Private+Accept,data=train)
summary(m1)

# d: Compute training-MSE

# compute training MSE
pred_train=predict(m1)
mse_train = mean((train$Apps - pred_train)^2)
mse_train

# e:  redo c) and d) but by only using Accept as the predictor.
#Fit the model and compute the training-MSE.

# redo c and d only using accept as the predictor
m2=lm(Apps~Accept,data=train)
pred_train2=predict(m2)
mse_train2 = mean((train$Apps - pred_train2)^2)
mse_train2 # R^2 increases with the number of X-variables, even if irrelevant.

# f: compute the test-MSE for the two regression models fitted in c) and e).
#This is how you do it for the one in c)

# test MSE for the two regression model in c and e
pred_test=predict(m1,newdata = test)
mse_test = mean((test$Apps - pred_test)^2)

mse_test

pred_test2=predict(m2,newdata = test)
mse_test2=mean((test$Apps-pred_test2)^2)
mse_test2
# The model with both Private and Accept gave more exact test predictions.
# A small model can achieve a smaller MSE on the test set than a large model.

# g: Comment on the relationships between the MSE’s between the models in trainingset and testset respectively.

# The test MSE for both models is larger than their training counterparts.
# This is expected as linear regression minimizes training MSE.

# h: Use KNN to predict Apps. Compare the test-MSE with the linear regression predictions.

knn=function(x0,x,y,K=3)
{
  dist=abs(x-x0)
  odist=order(dist)
  y0=mean(y[odist[1:K]])
  return(y0)
}
x0=test$Accept
x=train$Accept
y=train$Apps
pred_test3=apply(as.matrix(x0),1,knn,x=x,y=y)
mse_test3=mean((test$Apps-pred_test3)^2)
mse_test3




