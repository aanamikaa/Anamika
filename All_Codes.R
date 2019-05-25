library(caret)
library(glmnet)
library(ISLR)
ISLR::Credit
A = data.frame(Credit)
sf = sample(2,nrow(A),replace = TRUE,prob=c(0.7,0.3))
tsd=A[sf==1,]
trd=A[sf==2,]
#-------------Simple Model----------
m1 = lm(Limit ~ Income + Rating + Balance, data= trd)
pred = predict(m1,tsd)
#---------------Ridge Model---------
tc = trainControl(method = "cv", number = 10, verboseIter = T)
A = na.omit(A)
model1 = train(Limit ~ Income + Rating + Balance, method = "glmnet", data = A, trControl = tc,
               tuneGrid = expand.grid(alpha = 0, lambda = seq(0,500,length=50)))
pred1 = predict(model1,tsd)
Q2 = cbind(pred1,pred,tsd$Limit)
  Q2
  
  #===========Elastic Net=========================
  library(caret)
  library(glmnet)
  set.seed(123)
  tc=trainControl(method="cv",number=10,verboseIter = T)
  A=data.frame(iris)
  model3=train(Sepal.Length ~ ., method="glmnet",data=A,trcontrol=tc,
               tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.1,1,length=7)))
  model3=train(Sepal.Length ~ ., method="glmnet",data=A,trControl=tc,
               tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.1,1,length=7)))
  model3
  summary(model3)
  pred = predict(model3, trd)
  
  cbind(pred,tsd$Sepal.Length)
  
  #=========================Classification - Logistic============
  library(glmnet)
library(ISLR)
str(ISLR::Credit) 
A = data.frame(Credit)
fivenum(A$Income)
A$NEWINCOME = ifelse(A$Income>33.11,1,0)
sf = sample(2,nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
model1 =glm(NEWINCOME ~ Limit+Rating, data = trd)
pred = predict(model1,tsd)
pred1 = ifelse(pred<=0.5,0,1)
cbind(pred1,tsd$NEWINCOME)

#=============Naive Bayes================

library(ISLR)
library(naivebayes)
library(e1071)
A = data.frame(iris)
str(A)
sf = sample(2,nrow(A), replace=TRUE,prob=c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf ==2,]
model_nb = naive_bayes(Species ~ .,data = trd)
model_e1 = naiveBayes(Species ~ .,data = trd)
pred_nb = predict(model_nb,tsd)
pred_nb
table(pred_nb,tsd$Species)
pred_e1 = predict(model_e1,tsd)
pred_e1
table(pred_e1,tsd$Species)

#======with another dataset=======
A = data.frame(Credit)
str(A)
sf= sample(2, nrow(A), replace = TRUE, prob=c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
model2_nb =  naive_bayes(Ethnicity ~ .,data = trd)
model2_e1 = naiveBayes(Ethnicity ~ .,data = trd)

pred1_nb = predict(model2_nb,tsd)
pred1_nb
table(pred1_nb, tsd$Ethnicity)

#========Result: Misclassification is larger===========

#=========Another dataset===============

A = data.frame(cars)
str(A)

library(MASS)
A = data.frame(Cars93)
str(A)

#========== Decision Tree==============
library(tree)
A = data.frame(iris)
library(tree)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
m1 = tree(Species ~ ., data = trd)
plot(m1)
text(m1)
pred = predict(m1, tsd)
cbind(pred, tsd$Species)

table(pred, tsd$Species)

m2 = naive_bayes(Species ~ .,data = trd)
pred1 = predict(m2, tsd)
cbind(pred1, tsd$Species)

Q = ifelse(pred[,1]>0.5,"Setosa", ifelse(pred[,2] > 0.5, "versicolor", "virginica"))
Q1 = table(Q, tsd$Species)
sum(diag(Q1))
sum(diag(Q1))/nrow(tsd)
(sum(diag(Q1))/nrow(tsd))*100
100 - (sum(diag(Q1))/nrow(tsd))*100


#=============Another Dataset==============

library(ISLR)
ISLR::Credit

A = data.frame(Credit)
str(A)

sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]

m1 = lm(Income ~ ., data = trd)
pred = predict(m1, tsd)
pred
cbind(pred, tsd$Income)

m2 = tree(Income ~ ., data = trd)
pred1 = predict(m2, tsd)
pred1
cbind(pred1, tsd$Income)
m2
plot(m2)
text(m2)
A


#=======================Decision tree vs Random Forest Model=========

library(randomForest)
library(ISLR)
library(tree)
set.seed(243)
A = data.frame(iris)
sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
model_dt = tree(Species ~ .,data = trd)
model_rf = randomForest(Species ~ .,data = trd)

pred_dt = predict(model_dt, tsd)
pred_dt
pred_rf = predict(model_rf, tsd, type = "prob")
pred_rf
Q = ifelse(pred_dt[,1] > 0.5, "Setosa", ifelse(pred_dt[,2] > 0.5, "Versicolor", "Vergicolor"))
Q1 = ifelse(pred_rf[,1] > 0.5, "Setosa", ifelse(pred_dt[,2] > 0.5, "Versicolor", "Vergicolor"))
 table (Q, tsd$Species)
T1 = table (Q1, tsd$Species)
T1

#=========== another dataset===========

library(MASS)
A = data.frame(Cars93)
sf = sample(2, nrow(A), replace = TRUE, prob = c(0.7,0.3))
trd = A[sf == 1,]
tsd = A[sf == 2,]
model_dt = tree(Horsepower ~ .,data = tsd)
model_rf = randomForest(Species ~ .,data = trd)


#===============KNN Model================

library(caret)
library(ISLR)
data("Hitters")
A = data.frame(Hitters)
A = na.omit(A)
colnames(A)
str(A)
set.seed(123)
sf = sample(2, nrow(A), replace = TRUE, prob = c(0.6,0.4))
trd = A[sf == 1,]
tsd = A[sf == 2,]
tc = trainControl(method = 'cv', number = 10)
set.seed(123)
model1 = train(League ~ ., data = trd, method = 'knn', trControl = tc, preProc = c("center","scale"))
PR = predict(model1, newdata = tsd)
PR

###################ASSICIATION RULE MINING############################

library(amodel1)
library(amodel1Viz)
libray(datasets)
library(arules)
data(Groceries)

####################PCA##########

install.packages("devtools")
install.packages("ggbiplot")
library(devtools)
install_github("vqv/ggbiplot")















