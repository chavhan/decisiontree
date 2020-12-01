############################## Decision tree algorithm for assignmnet ####################
install.packages("party")
library(party)
library(caret)
data("iris")
partition <- createDataPartition(iris$Species,p=.75,list = F)  ## creating test and train data
train_iris <- iris[partition,]
test_iris <- iris[-partition,]

model_ctree <- ctree(train_iris$Species~.,data = train_iris)
model_ctree
summary(model_ctree)

## accuracy mechanism for train data 
ctree_train_pred <- predict(model_ctree,train_iris[,-5])
ctree_pred
tab1 <- table(ctree_train_pred,train_iris$Species)
train_accuray <- sum(diag(tab1))/sum(tab1)
train_accuray

## accuracy mechanism for test data 
ctree_test_pred <- predict(model_ctree,test_iris[,-5])
ctree_test_pred
tab2 <- table(ctree_test_pred,test_iris$Species)
test_accuray <- sum(diag(tab2))/sum(tab2)
test_accuray