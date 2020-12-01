#################### Decision tree on company data #######################
library(party)
library(C50)
library(caret)

names(Company_Data)
dim(Company_Data)
str(Company_Data)
company_new_data <- Company_Data

company_new_data$ShelveLoc <- as.numeric(as.factor(company_new_data$ShelveLoc))
company_new_data$Urban <- as.numeric(as.factor(company_new_data$Urban))
company_new_data$US <- as.numeric(as.factor(company_new_data$Urban))
head(company_new_data)

## dividing in train and test data 
partition <- createDataPartition(company_new_data$Sales,p=.75,list = F)
train_data <- company_new_data[partition,]
test_data <- company_new_data[-partition,]

##c50_model <- C5.0(train_data$Sales~.,data = train_data)    ## not working 
ctree_model <- ctree(train_data$Sales~.,data = train_data)
ctree_model

ctree_pred <- predict(ctree_model,train_data[,-1])
ctree_pred
ctree_table <- table(ctree_pred,train_data$Sales)
ctree_table

## function for accuracy 
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
sale_accuracy <- accuracy(ctree_table)
sale_accuracy

install.packages("MLmetrics")
library(MLmetrics)
sale_accuracy <- R2_Score(y_pred = predict(ctree_model),y_true = train_data$Sales)
sale_accuracy
?R2_Score

## bagging on company data 
sale_acc <- c()
for(i in 1:100)
{
  print(i)
  bag_partition <- createDataPartition(company_new_data$Sales,p=.75,list = F)
  bag_train_data <- company_new_data[bag_partition,]
  bag_test_data <- company_new_data[-bag_partition,]
  bag_ctree_model <- ctree(bag_train_data$Sales~.,data = bag_train_data)
  bag_sale_accuracy <- R2_Score(y_pred = predict(bag_ctree_model),y_true = bag_train_data$Sales)
  sale_acc <- c(sale_acc,bag_sale_accuracy)
}
summary(sale_accuracy)
sale_acc