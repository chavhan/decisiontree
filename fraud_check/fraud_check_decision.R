################### Decision tree on fruad check ####################
names(Fraud_check)
f_check <- Fraud_check

## changing categorical variable into numeric
f_check$Undergrad <- as.numeric(as.factor(f_check$Undergrad))
f_check$Marital.Status <- as.numeric(as.factor(f_check$Marital.Status))
f_check$Urban <- as.numeric(as.factor(f_check$Urban))

str(Fraud_check)
str(f_check)
?filter
risky_data <- filter(f_check, f_check$Taxable.Income <= 30000)
head(risky_data)
good_data <- filter(f_check, f_check$Taxable.Income > 30000)
head(good_data)

good_data$Taxable.Income <- 'good'
risky_data$Taxable.Income <- 'risky'

final_data <- rbind(good_data,risky_data)
final_data$Taxable.Income <- as.factor(final_data$Taxable.Income)
dim(final_data)
head(final_data)
str(final_data)

library(C50)
library(caret)
partition <- createDataPartition(final_data$Taxable.Income,p=.75,list = F)
train_data <- final_data[partition,]
test_data <- final_data[-partition,]

## decision tree model 
model <- C5.0(train_data$Taxable.Income~.,data = train_data)


## accuracy of train data 
train_pred <- predict(model,train_data[,-3])
train_pred
train_table <- table(train_pred,train_data$Taxable.Income)
train_accuray <- sum(diag(train_table))/sum(train_table)
train_accuray

## accuracy on test data 
test_pred <- predict(model,test_data[,-3])
test_pred
test_table <- table(test_pred,test_data$Taxable.Income)
test_accuray <- sum(diag(test_table))/sum(test_table)
test_accuray
