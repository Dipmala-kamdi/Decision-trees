fraud<- read.csv(choose.files())
View(fraud)

install.packages("c50")
library(C50)

install.packages("party")
library(party)

str(fraud)
hist(fraud$Taxable.Income)

x<- fraud$Taxable.Income
fraud_income<- ifelse(x<=30000,"risky","good")
View(fraud_income)

fraud$Taxable.Income <- fraud_income
View(fraud)

library(rpart)
?rpart
#model_1<- C5.0(fraud[], fraud$Taxable.Income)
str(fraud)
fraud$Taxable.Income<- as.factor(fraud$Taxable.Income)
model_1<- C5.0(fraud[], fraud$Taxable.Income)
plot(model_1)

pred<- predict(model_1,fraud)

mean(fraud$Taxable.Income==pred)

##################### by using train and test data ############

fraud_good <- fraud[fraud$Taxable.Income=="good",]  #476 object
fraud_risky <- fraud[fraud$Taxable.Income=="risky",] # 124 object
fraud_train <- rbind(fraud_good[1:60,],fraud_risky[1:60,])
View(fraud_train)
fraud_test <- rbind(fraud_good[61:120,],fraud_risky[61:120,])
View(fraud_test)

model_train <- C5.0(fraud_train[],fraud_train$Taxable.Income)
plot(model_train)

plot_tree<- tree(Taxable.Income~. , data = fraud_test)
plot(plot_tree)
?tree
plot_tree1<- tree(Taxable.Income~. , data = fraud_test, method="recursive.partition")
plot(plot_tree1)

# to ind accuracy of training data
pred_train <- predict(model_train,fraud_train)

mean(fraud_train$Taxable.Income==pred_train) # 100% Accuracy

