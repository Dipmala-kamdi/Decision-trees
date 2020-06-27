install.packages("c50")
library(C50)

install.packages("party")
library(party)

company<- read.csv(choose.files())
View(company)
str(company)
min(company$Sales)
max(company$Sales)
hist(company$Sales)

x<- company$Sales
comp_sales<- ifelse(x<=5,"5",ifelse(x<=10,"10","15"))
View(comp_sales)

company$Sales<- comp_sales
company$Sales <- as.factor(company$Sales)

sales_5 <- company[company$Sales=="5",]
sales_10 <- company[company$Sales=="10",]
sales_15 <- company[company$Sales=="15",]
comp_train<- rbind(sales_5[1:77,],sales_10[1:77,],sales_15[1:77,])
View(comp_train)

model_1<- C5.0(company[], company$Sales)
plot(model_1)

## checking for accuracy
pred<- predict(model_1,company)

mean(company$Sales==pred) #100
