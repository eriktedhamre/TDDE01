# 2.1
setwd("~/TDDE01/lab2")
data.credit <- data.frame(read.csv("creditscoring.csv"))
n=dim(data.credit)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data.credit[id,]
train$Name <- NULL
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data.credit[id2,]
valid$Name <- NULL
id3=setdiff(id1,id2)
test=data.credit[id3,]
test$Name <- NULL

#1.2
tree.fit.dev <- tree(good_bad ~., data = train, split = "deviance")
tree.fit.gini <- tree(good_bad ~., data = train, split = "gini")

pred.dev.train <- predict(tree.fit.dev, newdata = train, type="class")
table(train$good_bad, pred.dev.train)

pred.dev.test <- predict(tree.fit.dev, newdata = test, type="class")
table(test$good_bad, pred.dev.test)

pred.gini.train <- predict(tree.fit.gini, newdata = train, type="class")
table(train$good_bad, pred.gini.train)

pred.gini.test <- predict(tree.fit.gini, newdata = test, type="class")
table(test$good_bad, pred.gini.test)

#Deviance provides better result based on the diagonal of the confusion matrices

