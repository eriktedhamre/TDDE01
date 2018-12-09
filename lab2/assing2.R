library("e1071")
library("MASS")
library("tree")
library("ggplot2")
setwd("~/TDDE01/lab2")
data.credit <- data.frame(read.csv("creditscoring.csv"))
n=dim(data.credit)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data.credit[id,]
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data.credit[id2,]
id3=setdiff(id1,id2)
test=data.credit[id3,]

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

fit=tree(good_bad~., data=train)
set.seed(12345)
max.size <- 15
trainScore=rep(0,max.size)
testScore=rep(0,max.size)
for(i in 2:max.size) {
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:max.size, trainScore[2:max.size], type="b", col="red", ylim=c(250,400))
points(2:max.size, testScore[2:max.size], type="b", col="blue")

# 4 is very nice thank you sir

finalTree=prune.tree(fit, best=4)
plot(finalTree)
# Depth 3 from plot(finalTree)
Yfit=predict(finalTree, newdata=valid, type = "class")
table(valid$good_bad,Yfit)
mcr.tree <- mean(valid$good_bad != Yfit)
#Variables actually used in tree construction:
#[1] "savings"  "duration" "history" 

fit.bayes=naiveBayes(good_bad~., data=train)
Yfit.bayes.train=predict(fit.bayes, newdata=train)
Yfit.bayes.test=predict(fit.bayes, newdata=test)
table(Yfit.bayes.train, train$good_bad)
table(Yfit.bayes.test, test$good_bad)

createROCmatrix <- function(pred, pi.vector){
  tpr.vector = numeric()
  fpr.vector = double()
  
  for (pi.value in pi.vector) {
    predict.pi <- ifelse(pred[,2] > pi.value, "good", "bad")
    TP <- length(which(predict.pi == "good" & test$good_bad == "good"))
    TN <- length(which(predict.pi == "bad" & test$good_bad == "bad"))
    FP <- length(which(predict.pi == "good" & test$good_bad == "bad"))
    FN <- length(which(predict.pi == "bad" & test$good_bad == "good"))
    
    tpr.vector <- append(tpr.vector, TP/(TP+FN))
    fpr.vector <- append(fpr.vector, FP/(FP+TN))
  }
  return(data.frame(pi.vector, tpr.vector, fpr.vector))
}

Yfit.tree.test=predict(finalTree, newdata=test)
pi.vector <- seq(0.05, 0.95, 0.05)

tree.predict <- predict(finalTree, newdata = test)
bayes.predict <- predict(fit.bayes, newdata = test, type = "raw")

tree.ROC.matrix <- createROCmatrix(tree.predict, pi.vector)
bayes.ROC.matrix <- createROCmatrix(bayes.predict, pi.vector)

ggplot(data = NULL, aes(col = classifier)) +
  geom_point(data = tree.ROC.matrix, aes(x = fpr.vector, y = tpr.vector, col="Tree")) + 
  geom_line(data = tree.ROC.matrix, aes(x = fpr.vector, y = tpr.vector, col="Tree")) +
  geom_point(data = bayes.ROC.matrix, aes(x = fpr.vector, y = tpr.vector, col="Bayes")) + 
  geom_line(data = bayes.ROC.matrix, aes(x = fpr.vector, y = tpr.vector, col="Bayes")) 

loss_matrix <- matrix(data = c(0,1,10,0), nrow = 2, ncol = 2)
bayes.loss.predict <- ifelse(bayes.predict[,2]/bayes.predict[,1] > loss_matrix[3]/loss_matrix[2], "good", "bad")
table(test$good_bad, bayes.loss.predict)
