data <- data.frame(read.csv("spambase.csv"))
n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train <- data[id,]
test <- data[-id,]

# glm 
model <- glm(as.factor(Spam)~., family = binomial, data = train)
#predict
predict <- predict(model, test, "response")

# Y_hat = 1 if P(Y = 1|X) > 0.5, else Y_hat = 0
predict.condition.0.5 <- ifelse(predict > 0.5, 1, 0)

table(test$Spam, predict.condition.0.5)

# Y_hat = 1 if P(Y = 1|X) > 0.9, else Y_hat = 0
predict.condition.0.9 <- ifelse(predict > 0.9, 1, 0)

table(test$Spam, predict.condition.0.9)

# Standard KNN
kknn.response <- kknn(as.factor(Spam)~., train, test, k = 30)

