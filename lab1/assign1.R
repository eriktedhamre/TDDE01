setwd("~/TDDE01/lab1")
data <- data.frame(read.csv("spambase.csv"))
n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train <- data[id,]
test <- data[-id,]

# glm 
model <- glm(as.factor(Spam)~., family = binomial, data = train)
# predict
predict <- predict(model, test, "response")

# Y_hat = 1 if P(Y = 1|X) > 0.5, else Y_hat = 0
predict.condition.0.5 <- ifelse(predict > 0.5, 1, 0)
# Confusion matrix
table(test$Spam, predict.condition.0.5)

#   0   1
# 0 791 146
# 1  97 336

# Y_hat = 1 if P(Y = 1|X) > 0.9, else Y_hat = 0
predict.condition.0.9 <- ifelse(predict > 0.9, 1, 0)
# Confusion matrix
table(test$Spam, predict.condition.0.9)

#   0   1
# 0 936   1
# 1 427   6

# KKNN with K = 30
kknn.response.K30 <- kknn(as.factor(Spam)~., train, test, k = 30)
# Confusion matrix
table(test$Spam, kknn.response.K30$fitted.values)
table(train$Spam, kknn.response.K30$fitted.values)
#  Test
#    0   1
# 0 672 265
# 1 187 246

# KKNN with K = 1
kknn.response.K1 <- kknn(as.factor(Spam)~., train, test, k = 1)
# Confusion matrix
table(test$Spam, kknn.response.K1$fitted.values)

#    0   1
# 0 640 297
# 1 177 256
#
# Doesn't really seem to be that big of a difference,
# maybe considering a large amount of neighbors doesn't
# work well for our data since it isn't easily transformed to
# a geometric interpretation.
