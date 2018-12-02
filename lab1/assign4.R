setwd("~/TDDE01/lab1")
data <- data.frame(read.csv("tecator.csv"))
#Very nice, very linear, thanks SIR
set.seed(12345)
n <- dim(data)[1]
id <- sample(1:n, floor(n*0.5))
train <- data[id,]
test <- data[-id,]
plot(data$Moisture, data$Protein, xlab = "Moisture", ylab = "Protein")


mse <- function(actual, predicted) {return (mean((actual$Moisture - predicted)^2))}

create.mse.vector <-function(data.all, data.test, data.train, exponentials){
  result <- double()
  for ( exponent in exponentials) {
    fit <- lm(Moisture~ poly(Protein, exponent), data.train)
    pred.train <- predict(fit, data.train)
    pred.test <- predict(fit, data.test)
    result <- append(result, c(mse(data.train, pred.train), mse(data.test, pred.test)))
  }
  return(result)
}

i.vector <- seq(1, 6, 1)
mse.vector <- create.mse.vector(data.all = data, data.test = test, data.train = train, exponentials = i.vector)
mse.train <- mse.vector[c(TRUE, FALSE)]
mse.test <- mse.vector[c(FALSE, TRUE)]
ggplot() + 
  geom_line(aes(x = seq(1, 6, 1), y = mse.train), color = "red") +
  geom_line(aes(x = seq(1, 6, 1), y = mse.test), color = "blue") +
  labs() +
  xlab('Polynomial degree') +
  ylab('MSE')
## 4.4
data.fat <- subset(data, select = -c(Sample, Protein, Moisture))
set.seed(12345)
full.model <- lm(Fat ~., data = data.fat)
step.model <- stepAIC(full.model, direction = "backward", trace = FALSE)
coef(step.model)

#Selected all of them??

## 4.5
data.fat <- subset(data, select = -c(Sample, Protein, Moisture))
set.seed(12345)
covariates <- scale(data.fat[,1:100])
response <- scale(data.fat[,101])

model.rr <- glmnet(as.matrix(covariates), response, alpha = 0)
plot(model.rr, xvar="lambda", label=TRUE)

## 4.6
data.fat <- subset(data, select = -c(Sample, Protein, Moisture))
set.seed(12345)
covariates <- scale(data[,1:100])
response <- scale(data.fat[,101])
model.lasso <- glmnet(as.matrix(covariates), response , alpha = 1)
plot(model.lasso, xvar="lambda")

## 4.7
data.fat <- subset(data, select = -c(Sample, Protein, Moisture))
set.seed(12345)
covariates <- scale(data.fat[,1:100])
response <- scale(data.fat[,101])
cv.lasso <- cv.glmnet(as.matrix(covariates), response , alpha = 1, lambda = seq(0, 1, 0.001))
plot(cv.lasso)
cv.lasso$lambda
(best.lambda <- cv.lasso$lambda.min)

