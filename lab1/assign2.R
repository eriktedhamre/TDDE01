setwd("~/TDDE01/lab1")
data <- as.matrix(data.frame(read.csv("machines.csv")))
# Guessing it's an exponential distribution
plot(data)

llhood <- function(x, theta){
  return (log(prod(theta*exp(-theta*x))))
} 

theta.seq = seq(from = 0, to = 4, by = 0.001)

llhood.for.thetas <- function(theta.vector, data.vector ){
  res <- double()
  for (value in theta.vector) {
    res <- append(res, llhood(data.vector, value))
  }
  return(res)
}

thetas.all.data <- llhood.for.thetas(theta.seq, data)
theta.6.datapoints <- llhood.for.thetas(theta.seq, c(data[1:6]))
plot(theta.seq, thetas.all.data, type = 'l', xlim = range(0,4), ylim = range(-300,30 ))
lines(theta.seq, theta.6.datapoints, col = "red")
theta.seq[which.max(theta.6.datapoints)]

# 1.126 Erik probably smaller interval in theta-vector

# Bayesian

prior <- function(lambda, theta){
  return (log(theta*exp(-theta*lambda)))
}


llhood.bayesian <- function(theta.vector, data.vector ){
  res <- double()
  for (value in theta.vector) {
    res <- append(res, (llhood(data.vector, value) + prior(10, value)))
  }
  return(res)
}

thetas.bayesian <- llhood.bayesian(theta.seq, data)
theta.seq[which.max(thetas.bayesian)]
lines(theta.seq, thetas.bayesian, col = "blue")


# Lambda 0.931

generated.data = rexp(50, rate = 1.126)
hist(data)
hist(generated.data)

