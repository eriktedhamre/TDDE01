setwd("~/TDDE01/lab1")
data <- as.matrix(data.frame(read.csv("machines.csv")))
# Guessing it's an exponential distribution
plot(data)

llhood <- function(x, theta){
  return(sum(log(theta*exp(-theta*x)))/length(x))
} 

theta.seq = seq(from = 0.5, to = 1.5, by = 0.01)

llhood.for.thetas <- function(theta.vector, data.vector ){
  res <- double()
  for (value in theta.vector) {
    res <- append(res, llhood(data.vector, value))
  }
  return(res)
}

thetas.all.data <- llhood.for.thetas(theta.seq, data)
theta.6.datapoints <- llhood.for.thetas(theta.seq, c(data[1:6]))
plot(theta.seq, thetas.all.data, xlim = range(0.5,1.5), ylim = range(-2,1 ))
lines(theta.seq, theta.6.datapoints)
theta.seq[which.max(thetas)]

# 1.13 Erik probably smaller interval in theta-vector