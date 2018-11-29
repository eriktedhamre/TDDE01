setwd("~/TDDE01/lab2")
data <- data.frame(read.csv("australian-crabs.csv"))
plot(data$CL, data$RW , col = ifelse(data$sex == "Male", "blue", "red"))
# Yes, it seems to be two pretty distinct data sets, perhaps the beginning could be tricky
lda.model <- lda(sex ~ CL + RW, data = data)
lda.predict <- predict(lda.model, data)
plot(data$CL, data$RW, col = ifelse(lda.predict$x == "Male", "blue", "red"))

