#Clear Environment
remove(list = ls())

library(ggfortify)
###Question 2

set.seed(1234)

##Linear Regression
#Part A: Generate the independent and dependent variable (x,y)
x<-rnorm(1000, 5, 10)
y<-x+rnorm(1000) + 5

#Part B: Create the model
m1<-lm(y~x)
summary(m1)

#plot data and model
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
abline(m1, col='blue')

#Part C: Build cost and gradient descent function 
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}
# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000
# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)
# initialize coefficients
theta <- matrix(c(0,0), nrow=2)
# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))
# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

#plot cost function
plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')