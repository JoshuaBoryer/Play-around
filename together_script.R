# (Question 1)
#Linear CRNG

rm(list=ls())
set.seed(41497475)

#Congruential Number Generation Function
congruential <- function(n,a,c,m,seed){ 
  rvec <- c(seed)
  xim1 <- seed
  for (i in 1:n){
    xi <- (a * xim1 + c) %% m
    rvec <- c(rvec, xi)
    xim1 <- xi
  }
  return(rvec)
}

#Generating numbers Congruently and scaling them to be between 0 and 1
my_nums <- congruential(10000, 1103515245, 12345, 2^26, 41497475) / 2^26

#Generated numbers showing approximate uniform distribution
hist(my_nums,
     main = "Congruential Number Generator", 
     xlab = "Value scaled between 0 and 1")

plot(my_nums[1:10000], my_nums[2:10001])

#-------------------------------------------------------------------------------

#Question 2b)

#Inverse sampling function using Pareto Distribution
inverse_pareto_sample <- function(n, a, b){
  u <- my_nums[1:n]
  x_vals <- a / ((1 - u)^(1/b))
  return(x_vals)
}

#Generating values using a = 4, b = 5
pareto_values <- inverse_pareto_sample(1000, 4, 5)

#2c)

#Plotting the generated pareto distribution
hist(pareto_values, prob=TRUE) 

#Creating line 
x_vals <- seq(4, max(pareto_values), length.out = 200)
pdf_vals <- (5 * 4^5) / (x_vals^(5 + 1))
lines(x_vals, pdf_vals, col = "red", lwd = 2) 

#-------------------------------------------------------------------------------

#3a)

n <- 1000
u <- my_nums[1:n]
x <- numeric(n)
prob_vec <- c(0.1, 0.1, 0.3, 0.2, 0.2, 0.1)

for (i in 1:n) {
  j <- 1
  x[i] <- 1
  f <- prob_vec[j]
  while (u[i] > f) {
    j <- j + 1
    f <- f + prob_vec[j]
    x[i] <- x[i] + 1
  }
}

#3b)

hist(x,
     breaks = seq(0.5, 6.5, 1),
     probability = TRUE,
     col = "lightblue",
     xlab = "x",
     ylab = "Probability",
     main = "Sampled vs True PMF")      

points(1:6, prob_vec, pch = 16, col = "red")  
lines(1:6, prob_vec, col = "red")
legend("topright", legend = "True PMF", col = "red", pch = 16)

#-------------------------------------------------------------------------------

#4a)

trials = 10
p = 0.7
n = 1000

u <- my_nums[1:n]

prob_vec <- numeric(trials + 1) 
for (k in 0:trials) {
  prob_vec[k + 1] <- choose(trials, k) * p^k * (1 - p)^(trials - k)
}

x <- numeric(n)

for (i in 1:n) {
  j <- 1
  x[i] <- 0
  f <- prob_vec[j]
  while (u[i] > f) {
    j <- j + 1
    f <- f + prob_vec[j]
    x[i] <- x[i] + 1
  }
}

#4b)

hist(x, breaks = seq(-0.5, trials + 0.5, 1), freq = FALSE,
     main = "Simulated Binomial with True PMF", xlab = "x", ylim = c(0, max(true_pmf)))
true_pmf <- dbinom(0:trials, trials, p)
lines(0:trials, true_pmf, type = "b", col = "red", pch = 19)

#-------------------------------------------------------------------------------

#5a)


