# (Question 1)
#Linear CRNG

rm(list=ls())

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

#----------------------------------------------------




