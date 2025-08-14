#--------------------------------------------------

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

