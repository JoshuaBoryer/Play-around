#-------------------------------------------------------------------------------

#3a)
#Rejection sampler script

inversion_discrete <- function(n, tarpmf, proppmf, scale, my_nums) {
  samples <- numeric(0)
  index <- 1
  
  while (length(samples) < n) {
    # Check index not out of bound
    if (index > length(my_nums)) {
      stop("Ran out of random numbers in my_nums :0")
    }
    
    # Proposal from my_nums
    u_prop <- my_nums[index]; index <- index + 1
    x_prop <- which(cumsum(proppmf) >= u_prop)[1]
    
    #ran into problems with missing values
    if (is.na(x_prop)) {
      next  
    }
    
    #check proppmf[x_prop] not zero
    denom <- scale * proppmf[x_prop]
    if (denom == 0) {
      next
    }
    
    #acceptance check
    if (index > length(my_nums)) {
      stop("Ran out of random numbers in my_nums")
    }
    u_acc <- my_nums[index]; index <- index + 1
    
    #check for NA 
    if (is.na(u_acc) || is.na(tarpmf[x_prop]) || is.na(denom)) {
      next
    }
    
    if (u_acc <= tarpmf[x_prop] / denom) {
      samples <- c(samples, x_prop)
    }
  }
  
  return(samples)
}

#Target
p <- c(0.1, 0.1, 0.3, 0.2, 0.2, 0.1)
#Proposed
q <- rep(1/6, 6)
#Scalar
m <- max(p / q)
my_nums <- congruential(1000, 5, 3, 2^35, 41497475) / 2^35
samples <- inversion_discrete(1000, p, q, m, my_nums)

#3b)

#-------------------------------------------------------------------------------
