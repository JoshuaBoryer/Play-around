# How many sample points
n=1000
# Simulate over support of distribution
X=runif(n,0,1)
# Simulate from bounding density in y direction
U=runif(n,0,16/9)
# Accept if point is under density U<f(X)
densX=dbeta(X,3,2) #f(X)
accept=(U<densX)
# Extract those that are accepted
beta.sample=X[accept]

