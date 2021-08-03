library(kernlab)
eps = 1e-8
n = 30

space = seq(-2,2,length.out=n)
time = space

K1 = kernelMatrix(rbfdot(4), as.matrix(space))
K2 = kernelMatrix(rbfdot(1), as.matrix(time))

L1 = chol(K1 + eps * diag(n))
L2 = chol(K2 + eps * diag(n))

v = rnorm(n*n)

y = as.numeric(matrix(t(t(L2) %*% matrix(t(t(L1) %*% matrix(v,n,n)),n,n)),n*n,1))
y = y + rnorm(n*n,sd=.1) # Add spherical noise

y = matrix(as.numeric(y), ncol = n, byrow = FALSE) ## NOT in original R code, but the Stan code expects an n2 x n1 matrix, as noted in the Stan data block.

data = list(n1=length(space),n2=length(time), x1=space, x2=time, y=y)
