source('./scotland_lip_cancer.RData', chdir = T)
W <- A # adjacency matrix
scaled_x <- c(scale(x))
X <- model.matrix(~scaled_x)

data <- list(n = nrow(X),         # number of observations
			   p = ncol(X),         # number of coefficients
			   X = X,               # design matrix
			   y = O,               # observed number of cases
			   log_offset = log(E), # log(expected) num. cases
			   W = W)               # adjacency matrix

