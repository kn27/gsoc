#install.packages("TAM")
library(TAM)
library(edstan)

# Attach the example dataset. The TAM package is required.
data(data.timssAusTwn.scored, package = "TAM")

# Subset the full data
select <- floor(seq(from = 1, to = nrow(data.timssAusTwn.scored),
					length.out = 500))
subsetted_df <- data.timssAusTwn.scored[select, ]
str(subsetted_df)

# Make a matrix of person predictors
w_mat <- cbind(intercept = rep(1, times = nrow(subsetted_df)),
			   taiwan = as.numeric(subsetted_df$IDCNTRY == 158),
			   female = as.numeric(subsetted_df$ITSEX == 2),
			   book14 = as.numeric(subsetted_df$IDBOOK == 14))
head(w_mat)

# Make a matrix of item responses
y_mat <- as.matrix(subsetted_df[, grep("^M", names(subsetted_df))])
head(y_mat)

# Maximum score for each item
apply(y_mat, 2, max)

# Assemble data list for Stan
data <- irt_data(response_matrix = y_mat, covariates = as.data.frame(w_mat),
					formula = ~ taiwan*female + book14)
