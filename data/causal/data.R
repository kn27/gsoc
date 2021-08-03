# Collect data into a list format suitable for Stan

# Use example dataset form Matching package
data(lalonde, package = "Matching")

# Change the scales of all earnings variables in thousands
lalonde$re74 <- lalonde$re74/1000
lalonde$re75 <- lalonde$re75/1000
lalonde$re78 <- lalonde$re78/1000

# Prepare outcome and treatment
y <- lalonde$re78
w <- lalonde$treat

# Display summary statistics
sumstats <- lalonde %>%
	summarise_all(funs(mean, sd, min, max)) %>%
	gather(key, value, everything()) %>%
	separate(key, into = c("variable", "stat"), sep = "_") %>%
	spread(stat, value) %>%
	dplyr::select(variable, mean, sd, min, max) %>%
	mutate_each(funs(round(., 1)), -variable)
knitr::kable(sumstats, caption = "Summary statistics for the NSW data")

data <- list(N = nrow(lalonde), y = y, w = w)
