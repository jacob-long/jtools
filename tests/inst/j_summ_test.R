# Create lm object
fit <- lm(Income ~ `HS Grad` + Illiteracy + Murder, data=as.data.frame(state.x77))
# Print the output with standardized coefficients and 2 digits past the decimal
j_summ(fit, stdbeta=TRUE, digits=2)
# With VIF, robust
j_summ(fit, vifs=TRUE, robust=TRUE)
