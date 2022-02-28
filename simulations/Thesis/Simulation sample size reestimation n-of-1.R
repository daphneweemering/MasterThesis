# 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 

library(lme4)
library(pwr)
library(lmerTest)

# Specify the values
hvar_treatment <- c(0.5, 1, 2)
hvar_error <- c(0.25, 0.5, 1)
tvar_treatment <- c(0.5, 1, 2)
tvar_error <- c(0.25, 0.5, 1)
fn <- c(0.25, 0.5, 0.75)

# Create data frame including all possible combinations of 'hvar_treatment', 
# 'hvar_error', 'tvar_treatment', tvar_error' and 'fn'
x <- expand.grid(hvar_treatment, hvar_error, tvar_treatment, tvar_error, fn)

x1 <- x[-c(1,2,3,4,5,6,7,8,9,10),]

# Use apply to use the function 'reestim' from the file 'Interim n-reestimation
# n-of-1 trial.R' to all the rows of x
initial <- apply(x, 1, reestim)
 
# Make 'initial' a matrix so that it can be combined with x
initial <- as.matrix(initial, round = 2)

# Combine x and 'initial'
results <- cbind(x, initial)














