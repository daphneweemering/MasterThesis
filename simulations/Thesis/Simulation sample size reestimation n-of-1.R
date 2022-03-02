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

# Use apply to use the function 'reestim' from the file 'Interim n-reestimation
# n-of-1 trial.R' to all the rows of x
initial <- apply(x, 1, reestim)  

# Unlist 'initial'
temp <- t(as.data.frame(lapply(initial, unlist)))

# Combine x and 'initial'
results <- cbind(x, temp)

# Remove the long rownames from 'results'
rownames(results) <- c()

# Change the column names for the 'initial' matrix (for interpretability)
names(results)[1] <- 'hyp_psi'
names(results)[2] <- 'hyp_sigma'
names(results)[3] <- 'true_psi'
names(results)[4] <- 'true_sigma'
names(results)[5] <- 'fraction'
names(results)[6] <- 'power'
names(results)[7] <- 'initsampsize'
names(results)[8] <- 'f_initsampsize'
names(results)[9] <- 'reestimsampsize'
names(results)[10] <- 'final_minus_initial'
















