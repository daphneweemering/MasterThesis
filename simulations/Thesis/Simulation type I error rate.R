#### Simulations for estimating type I error rate for series of N-of-1 trials with 
#### sample size reestimation

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
resultsalpha <- cbind(x, temp)

# Remove the long rownames from 'results'
rownames(resultsalpha) <- c()

# Change the column names for the 'initial' matrix (for interpretability)
names(resultsalpha)[1] <- 'hyp_psi'
names(resultsalpha)[2] <- 'hyp_sigma'
names(resultsalpha)[3] <- 'true_psi'
names(resultsalpha)[4] <- 'true_sigma'
names(resultsalpha)[5] <- 'fraction'
names(resultsalpha)[6] <- 'alpha'
names(resultsalpha)[7] <- 'initialsampsize'
names(resultsalpha)[8] <- 'f_initsampsize'
names(resultsalpha)[9] <- 'reestimsampsize'
names(resultsalpha)[10] <- 'var_reestimsampsize'
names(resultsalpha)[11] <- 'sd_reestimsampsize'

# Save the results in a separate file
setwd('//Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/data/Raw data')
save.image(file = 'type1errorrate.RData')


