#### Simulations for estimating  power for series of N-of-1 trials with sample
#### size reestimation

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
resultspower <- cbind(x, temp)
results <- cbind(x, temp)

# Remove the long rownames from 'results'
rownames(resultspower) <- c()

# Change the column names for the 'initial' matrix (for interpretability)
names(resultspower)[1] <- 'hyp_psi'
names(resultspower)[2] <- 'hyp_sigma'
names(resultspower)[3] <- 'true_psi'
names(resultspower)[4] <- 'true_sigma'
names(resultspower)[5] <- 'fraction'
names(resultspower)[6] <- 'power'
names(resultspower)[7] <- 'initialsampsize'
names(resultspower)[8] <- 'f_initsampsize'
names(resultspower)[9] <- 'reestimsampsize'
names(resultspower)[10] <- 'var_reestimsampsize'
names(resultspower)[11] <- 'sd_reestimsampsize'
names(resultspower)[12] <- 'median_reestimsampsize'
names(resultspower)[13] <- 'min_reestimsampsize'
names(resultspower)[14] <- 'max_reestimsampsize'

# Save the results in a separate file
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/data/Raw data')
save.image(file = 'power-reestimation.RData')












