# 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 

library(lme4)
library(pwr)
library(lmerTest)

# Specify the values
hvar_treatment <- c(0.5, 1, 2)
hvar_error <- c(0.25, 0.5, 1)
tvar_treatment <- c(0.5, 1, 2)
tvar_error <- c(0.25, 0.5, 1)

# Create data frame including all possible combinations of 'hvar_treatment', 
# 'hvar_error', 'tvar_treatment', tvar_error' and 'fn'
x <- expand.grid(hvar_treatment, hvar_error, tvar_treatment, tvar_error)

# Use apply to use the function 'reestim' from the file 'Interim n-reestimation
# n-of-1 trial.R' to all the rows of x
initial <- apply(x, 1, reestim)

# Unlist 'initial'
temp <- t(as.data.frame(lapply(initial, unlist)))

# Combine x and 'initial'
resultsfixedalpha <- cbind(x, temp)

# Remove the long rownames from 'results'
rownames(resultsfixedalpha) <- c()

# Change the column names for the 'initial' matrix (for interpretability)
names(resultsfixedalpha)[1] <- 'hyp_psi'
names(resultsfixedalpha)[2] <- 'hyp_sigma'
names(resultsfixedalpha)[3] <- 'true_psi'
names(resultsfixedalpha)[4] <- 'true_sigma'
names(resultsfixedalpha)[5] <- 'power_fixed'

# Save the results in a separate file
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/data/Raw data')
save.image(file = 'power-fixed.RData')


















