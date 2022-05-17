# THESIS: 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 
# Daphne Weemering, 3239480, M&S for the Behavioral, Biomedical and Social Sciences,
# Utrecht University

# Load in the required packages 
library(lme4)     # For the 'lmer' function fitting the linear mixed models
library(pwr)      # To calculate the sample size 
library(lmerTest) # Provides p-values in summary tables for linear mixed models

# ------------------------------------------------------------------------------
# Make sure that the function 'reestim' in file 'Interim n-reestimation n-of-1 
# trial.R' has been ran and is available in the environment. 

# With this file, the values are specified for the hypothesized value for the 
# variance of the treatment effect, hypothesized value for the variance of the 
# random error, true value for the variance of the treatment effect, true value 
# for the variance of the random error and for the fraction of the initial sample 
# size. 

# To obtain the power, make sure that 'avg_treatment_data' of the 'reestim' function
# in the file 'Interim n-reestimation n-of-1 trial.R' is set to 1. 
# ------------------------------------------------------------------------------

# Specify the values for 'x' that needs to be specified for the function 'reestim'
hvar_treatment <- c(0.5, 1, 2)
hvar_error <- c(0.25, 0.5, 1)
tvar_treatment <- c(0.5, 1, 2)
tvar_error <- c(0.25, 0.5, 1)
fn <- c(0.25, 0.5, 0.75)

# Create data frame including all combinations of 'hvar_treatment', 'hvar_error',
# 'tvar_treatment', tvar_error' and 'fn'. Name the data frame x. This is used as
# an input for the function 'reestim'
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

# Change the column names for the 'initial' matrix 
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
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Raw output')
save.image(file = 'power-reestimation.RData')












