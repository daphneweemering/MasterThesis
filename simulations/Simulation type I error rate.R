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

# To obtain the type I error rate, make sure that 'avg_treatment_data' of the 
# 'reestim' function in the file 'Interim n-reestimation n-of-1 trial.R' is set to 
# 0. 
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
resultsalpha <- cbind(x, temp)

# Remove the long rownames from 'results'
rownames(resultsalpha) <- c()

# Change the column names for the 'initial' matrix 
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
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Raw output')
save.image(file = 'type1errorrate.RData')


