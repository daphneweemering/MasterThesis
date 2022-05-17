# THESIS: 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 
# Daphne Weemering, 3239480, M&S for the Behavioral, Biomedical and Social Sciences,
# Utrecht University

# Load in the required packages 
library(lme4)     # For the 'lmer' function fitting the linear mixed models
library(pwr)      # To calculate the sample size 
library(lmerTest) # Provides p-values in summary tables for linear mixed models

# ------------------------------------------------------------------------------
# Make sure that the function 'fixed' in file 'Power in fixed sample size trials.R' 
# has been ran and is available in the environment. 

# With this file, the values are specified for the hypothesized value for the 
# variance of the treatment effect, hypothesized value for the variance of the 
# random error, true value for the variance of the treatment effect and the true
# value for the variance of the random error.
# ------------------------------------------------------------------------------

# Specify the values for 'x' that needs to be specified for the function 'reestim'
hvar_treatment <- c(0.5, 1, 2)
hvar_error <- c(0.25, 0.5, 1)
tvar_treatment <- c(0.5, 1, 2)
tvar_error <- c(0.25, 0.5, 1)

# Create data frame including all combinations of 'hvar_treatment', 'hvar_error',
# 'tvar_treatment' and 'tvar_error'. Name the data frame x. This is used as
# an input for the function 'fixed'
x <- expand.grid(hvar_treatment, hvar_error, tvar_treatment, tvar_error)

# Use apply to use the function 'fixed' from the file 'Power in fixed sample size
# trials.R' to all the rows of x
initial <- apply(x, 1, fixed)

# Unlist 'initial'
temp <- t(as.data.frame(lapply(initial, unlist)))

# Combine x and 'initial'
resultsfixedpower <- cbind(x, temp)

# Remove the long rownames from 'results'
rownames(resultsfixedpower) <- c()

# Change the column names for the 'initial' matrix 
names(resultsfixedpower)[1] <- 'hyp_psi'
names(resultsfixedpower)[2] <- 'hyp_sigma'
names(resultsfixedpower)[3] <- 'true_psi'
names(resultsfixedpower)[4] <- 'true_sigma'
names(resultsfixedpower)[5] <- 'power_fixed'

# Save the results in a separate file
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Raw output')
save.image(file = 'power-fixed.RData')


















